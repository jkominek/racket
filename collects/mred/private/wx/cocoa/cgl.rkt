#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../../lock.rkt"
         racket/draw/unsafe/cairo
         racket/draw/private/local
         racket/draw/private/gl-context
         racket/draw/private/gl-config
         racket/draw/private/bitmap)

(provide (protect-out create-gl-bitmap
                      CGLChoosePixelFormat
                      CGLCreateContext
                      CGLPFADoubleBuffer
                      CGLPFAStereo
                      CGLPFAColorSize
                      CGLPFAAlphaSize
                      CGLPFADepthSize
                      CGLPFAStencilSize
                      CGLPFAAccumSize
                      CGLPFAOffScreen
                      CGLPFASampleBuffers
                      CGLPFASamples
                      CGLPFAMultisample))


(define cgl-lib 
  (ffi-lib "/System/Library/Frameworks/OpenGL.framework/OpenGL"))

(define-ffi-definer define-cgl cgl-lib)

(define _GLsizei _int)
(define _GLint _int)
(define _GLboolean _bool)
(define _CGLPixelFormat (_cpointer/null 'CGLPixelFormat))
(define _CGLContext (_cpointer/null 'CGLContext))
; see Frameworks/OpenGL.framework/headers/CGLTypes.h
(define _CGLError (_enum '(CGLNoError            = 0
                           CGLBadAttribute       = 10000
                           CGLBadProperty        = 10001
                           CGLBadPixelFormat     = 10002
                           CGLBadRendererInfo    = 10003
                           CGLBadContext         = 10004
                           CGLBadDrawable        = 10005
                           CGLBadDisplay         = 10006
                           CGLBadState           = 10007
                           CGLBadValue           = 10008
                           CGLBadMatch           = 10009
                           CGLBadEnumeration     = 10010
                           CGLBadOffScreen       = 10011
                           CGLBadFullScreen      = 10012
                           CGLBadWindow          = 10013
                           CGLBadAddress         = 10014
                           CGLBadCodeModule      = 10015
                           CGLBadAlloc           = 10016
                           CGLBadConnection      = 10017)))

(define (cglerror-handler [finalizer void])
  (lambda (f)
    (lambda x
      (let-values ([(err v) (apply f x)])
                  (if (eq? 'CGLNoError err)
                      (begin
                        (register-finalizer v finalizer)
                        v)
                      (error (object-name f)
                             "CGL error: ~a" err))))))

(define-cgl CGLReleasePixelFormat (_fun _CGLPixelFormat -> _void))
(define-cgl CGLChoosePixelFormat (_fun (_list i _int) (pf : (_ptr o _CGLPixelFormat)) (_ptr o _GLint)
                                    -> (err : _CGLError)
                                    -> (values err pf))
  #:wrap (cglerror-handler CGLReleasePixelFormat))
(define-cgl CGLReleaseContext (_fun _CGLContext -> _void))
(define-cgl CGLCreateContext (_fun _CGLPixelFormat _CGLContext (ctx : (_ptr o _CGLContext))
                                -> (err : _CGLError)
                                -> (values err ctx))
  #:wrap (cglerror-handler CGLReleaseContext))

; This function (which is key to the way our GL bitmaps work) is deprecated
; in OSX 10.7. The correct thing to do is set up a framebuffer object, with
; all of the appropriate attachments, and render into it.
(define-cgl CGLSetOffScreen (_fun _CGLContext _GLsizei _GLsizei _GLint _pointer
                               -> (err : _CGLError)
                               -> (values err (void)))
  #:wrap (cglerror-handler))

(define-cgl CGLSetCurrentContext (_fun _CGLContext
                                    -> (err : _CGLError)
                                    -> (values err (void)))
  #:wrap (cglerror-handler))

(define CGLPFAAllRenderers 1)   ; choose from all available renderers 
(define CGLPFADoubleBuffer 5)   ; choose a double buffered pixel format 
(define CGLPFAStereo 6)         ; stereo buffering supported 
(define CGLPFAAuxBuffers 7)     ; number of aux buffers 
(define CGLPFAColorSize 8)      ; number of color buffer bits 
(define CGLPFAAlphaSize 11)     ; number of alpha component bits 
(define CGLPFADepthSize 12)     ; number of depth buffer bits 
(define CGLPFAStencilSize 13)   ; number of stencil buffer bits 
(define CGLPFAAccumSize 14)     ; number of accum buffer bits 
(define CGLPFAMinimumPolicy 51) ; never choose smaller buffers than requested 
(define CGLPFAMaximumPolicy 52) ; choose largest buffers of type requested 
(define CGLPFAOffScreen 53)     ; choose an off-screen capable renderer 
(define CGLPFAFullScreen 54)    ; choose a full-screen capable renderer 
(define CGLPFASampleBuffers 55) ; number of multi sample buffers 
(define CGLPFASamples 56)       ; number of samples per multi sample buffer 
(define CGLPFAAuxDepthStencil 57) ; each aux buffer has its own depth stencil 
(define CGLPFAColorFloat 58)    ; color buffers store floating point pixels 
(define CGLPFAMultisample 59)   ; choose multisampling 
(define CGLPFASupersample 60)   ; choose supersampling 
(define CGLPFASampleAlpha 61)   ; request alpha filtering 
(define CGLPFARendererID 70)    ; request renderer by ID 
(define CGLPFASingleRenderer 71)  ; choose a single renderer for all screens 
(define CGLPFANoRecovery 72)    ; disable all failure recovery systems 
(define CGLPFAAccelerated 73)   ; choose a hardware accelerated renderer 
(define CGLPFAClosestPolicy 74) ; choose the closest color buffer to request 
(define CGLPFABackingStore 76)  ; back buffer contents are valid after swap 
(define CGLPFAWindow 80)        ; can be used to render to an onscreen window 
(define CGLPFACompliant 83)     ; renderer is opengl compliant 
(define CGLPFADisplayMask 84)   ; mask limiting supported displays 
(define CGLPFAPBuffer 90)       ; can be used to render to a pbuffer 
(define CGLPFARemotePBuffer 91) ; can be used to render offline to a pbuffer 
(define CGLPFAAllowOfflineRenderers 96) ; show offline renderers in pixel formats 
(define CGLPFAAcceleratedCompute 97)    ; choose a hardware accelerated compute device 
(define CGLPFAVirtualScreenCount 128)   ; number of virtual screens in this format 

(define dummy-cgl #f)
(define current-cgl #f)

(define cgl-context%
  (let ([orig-gl-context% gl-context%])
    (define gl-context%
      (class orig-gl-context%
        (init-field cgl)

        (define/override (get-handle)
          cgl)
        
        (define/override (do-call-as-current t)
          (dynamic-wind
              (lambda () 
                (atomically
                 (CGLSetCurrentContext cgl)
                 (set! current-cgl cgl)))
              t
              (lambda () 
                (atomically
                 (CGLSetCurrentContext dummy-cgl)
                 (set! current-cgl #f)))))

        (define/override (do-swap-buffers)
          (void))

        (super-new)))
    gl-context%))
    

(define cgl-bitmap%
  (let ([orig-bitmap% bitmap%])
    (define bitmap%
      (class orig-bitmap%
        (init cgl)
        (super-new)

        (define ctx (make-object cgl-context% cgl))

        (define/override (get-bitmap-gl-context)
          ctx)

        (define/override (release-bitmap-storage)
          (set! ctx #f)
          (super release-bitmap-storage))))
    bitmap%))

(define (create-gl-bitmap w h conf)
  (let* ([share-context (send conf get-share-context)]
         [context-handle (if share-context (send share-context get-handle) #f)]
         [fmt (CGLChoosePixelFormat
              (append
               (list CGLPFAColorSize 24
                     CGLPFAAlphaSize 8
                     CGLPFAOffScreen)
               (if (send conf get-stereo) (list CGLPFAStereo) null)
               (list
                CGLPFADepthSize (send conf get-depth-size)
                CGLPFAStencilSize (send conf get-stencil-size)
                CGLPFAAccumSize (send conf get-accum-size))
               (let ([ms (send conf get-multisample-size)])
                 (if (zero? ms)
                     null
                     (list CGLPFAMultisample
                           CGLPFASampleBuffers 1
                           CGLPFASamples ms)))
               (list 0)))])
    (and fmt
         (let ([cgl (CGLCreateContext fmt context-handle)]
               [d-cgl (or dummy-cgl
                          (let ([d (CGLCreateContext fmt context-handle)])
                            (when d
                              (set! dummy-cgl d)
                              d)))])
           (and cgl
                d-cgl
                (let ([bm (make-object cgl-bitmap% cgl w h #f #t)])
                  (and (send bm ok?)
                       (let ([s (send bm get-cairo-surface)])
                         (and (CGLSetOffScreen cgl w h
                                               (cairo_image_surface_get_stride s)
                                               (cairo_image_surface_get_data s))
                              bm)))))))))

