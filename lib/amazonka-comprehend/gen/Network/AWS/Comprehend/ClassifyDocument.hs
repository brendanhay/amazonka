{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.ClassifyDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classification request to analyze a single document in real-time, using a previously created and trained custom model and an endpoint.
module Network.AWS.Comprehend.ClassifyDocument
    (
    -- * Creating a request
      ClassifyDocument (..)
    , mkClassifyDocument
    -- ** Request lenses
    , cdText
    , cdEndpointArn

    -- * Destructuring the response
    , ClassifyDocumentResponse (..)
    , mkClassifyDocumentResponse
    -- ** Response lenses
    , cdrrsClasses
    , cdrrsLabels
    , cdrrsResponseStatus
    ) where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkClassifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { text :: Types.CustomerInputString
    -- ^ The document text to be analyzed.
  , endpointArn :: Types.EndpointArn
    -- ^ The Amazon Resource Number (ARN) of the endpoint.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClassifyDocument' value with any optional fields omitted.
mkClassifyDocument
    :: Types.CustomerInputString -- ^ 'text'
    -> Types.EndpointArn -- ^ 'endpointArn'
    -> ClassifyDocument
mkClassifyDocument text endpointArn
  = ClassifyDocument'{text, endpointArn}

-- | The document text to be analyzed.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdText :: Lens.Lens' ClassifyDocument Types.CustomerInputString
cdText = Lens.field @"text"
{-# INLINEABLE cdText #-}
{-# DEPRECATED text "Use generic-lens or generic-optics with 'text' instead"  #-}

-- | The Amazon Resource Number (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEndpointArn :: Lens.Lens' ClassifyDocument Types.EndpointArn
cdEndpointArn = Lens.field @"endpointArn"
{-# INLINEABLE cdEndpointArn #-}
{-# DEPRECATED endpointArn "Use generic-lens or generic-optics with 'endpointArn' instead"  #-}

instance Core.ToQuery ClassifyDocument where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ClassifyDocument where
        toHeaders ClassifyDocument{..}
          = Core.pure
              ("X-Amz-Target", "Comprehend_20171127.ClassifyDocument")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ClassifyDocument where
        toJSON ClassifyDocument{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Text" Core..= text),
                  Core.Just ("EndpointArn" Core..= endpointArn)])

instance Core.AWSRequest ClassifyDocument where
        type Rs ClassifyDocument = ClassifyDocumentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ClassifyDocumentResponse' Core.<$>
                   (x Core..:? "Classes") Core.<*> x Core..:? "Labels" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkClassifyDocumentResponse' smart constructor.
data ClassifyDocumentResponse = ClassifyDocumentResponse'
  { classes :: Core.Maybe [Types.DocumentClass]
    -- ^ The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time. 
  , labels :: Core.Maybe [Types.DocumentLabel]
    -- ^ The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClassifyDocumentResponse' value with any optional fields omitted.
mkClassifyDocumentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ClassifyDocumentResponse
mkClassifyDocumentResponse responseStatus
  = ClassifyDocumentResponse'{classes = Core.Nothing,
                              labels = Core.Nothing, responseStatus}

-- | The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time. 
--
-- /Note:/ Consider using 'classes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsClasses :: Lens.Lens' ClassifyDocumentResponse (Core.Maybe [Types.DocumentClass])
cdrrsClasses = Lens.field @"classes"
{-# INLINEABLE cdrrsClasses #-}
{-# DEPRECATED classes "Use generic-lens or generic-optics with 'classes' instead"  #-}

-- | The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time. 
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsLabels :: Lens.Lens' ClassifyDocumentResponse (Core.Maybe [Types.DocumentLabel])
cdrrsLabels = Lens.field @"labels"
{-# INLINEABLE cdrrsLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' ClassifyDocumentResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
