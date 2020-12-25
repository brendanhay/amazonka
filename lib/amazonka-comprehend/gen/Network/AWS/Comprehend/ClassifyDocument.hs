{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ClassifyDocument (..),
    mkClassifyDocument,

    -- ** Request lenses
    cdText,
    cdEndpointArn,

    -- * Destructuring the response
    ClassifyDocumentResponse (..),
    mkClassifyDocumentResponse,

    -- ** Response lenses
    cdrrsClasses,
    cdrrsLabels,
    cdrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkClassifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { -- | The document text to be analyzed.
    text :: Types.CustomerInputString,
    -- | The Amazon Resource Number (ARN) of the endpoint.
    endpointArn :: Types.EndpointArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClassifyDocument' value with any optional fields omitted.
mkClassifyDocument ::
  -- | 'text'
  Types.CustomerInputString ->
  -- | 'endpointArn'
  Types.EndpointArn ->
  ClassifyDocument
mkClassifyDocument text endpointArn =
  ClassifyDocument' {text, endpointArn}

-- | The document text to be analyzed.
--
-- /Note:/ Consider using 'text' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdText :: Lens.Lens' ClassifyDocument Types.CustomerInputString
cdText = Lens.field @"text"
{-# DEPRECATED cdText "Use generic-lens or generic-optics with 'text' instead." #-}

-- | The Amazon Resource Number (ARN) of the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdEndpointArn :: Lens.Lens' ClassifyDocument Types.EndpointArn
cdEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED cdEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

instance Core.FromJSON ClassifyDocument where
  toJSON ClassifyDocument {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Text" Core..= text),
            Core.Just ("EndpointArn" Core..= endpointArn)
          ]
      )

instance Core.AWSRequest ClassifyDocument where
  type Rs ClassifyDocument = ClassifyDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "Comprehend_20171127.ClassifyDocument")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ClassifyDocumentResponse'
            Core.<$> (x Core..:? "Classes")
            Core.<*> (x Core..:? "Labels")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkClassifyDocumentResponse' smart constructor.
data ClassifyDocumentResponse = ClassifyDocumentResponse'
  { -- | The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time.
    classes :: Core.Maybe [Types.DocumentClass],
    -- | The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time.
    labels :: Core.Maybe [Types.DocumentLabel],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClassifyDocumentResponse' value with any optional fields omitted.
mkClassifyDocumentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ClassifyDocumentResponse
mkClassifyDocumentResponse responseStatus =
  ClassifyDocumentResponse'
    { classes = Core.Nothing,
      labels = Core.Nothing,
      responseStatus
    }

-- | The classes used by the document being analyzed. These are used for multi-class trained models. Individual classes are mutually exclusive and each document is expected to have only a single class assigned to it. For example, an animal can be a dog or a cat, but not both at the same time.
--
-- /Note:/ Consider using 'classes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsClasses :: Lens.Lens' ClassifyDocumentResponse (Core.Maybe [Types.DocumentClass])
cdrrsClasses = Lens.field @"classes"
{-# DEPRECATED cdrrsClasses "Use generic-lens or generic-optics with 'classes' instead." #-}

-- | The labels used the document being analyzed. These are used for multi-label trained models. Individual labels represent different categories that are related in some manner and are not mutually exclusive. For example, a movie can be just an action movie, or it can be an action movie, a science fiction movie, and a comedy, all at the same time.
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsLabels :: Lens.Lens' ClassifyDocumentResponse (Core.Maybe [Types.DocumentLabel])
cdrrsLabels = Lens.field @"labels"
{-# DEPRECATED cdrrsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrrsResponseStatus :: Lens.Lens' ClassifyDocumentResponse Core.Int
cdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
