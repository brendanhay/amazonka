{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.DeleteDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a previously created document classifier
--
-- Only those classifiers that are in terminated states (IN_ERROR, TRAINED) will be deleted. If an active inference job is using the model, a @ResourceInUseException@ will be returned.
-- This is an asynchronous action that puts the classifier into a DELETING state, and it is then removed by a background job. Once removed, the classifier disappears from your account and is no longer available for use.
module Network.AWS.Comprehend.DeleteDocumentClassifier
  ( -- * Creating a request
    DeleteDocumentClassifier (..),
    mkDeleteDocumentClassifier,

    -- ** Request lenses
    dDocumentClassifierArn,

    -- * Destructuring the response
    DeleteDocumentClassifierResponse (..),
    mkDeleteDocumentClassifierResponse,

    -- ** Response lenses
    ddcrfrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteDocumentClassifier' smart constructor.
newtype DeleteDocumentClassifier = DeleteDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier.
    documentClassifierArn :: Types.DocumentClassifierArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentClassifier' value with any optional fields omitted.
mkDeleteDocumentClassifier ::
  -- | 'documentClassifierArn'
  Types.DocumentClassifierArn ->
  DeleteDocumentClassifier
mkDeleteDocumentClassifier documentClassifierArn =
  DeleteDocumentClassifier' {documentClassifierArn}

-- | The Amazon Resource Name (ARN) that identifies the document classifier.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDocumentClassifierArn :: Lens.Lens' DeleteDocumentClassifier Types.DocumentClassifierArn
dDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# DEPRECATED dDocumentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead." #-}

instance Core.FromJSON DeleteDocumentClassifier where
  toJSON DeleteDocumentClassifier {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DocumentClassifierArn" Core..= documentClassifierArn)
          ]
      )

instance Core.AWSRequest DeleteDocumentClassifier where
  type Rs DeleteDocumentClassifier = DeleteDocumentClassifierResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Comprehend_20171127.DeleteDocumentClassifier")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDocumentClassifierResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDocumentClassifierResponse' smart constructor.
newtype DeleteDocumentClassifierResponse = DeleteDocumentClassifierResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentClassifierResponse' value with any optional fields omitted.
mkDeleteDocumentClassifierResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDocumentClassifierResponse
mkDeleteDocumentClassifierResponse responseStatus =
  DeleteDocumentClassifierResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcrfrsResponseStatus :: Lens.Lens' DeleteDocumentClassifierResponse Core.Int
ddcrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddcrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
