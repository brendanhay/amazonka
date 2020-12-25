{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopTrainingDocumentClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a document classifier training job while in progress.
--
-- If the training job state is @TRAINING@ , the job is marked for termination and put into the @STOP_REQUESTED@ state. If the training job completes before it can be stopped, it is put into the @TRAINED@ ; otherwise the training job is stopped and put into the @STOPPED@ state and the service sends back an HTTP 200 response with an empty HTTP body.
module Network.AWS.Comprehend.StopTrainingDocumentClassifier
  ( -- * Creating a request
    StopTrainingDocumentClassifier (..),
    mkStopTrainingDocumentClassifier,

    -- ** Request lenses
    stdcDocumentClassifierArn,

    -- * Destructuring the response
    StopTrainingDocumentClassifierResponse (..),
    mkStopTrainingDocumentClassifierResponse,

    -- ** Response lenses
    stdcrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopTrainingDocumentClassifier' smart constructor.
newtype StopTrainingDocumentClassifier = StopTrainingDocumentClassifier'
  { -- | The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
    documentClassifierArn :: Types.DocumentClassifierArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingDocumentClassifier' value with any optional fields omitted.
mkStopTrainingDocumentClassifier ::
  -- | 'documentClassifierArn'
  Types.DocumentClassifierArn ->
  StopTrainingDocumentClassifier
mkStopTrainingDocumentClassifier documentClassifierArn =
  StopTrainingDocumentClassifier' {documentClassifierArn}

-- | The Amazon Resource Name (ARN) that identifies the document classifier currently being trained.
--
-- /Note:/ Consider using 'documentClassifierArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdcDocumentClassifierArn :: Lens.Lens' StopTrainingDocumentClassifier Types.DocumentClassifierArn
stdcDocumentClassifierArn = Lens.field @"documentClassifierArn"
{-# DEPRECATED stdcDocumentClassifierArn "Use generic-lens or generic-optics with 'documentClassifierArn' instead." #-}

instance Core.FromJSON StopTrainingDocumentClassifier where
  toJSON StopTrainingDocumentClassifier {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DocumentClassifierArn" Core..= documentClassifierArn)
          ]
      )

instance Core.AWSRequest StopTrainingDocumentClassifier where
  type
    Rs StopTrainingDocumentClassifier =
      StopTrainingDocumentClassifierResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.StopTrainingDocumentClassifier"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopTrainingDocumentClassifierResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopTrainingDocumentClassifierResponse' smart constructor.
newtype StopTrainingDocumentClassifierResponse = StopTrainingDocumentClassifierResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingDocumentClassifierResponse' value with any optional fields omitted.
mkStopTrainingDocumentClassifierResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopTrainingDocumentClassifierResponse
mkStopTrainingDocumentClassifierResponse responseStatus =
  StopTrainingDocumentClassifierResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stdcrrsResponseStatus :: Lens.Lens' StopTrainingDocumentClassifierResponse Core.Int
stdcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED stdcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
