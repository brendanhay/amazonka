{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.StopTrainingEntityRecognizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an entity recognizer training job while in progress.
--
-- If the training job state is @TRAINING@ , the job is marked for termination and put into the @STOP_REQUESTED@ state. If the training job completes before it can be stopped, it is put into the @TRAINED@ ; otherwise the training job is stopped and putted into the @STOPPED@ state and the service sends back an HTTP 200 response with an empty HTTP body.
module Network.AWS.Comprehend.StopTrainingEntityRecognizer
  ( -- * Creating a request
    StopTrainingEntityRecognizer (..),
    mkStopTrainingEntityRecognizer,

    -- ** Request lenses
    sterEntityRecognizerArn,

    -- * Destructuring the response
    StopTrainingEntityRecognizerResponse (..),
    mkStopTrainingEntityRecognizerResponse,

    -- ** Response lenses
    sterrrsResponseStatus,
  )
where

import qualified Network.AWS.Comprehend.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopTrainingEntityRecognizer' smart constructor.
newtype StopTrainingEntityRecognizer = StopTrainingEntityRecognizer'
  { -- | The Amazon Resource Name (ARN) that identifies the entity recognizer currently being trained.
    entityRecognizerArn :: Types.EntityRecognizerArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingEntityRecognizer' value with any optional fields omitted.
mkStopTrainingEntityRecognizer ::
  -- | 'entityRecognizerArn'
  Types.EntityRecognizerArn ->
  StopTrainingEntityRecognizer
mkStopTrainingEntityRecognizer entityRecognizerArn =
  StopTrainingEntityRecognizer' {entityRecognizerArn}

-- | The Amazon Resource Name (ARN) that identifies the entity recognizer currently being trained.
--
-- /Note:/ Consider using 'entityRecognizerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sterEntityRecognizerArn :: Lens.Lens' StopTrainingEntityRecognizer Types.EntityRecognizerArn
sterEntityRecognizerArn = Lens.field @"entityRecognizerArn"
{-# DEPRECATED sterEntityRecognizerArn "Use generic-lens or generic-optics with 'entityRecognizerArn' instead." #-}

instance Core.FromJSON StopTrainingEntityRecognizer where
  toJSON StopTrainingEntityRecognizer {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("EntityRecognizerArn" Core..= entityRecognizerArn)]
      )

instance Core.AWSRequest StopTrainingEntityRecognizer where
  type
    Rs StopTrainingEntityRecognizer =
      StopTrainingEntityRecognizerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Comprehend_20171127.StopTrainingEntityRecognizer"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopTrainingEntityRecognizerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopTrainingEntityRecognizerResponse' smart constructor.
newtype StopTrainingEntityRecognizerResponse = StopTrainingEntityRecognizerResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopTrainingEntityRecognizerResponse' value with any optional fields omitted.
mkStopTrainingEntityRecognizerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopTrainingEntityRecognizerResponse
mkStopTrainingEntityRecognizerResponse responseStatus =
  StopTrainingEntityRecognizerResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sterrrsResponseStatus :: Lens.Lens' StopTrainingEntityRecognizerResponse Core.Int
sterrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sterrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
