{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.StopHyperParameterTuningJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running hyperparameter tuning job and all running training jobs that the tuning job launched.
--
-- All model artifacts output from the training jobs are stored in Amazon Simple Storage Service (Amazon S3). All data that the training jobs write to Amazon CloudWatch Logs are still available in CloudWatch. After the tuning job moves to the @Stopped@ state, it releases all reserved resources for the tuning job.
module Network.AWS.SageMaker.StopHyperParameterTuningJob
  ( -- * Creating a request
    StopHyperParameterTuningJob (..),
    mkStopHyperParameterTuningJob,

    -- ** Request lenses
    shptjHyperParameterTuningJobName,

    -- * Destructuring the response
    StopHyperParameterTuningJobResponse (..),
    mkStopHyperParameterTuningJobResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkStopHyperParameterTuningJob' smart constructor.
newtype StopHyperParameterTuningJob = StopHyperParameterTuningJob'
  { -- | The name of the tuning job to stop.
    hyperParameterTuningJobName :: Types.HyperParameterTuningJobName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopHyperParameterTuningJob' value with any optional fields omitted.
mkStopHyperParameterTuningJob ::
  -- | 'hyperParameterTuningJobName'
  Types.HyperParameterTuningJobName ->
  StopHyperParameterTuningJob
mkStopHyperParameterTuningJob hyperParameterTuningJobName =
  StopHyperParameterTuningJob' {hyperParameterTuningJobName}

-- | The name of the tuning job to stop.
--
-- /Note:/ Consider using 'hyperParameterTuningJobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
shptjHyperParameterTuningJobName :: Lens.Lens' StopHyperParameterTuningJob Types.HyperParameterTuningJobName
shptjHyperParameterTuningJobName = Lens.field @"hyperParameterTuningJobName"
{-# DEPRECATED shptjHyperParameterTuningJobName "Use generic-lens or generic-optics with 'hyperParameterTuningJobName' instead." #-}

instance Core.FromJSON StopHyperParameterTuningJob where
  toJSON StopHyperParameterTuningJob {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "HyperParameterTuningJobName"
                  Core..= hyperParameterTuningJobName
              )
          ]
      )

instance Core.AWSRequest StopHyperParameterTuningJob where
  type
    Rs StopHyperParameterTuningJob =
      StopHyperParameterTuningJobResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.StopHyperParameterTuningJob")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveNull StopHyperParameterTuningJobResponse'

-- | /See:/ 'mkStopHyperParameterTuningJobResponse' smart constructor.
data StopHyperParameterTuningJobResponse = StopHyperParameterTuningJobResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StopHyperParameterTuningJobResponse' value with any optional fields omitted.
mkStopHyperParameterTuningJobResponse ::
  StopHyperParameterTuningJobResponse
mkStopHyperParameterTuningJobResponse =
  StopHyperParameterTuningJobResponse'
