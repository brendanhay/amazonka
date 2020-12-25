{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.PutEvaluations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by an AWS Lambda function to deliver evaluation results to AWS Config. This action is required in every AWS Lambda function that is invoked by an AWS Config rule.
module Network.AWS.Config.PutEvaluations
  ( -- * Creating a request
    PutEvaluations (..),
    mkPutEvaluations,

    -- ** Request lenses
    peResultToken,
    peEvaluations,
    peTestMode,

    -- * Destructuring the response
    PutEvaluationsResponse (..),
    mkPutEvaluationsResponse,

    -- ** Response lenses
    perrsFailedEvaluations,
    perrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkPutEvaluations' smart constructor.
data PutEvaluations = PutEvaluations'
  { -- | An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
    resultToken :: Types.String,
    -- | The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
    evaluations :: Core.Maybe [Types.Evaluation],
    -- | Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
    testMode :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutEvaluations' value with any optional fields omitted.
mkPutEvaluations ::
  -- | 'resultToken'
  Types.String ->
  PutEvaluations
mkPutEvaluations resultToken =
  PutEvaluations'
    { resultToken,
      evaluations = Core.Nothing,
      testMode = Core.Nothing
    }

-- | An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
--
-- /Note:/ Consider using 'resultToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peResultToken :: Lens.Lens' PutEvaluations Types.String
peResultToken = Lens.field @"resultToken"
{-# DEPRECATED peResultToken "Use generic-lens or generic-optics with 'resultToken' instead." #-}

-- | The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
--
-- /Note:/ Consider using 'evaluations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEvaluations :: Lens.Lens' PutEvaluations (Core.Maybe [Types.Evaluation])
peEvaluations = Lens.field @"evaluations"
{-# DEPRECATED peEvaluations "Use generic-lens or generic-optics with 'evaluations' instead." #-}

-- | Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
--
-- /Note:/ Consider using 'testMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peTestMode :: Lens.Lens' PutEvaluations (Core.Maybe Core.Bool)
peTestMode = Lens.field @"testMode"
{-# DEPRECATED peTestMode "Use generic-lens or generic-optics with 'testMode' instead." #-}

instance Core.FromJSON PutEvaluations where
  toJSON PutEvaluations {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResultToken" Core..= resultToken),
            ("Evaluations" Core..=) Core.<$> evaluations,
            ("TestMode" Core..=) Core.<$> testMode
          ]
      )

instance Core.AWSRequest PutEvaluations where
  type Rs PutEvaluations = PutEvaluationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StarlingDoveService.PutEvaluations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEvaluationsResponse'
            Core.<$> (x Core..:? "FailedEvaluations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkPutEvaluationsResponse' smart constructor.
data PutEvaluationsResponse = PutEvaluationsResponse'
  { -- | Requests that failed because of a client or server error.
    failedEvaluations :: Core.Maybe [Types.Evaluation],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'PutEvaluationsResponse' value with any optional fields omitted.
mkPutEvaluationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutEvaluationsResponse
mkPutEvaluationsResponse responseStatus =
  PutEvaluationsResponse'
    { failedEvaluations = Core.Nothing,
      responseStatus
    }

-- | Requests that failed because of a client or server error.
--
-- /Note:/ Consider using 'failedEvaluations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsFailedEvaluations :: Lens.Lens' PutEvaluationsResponse (Core.Maybe [Types.Evaluation])
perrsFailedEvaluations = Lens.field @"failedEvaluations"
{-# DEPRECATED perrsFailedEvaluations "Use generic-lens or generic-optics with 'failedEvaluations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsResponseStatus :: Lens.Lens' PutEvaluationsResponse Core.Int
perrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED perrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
