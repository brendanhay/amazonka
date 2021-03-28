{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      PutEvaluations (..)
    , mkPutEvaluations
    -- ** Request lenses
    , peResultToken
    , peEvaluations
    , peTestMode

    -- * Destructuring the response
    , PutEvaluationsResponse (..)
    , mkPutEvaluationsResponse
    -- ** Response lenses
    , perrsFailedEvaluations
    , perrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkPutEvaluations' smart constructor.
data PutEvaluations = PutEvaluations'
  { resultToken :: Core.Text
    -- ^ An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
  , evaluations :: Core.Maybe [Types.Evaluation]
    -- ^ The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
  , testMode :: Core.Maybe Core.Bool
    -- ^ Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutEvaluations' value with any optional fields omitted.
mkPutEvaluations
    :: Core.Text -- ^ 'resultToken'
    -> PutEvaluations
mkPutEvaluations resultToken
  = PutEvaluations'{resultToken, evaluations = Core.Nothing,
                    testMode = Core.Nothing}

-- | An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
--
-- /Note:/ Consider using 'resultToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peResultToken :: Lens.Lens' PutEvaluations Core.Text
peResultToken = Lens.field @"resultToken"
{-# INLINEABLE peResultToken #-}
{-# DEPRECATED resultToken "Use generic-lens or generic-optics with 'resultToken' instead"  #-}

-- | The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
--
-- /Note:/ Consider using 'evaluations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peEvaluations :: Lens.Lens' PutEvaluations (Core.Maybe [Types.Evaluation])
peEvaluations = Lens.field @"evaluations"
{-# INLINEABLE peEvaluations #-}
{-# DEPRECATED evaluations "Use generic-lens or generic-optics with 'evaluations' instead"  #-}

-- | Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
--
-- /Note:/ Consider using 'testMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
peTestMode :: Lens.Lens' PutEvaluations (Core.Maybe Core.Bool)
peTestMode = Lens.field @"testMode"
{-# INLINEABLE peTestMode #-}
{-# DEPRECATED testMode "Use generic-lens or generic-optics with 'testMode' instead"  #-}

instance Core.ToQuery PutEvaluations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutEvaluations where
        toHeaders PutEvaluations{..}
          = Core.pure ("X-Amz-Target", "StarlingDoveService.PutEvaluations")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutEvaluations where
        toJSON PutEvaluations{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResultToken" Core..= resultToken),
                  ("Evaluations" Core..=) Core.<$> evaluations,
                  ("TestMode" Core..=) Core.<$> testMode])

instance Core.AWSRequest PutEvaluations where
        type Rs PutEvaluations = PutEvaluationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 PutEvaluationsResponse' Core.<$>
                   (x Core..:? "FailedEvaluations") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | 
--
-- /See:/ 'mkPutEvaluationsResponse' smart constructor.
data PutEvaluationsResponse = PutEvaluationsResponse'
  { failedEvaluations :: Core.Maybe [Types.Evaluation]
    -- ^ Requests that failed because of a client or server error.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutEvaluationsResponse' value with any optional fields omitted.
mkPutEvaluationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutEvaluationsResponse
mkPutEvaluationsResponse responseStatus
  = PutEvaluationsResponse'{failedEvaluations = Core.Nothing,
                            responseStatus}

-- | Requests that failed because of a client or server error.
--
-- /Note:/ Consider using 'failedEvaluations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsFailedEvaluations :: Lens.Lens' PutEvaluationsResponse (Core.Maybe [Types.Evaluation])
perrsFailedEvaluations = Lens.field @"failedEvaluations"
{-# INLINEABLE perrsFailedEvaluations #-}
{-# DEPRECATED failedEvaluations "Use generic-lens or generic-optics with 'failedEvaluations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
perrsResponseStatus :: Lens.Lens' PutEvaluationsResponse Core.Int
perrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE perrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
