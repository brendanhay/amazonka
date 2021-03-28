{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteEvaluationResults
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the evaluation results for the specified AWS Config rule. You can specify one AWS Config rule per request. After you delete the evaluation results, you can call the 'StartConfigRulesEvaluation' API to start evaluating your AWS resources against the rule.
module Network.AWS.Config.DeleteEvaluationResults
    (
    -- * Creating a request
      DeleteEvaluationResults (..)
    , mkDeleteEvaluationResults
    -- ** Request lenses
    , derConfigRuleName

    -- * Destructuring the response
    , DeleteEvaluationResultsResponse (..)
    , mkDeleteEvaluationResultsResponse
    -- ** Response lenses
    , derrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkDeleteEvaluationResults' smart constructor.
newtype DeleteEvaluationResults = DeleteEvaluationResults'
  { configRuleName :: Types.StringWithCharLimit64
    -- ^ The name of the AWS Config rule for which you want to delete the evaluation results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluationResults' value with any optional fields omitted.
mkDeleteEvaluationResults
    :: Types.StringWithCharLimit64 -- ^ 'configRuleName'
    -> DeleteEvaluationResults
mkDeleteEvaluationResults configRuleName
  = DeleteEvaluationResults'{configRuleName}

-- | The name of the AWS Config rule for which you want to delete the evaluation results.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derConfigRuleName :: Lens.Lens' DeleteEvaluationResults Types.StringWithCharLimit64
derConfigRuleName = Lens.field @"configRuleName"
{-# INLINEABLE derConfigRuleName #-}
{-# DEPRECATED configRuleName "Use generic-lens or generic-optics with 'configRuleName' instead"  #-}

instance Core.ToQuery DeleteEvaluationResults where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteEvaluationResults where
        toHeaders DeleteEvaluationResults{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.DeleteEvaluationResults")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteEvaluationResults where
        toJSON DeleteEvaluationResults{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConfigRuleName" Core..= configRuleName)])

instance Core.AWSRequest DeleteEvaluationResults where
        type Rs DeleteEvaluationResults = DeleteEvaluationResultsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteEvaluationResultsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output when you delete the evaluation results for the specified AWS Config rule.
--
-- /See:/ 'mkDeleteEvaluationResultsResponse' smart constructor.
newtype DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluationResultsResponse' value with any optional fields omitted.
mkDeleteEvaluationResultsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteEvaluationResultsResponse
mkDeleteEvaluationResultsResponse responseStatus
  = DeleteEvaluationResultsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrrsResponseStatus :: Lens.Lens' DeleteEvaluationResultsResponse Core.Int
derrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE derrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
