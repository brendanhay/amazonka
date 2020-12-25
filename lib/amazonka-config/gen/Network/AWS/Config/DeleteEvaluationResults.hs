{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    DeleteEvaluationResults (..),
    mkDeleteEvaluationResults,

    -- ** Request lenses
    derConfigRuleName,

    -- * Destructuring the response
    DeleteEvaluationResultsResponse (..),
    mkDeleteEvaluationResultsResponse,

    -- ** Response lenses
    derrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkDeleteEvaluationResults' smart constructor.
newtype DeleteEvaluationResults = DeleteEvaluationResults'
  { -- | The name of the AWS Config rule for which you want to delete the evaluation results.
    configRuleName :: Types.StringWithCharLimit64
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluationResults' value with any optional fields omitted.
mkDeleteEvaluationResults ::
  -- | 'configRuleName'
  Types.StringWithCharLimit64 ->
  DeleteEvaluationResults
mkDeleteEvaluationResults configRuleName =
  DeleteEvaluationResults' {configRuleName}

-- | The name of the AWS Config rule for which you want to delete the evaluation results.
--
-- /Note:/ Consider using 'configRuleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derConfigRuleName :: Lens.Lens' DeleteEvaluationResults Types.StringWithCharLimit64
derConfigRuleName = Lens.field @"configRuleName"
{-# DEPRECATED derConfigRuleName "Use generic-lens or generic-optics with 'configRuleName' instead." #-}

instance Core.FromJSON DeleteEvaluationResults where
  toJSON DeleteEvaluationResults {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("ConfigRuleName" Core..= configRuleName)]
      )

instance Core.AWSRequest DeleteEvaluationResults where
  type Rs DeleteEvaluationResults = DeleteEvaluationResultsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.DeleteEvaluationResults")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEvaluationResultsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The output when you delete the evaluation results for the specified AWS Config rule.
--
-- /See:/ 'mkDeleteEvaluationResultsResponse' smart constructor.
newtype DeleteEvaluationResultsResponse = DeleteEvaluationResultsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteEvaluationResultsResponse' value with any optional fields omitted.
mkDeleteEvaluationResultsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteEvaluationResultsResponse
mkDeleteEvaluationResultsResponse responseStatus =
  DeleteEvaluationResultsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
derrrsResponseStatus :: Lens.Lens' DeleteEvaluationResultsResponse Core.Int
derrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED derrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
