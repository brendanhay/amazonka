{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.StartConfigRulesEvaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs an on-demand evaluation for the specified AWS Config rules against the last known configuration state of the resources. Use @StartConfigRulesEvaluation@ when you want to test that a rule you updated is working as expected. @StartConfigRulesEvaluation@ does not re-record the latest configuration state for your resources. It re-runs an evaluation against the last known state of your resources.
--
-- You can specify up to 25 AWS Config rules per request.
-- An existing @StartConfigRulesEvaluation@ call for the specified rules must complete before you can call the API again. If you chose to have AWS Config stream to an Amazon SNS topic, you will receive a @ConfigRuleEvaluationStarted@ notification when the evaluation starts.
-- The @StartConfigRulesEvaluation@ API is useful if you want to run on-demand evaluations, such as the following example:
--
--     * You have a custom rule that evaluates your IAM resources every 24 hours.
--
--
--     * You update your Lambda function to add additional conditions to your rule.
--
--
--     * Instead of waiting for the next periodic evaluation, you call the @StartConfigRulesEvaluation@ API.
--
--
--     * AWS Config invokes your Lambda function and evaluates your IAM resources.
--
--
--     * Your custom rule will still run periodic evaluations every 24 hours.
module Network.AWS.Config.StartConfigRulesEvaluation
  ( -- * Creating a request
    StartConfigRulesEvaluation (..),
    mkStartConfigRulesEvaluation,

    -- ** Request lenses
    screConfigRuleNames,

    -- * Destructuring the response
    StartConfigRulesEvaluationResponse (..),
    mkStartConfigRulesEvaluationResponse,

    -- ** Response lenses
    screrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkStartConfigRulesEvaluation' smart constructor.
newtype StartConfigRulesEvaluation = StartConfigRulesEvaluation'
  { -- | The list of names of AWS Config rules that you want to run evaluations for.
    configRuleNames :: Core.Maybe (Core.NonEmpty Types.ConfigRuleName)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartConfigRulesEvaluation' value with any optional fields omitted.
mkStartConfigRulesEvaluation ::
  StartConfigRulesEvaluation
mkStartConfigRulesEvaluation =
  StartConfigRulesEvaluation' {configRuleNames = Core.Nothing}

-- | The list of names of AWS Config rules that you want to run evaluations for.
--
-- /Note:/ Consider using 'configRuleNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
screConfigRuleNames :: Lens.Lens' StartConfigRulesEvaluation (Core.Maybe (Core.NonEmpty Types.ConfigRuleName))
screConfigRuleNames = Lens.field @"configRuleNames"
{-# DEPRECATED screConfigRuleNames "Use generic-lens or generic-optics with 'configRuleNames' instead." #-}

instance Core.FromJSON StartConfigRulesEvaluation where
  toJSON StartConfigRulesEvaluation {..} =
    Core.object
      ( Core.catMaybes
          [("ConfigRuleNames" Core..=) Core.<$> configRuleNames]
      )

instance Core.AWSRequest StartConfigRulesEvaluation where
  type
    Rs StartConfigRulesEvaluation =
      StartConfigRulesEvaluationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StarlingDoveService.StartConfigRulesEvaluation")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartConfigRulesEvaluationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The output when you start the evaluation for the specified AWS Config rule.
--
-- /See:/ 'mkStartConfigRulesEvaluationResponse' smart constructor.
newtype StartConfigRulesEvaluationResponse = StartConfigRulesEvaluationResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartConfigRulesEvaluationResponse' value with any optional fields omitted.
mkStartConfigRulesEvaluationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartConfigRulesEvaluationResponse
mkStartConfigRulesEvaluationResponse responseStatus =
  StartConfigRulesEvaluationResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
screrrsResponseStatus :: Lens.Lens' StartConfigRulesEvaluationResponse Core.Int
screrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED screrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
