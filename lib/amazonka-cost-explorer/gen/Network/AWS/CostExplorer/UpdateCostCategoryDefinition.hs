{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.UpdateCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Cost Category. Changes made to the Cost Category rules will be used to categorize the current month’s expenses and future expenses. This won’t change categorization for the previous months.
module Network.AWS.CostExplorer.UpdateCostCategoryDefinition
  ( -- * Creating a request
    UpdateCostCategoryDefinition (..),
    mkUpdateCostCategoryDefinition,

    -- ** Request lenses
    uccdCostCategoryArn,
    uccdRuleVersion,
    uccdRules,

    -- * Destructuring the response
    UpdateCostCategoryDefinitionResponse (..),
    mkUpdateCostCategoryDefinitionResponse,

    -- ** Response lenses
    uccdrrsCostCategoryArn,
    uccdrrsEffectiveStart,
    uccdrrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCostCategoryDefinition' smart constructor.
data UpdateCostCategoryDefinition = UpdateCostCategoryDefinition'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Types.Arn,
    ruleVersion :: Types.CostCategoryRuleVersion,
    -- | The @Expression@ object used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule > .
    rules :: Core.NonEmpty Types.CostCategoryRule
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCostCategoryDefinition' value with any optional fields omitted.
mkUpdateCostCategoryDefinition ::
  -- | 'costCategoryArn'
  Types.Arn ->
  -- | 'ruleVersion'
  Types.CostCategoryRuleVersion ->
  -- | 'rules'
  Core.NonEmpty Types.CostCategoryRule ->
  UpdateCostCategoryDefinition
mkUpdateCostCategoryDefinition costCategoryArn ruleVersion rules =
  UpdateCostCategoryDefinition'
    { costCategoryArn,
      ruleVersion,
      rules
    }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdCostCategoryArn :: Lens.Lens' UpdateCostCategoryDefinition Types.Arn
uccdCostCategoryArn = Lens.field @"costCategoryArn"
{-# DEPRECATED uccdCostCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'ruleVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdRuleVersion :: Lens.Lens' UpdateCostCategoryDefinition Types.CostCategoryRuleVersion
uccdRuleVersion = Lens.field @"ruleVersion"
{-# DEPRECATED uccdRuleVersion "Use generic-lens or generic-optics with 'ruleVersion' instead." #-}

-- | The @Expression@ object used to categorize costs. For more information, see <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_CostCategoryRule.html CostCategoryRule > .
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdRules :: Lens.Lens' UpdateCostCategoryDefinition (Core.NonEmpty Types.CostCategoryRule)
uccdRules = Lens.field @"rules"
{-# DEPRECATED uccdRules "Use generic-lens or generic-optics with 'rules' instead." #-}

instance Core.FromJSON UpdateCostCategoryDefinition where
  toJSON UpdateCostCategoryDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CostCategoryArn" Core..= costCategoryArn),
            Core.Just ("RuleVersion" Core..= ruleVersion),
            Core.Just ("Rules" Core..= rules)
          ]
      )

instance Core.AWSRequest UpdateCostCategoryDefinition where
  type
    Rs UpdateCostCategoryDefinition =
      UpdateCostCategoryDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSInsightsIndexService.UpdateCostCategoryDefinition"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCostCategoryDefinitionResponse'
            Core.<$> (x Core..:? "CostCategoryArn")
            Core.<*> (x Core..:? "EffectiveStart")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateCostCategoryDefinitionResponse' smart constructor.
data UpdateCostCategoryDefinitionResponse = UpdateCostCategoryDefinitionResponse'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Core.Maybe Types.Arn,
    -- | The Cost Category's effective start date.
    effectiveStart :: Core.Maybe Types.EffectiveStart,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCostCategoryDefinitionResponse' value with any optional fields omitted.
mkUpdateCostCategoryDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateCostCategoryDefinitionResponse
mkUpdateCostCategoryDefinitionResponse responseStatus =
  UpdateCostCategoryDefinitionResponse'
    { costCategoryArn =
        Core.Nothing,
      effectiveStart = Core.Nothing,
      responseStatus
    }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdrrsCostCategoryArn :: Lens.Lens' UpdateCostCategoryDefinitionResponse (Core.Maybe Types.Arn)
uccdrrsCostCategoryArn = Lens.field @"costCategoryArn"
{-# DEPRECATED uccdrrsCostCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead." #-}

-- | The Cost Category's effective start date.
--
-- /Note:/ Consider using 'effectiveStart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdrrsEffectiveStart :: Lens.Lens' UpdateCostCategoryDefinitionResponse (Core.Maybe Types.EffectiveStart)
uccdrrsEffectiveStart = Lens.field @"effectiveStart"
{-# DEPRECATED uccdrrsEffectiveStart "Use generic-lens or generic-optics with 'effectiveStart' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uccdrrsResponseStatus :: Lens.Lens' UpdateCostCategoryDefinitionResponse Core.Int
uccdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uccdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
