{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.DescribeCostCategoryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the name, ARN, rules, definition, and effective dates of a Cost Category that's defined in the account.
--
-- You have the option to use @EffectiveOn@ to return a Cost Category that is active on a specific date. If there is no @EffectiveOn@ specified, you’ll see a Cost Category that is effective on the current date. If Cost Category is still effective, @EffectiveEnd@ is omitted in the response.
module Network.AWS.CostExplorer.DescribeCostCategoryDefinition
  ( -- * Creating a request
    DescribeCostCategoryDefinition (..),
    mkDescribeCostCategoryDefinition,

    -- ** Request lenses
    dCostCategoryArn,
    dEffectiveOn,

    -- * Destructuring the response
    DescribeCostCategoryDefinitionResponse (..),
    mkDescribeCostCategoryDefinitionResponse,

    -- ** Response lenses
    drsCostCategory,
    drsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCostCategoryDefinition' smart constructor.
data DescribeCostCategoryDefinition = DescribeCostCategoryDefinition'
  { -- | The unique identifier for your Cost Category.
    costCategoryArn :: Types.Arn,
    -- | The date when the Cost Category was effective.
    effectiveOn :: Core.Maybe Types.EffectiveOn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCostCategoryDefinition' value with any optional fields omitted.
mkDescribeCostCategoryDefinition ::
  -- | 'costCategoryArn'
  Types.Arn ->
  DescribeCostCategoryDefinition
mkDescribeCostCategoryDefinition costCategoryArn =
  DescribeCostCategoryDefinition'
    { costCategoryArn,
      effectiveOn = Core.Nothing
    }

-- | The unique identifier for your Cost Category.
--
-- /Note:/ Consider using 'costCategoryArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCostCategoryArn :: Lens.Lens' DescribeCostCategoryDefinition Types.Arn
dCostCategoryArn = Lens.field @"costCategoryArn"
{-# DEPRECATED dCostCategoryArn "Use generic-lens or generic-optics with 'costCategoryArn' instead." #-}

-- | The date when the Cost Category was effective.
--
-- /Note:/ Consider using 'effectiveOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEffectiveOn :: Lens.Lens' DescribeCostCategoryDefinition (Core.Maybe Types.EffectiveOn)
dEffectiveOn = Lens.field @"effectiveOn"
{-# DEPRECATED dEffectiveOn "Use generic-lens or generic-optics with 'effectiveOn' instead." #-}

instance Core.FromJSON DescribeCostCategoryDefinition where
  toJSON DescribeCostCategoryDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CostCategoryArn" Core..= costCategoryArn),
            ("EffectiveOn" Core..=) Core.<$> effectiveOn
          ]
      )

instance Core.AWSRequest DescribeCostCategoryDefinition where
  type
    Rs DescribeCostCategoryDefinition =
      DescribeCostCategoryDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSInsightsIndexService.DescribeCostCategoryDefinition"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCostCategoryDefinitionResponse'
            Core.<$> (x Core..:? "CostCategory") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeCostCategoryDefinitionResponse' smart constructor.
data DescribeCostCategoryDefinitionResponse = DescribeCostCategoryDefinitionResponse'
  { costCategory :: Core.Maybe Types.CostCategory,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCostCategoryDefinitionResponse' value with any optional fields omitted.
mkDescribeCostCategoryDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCostCategoryDefinitionResponse
mkDescribeCostCategoryDefinitionResponse responseStatus =
  DescribeCostCategoryDefinitionResponse'
    { costCategory =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'costCategory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCostCategory :: Lens.Lens' DescribeCostCategoryDefinitionResponse (Core.Maybe Types.CostCategory)
drsCostCategory = Lens.field @"costCategory"
{-# DEPRECATED drsCostCategory "Use generic-lens or generic-optics with 'costCategory' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeCostCategoryDefinitionResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
