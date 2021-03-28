{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.Usage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.Usage
  ( Usage (..)
  -- * Smart constructor
  , mkUsage
  -- * Lenses
  , uEndDate
  , uItems
  , uPosition
  , uStartDate
  , uUsagePlanId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the usage data of a usage plan.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> , <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-create-usage-plans-with-console.html#api-gateway-usage-plan-manage-usage Manage Usage in a Usage Plan> 
--
-- /See:/ 'mkUsage' smart constructor.
data Usage = Usage'
  { endDate :: Core.Maybe Core.Text
    -- ^ The ending date of the usage data.
  , items :: Core.Maybe (Core.HashMap Core.Text [[Core.Integer]])
    -- ^ The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
  , position :: Core.Maybe Core.Text
  , startDate :: Core.Maybe Core.Text
    -- ^ The starting date of the usage data.
  , usagePlanId :: Core.Maybe Core.Text
    -- ^ The plan Id associated with this usage data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Usage' value with any optional fields omitted.
mkUsage
    :: Usage
mkUsage
  = Usage'{endDate = Core.Nothing, items = Core.Nothing,
           position = Core.Nothing, startDate = Core.Nothing,
           usagePlanId = Core.Nothing}

-- | The ending date of the usage data.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uEndDate :: Lens.Lens' Usage (Core.Maybe Core.Text)
uEndDate = Lens.field @"endDate"
{-# INLINEABLE uEndDate #-}
{-# DEPRECATED endDate "Use generic-lens or generic-optics with 'endDate' instead"  #-}

-- | The usage data, as daily logs of used and remaining quotas, over the specified time interval indexed over the API keys in a usage plan. For example, @{..., "values" : { "{api_key}" : [ [0, 100], [10, 90], [100, 10]]}@ , where @{api_key}@ stands for an API key value and the daily log entry is of the format @[used quota, remaining quota]@ .
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uItems :: Lens.Lens' Usage (Core.Maybe (Core.HashMap Core.Text [[Core.Integer]]))
uItems = Lens.field @"items"
{-# INLINEABLE uItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uPosition :: Lens.Lens' Usage (Core.Maybe Core.Text)
uPosition = Lens.field @"position"
{-# INLINEABLE uPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The starting date of the usage data.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uStartDate :: Lens.Lens' Usage (Core.Maybe Core.Text)
uStartDate = Lens.field @"startDate"
{-# INLINEABLE uStartDate #-}
{-# DEPRECATED startDate "Use generic-lens or generic-optics with 'startDate' instead"  #-}

-- | The plan Id associated with this usage data.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uUsagePlanId :: Lens.Lens' Usage (Core.Maybe Core.Text)
uUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE uUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

instance Core.FromJSON Usage where
        parseJSON
          = Core.withObject "Usage" Core.$
              \ x ->
                Usage' Core.<$>
                  (x Core..:? "endDate") Core.<*> x Core..:? "values" Core.<*>
                    x Core..:? "position"
                    Core.<*> x Core..:? "startDate"
                    Core.<*> x Core..:? "usagePlanId"
