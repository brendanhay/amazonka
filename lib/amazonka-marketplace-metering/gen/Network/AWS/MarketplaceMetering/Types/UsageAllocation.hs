{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageAllocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageAllocation
  ( UsageAllocation (..),

    -- * Smart constructor
    mkUsageAllocation,

    -- * Lenses
    uaAllocatedUsageQuantity,
    uaTags,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MarketplaceMetering.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Usage allocations allow you to split usage into buckets by tags.
--
-- Each UsageAllocation indicates the usage quantity for a specific set of tags.
--
-- /See:/ 'mkUsageAllocation' smart constructor.
data UsageAllocation = UsageAllocation'
  { -- | The total quantity allocated to this bucket of usage.
    allocatedUsageQuantity :: Core.Natural,
    -- | The set of tags that define the bucket of usage. For the bucket of items with no tags, this parameter can be left out.
    tags :: Core.Maybe (Core.NonEmpty Types.Tag)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UsageAllocation' value with any optional fields omitted.
mkUsageAllocation ::
  -- | 'allocatedUsageQuantity'
  Core.Natural ->
  UsageAllocation
mkUsageAllocation allocatedUsageQuantity =
  UsageAllocation' {allocatedUsageQuantity, tags = Core.Nothing}

-- | The total quantity allocated to this bucket of usage.
--
-- /Note:/ Consider using 'allocatedUsageQuantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAllocatedUsageQuantity :: Lens.Lens' UsageAllocation Core.Natural
uaAllocatedUsageQuantity = Lens.field @"allocatedUsageQuantity"
{-# DEPRECATED uaAllocatedUsageQuantity "Use generic-lens or generic-optics with 'allocatedUsageQuantity' instead." #-}

-- | The set of tags that define the bucket of usage. For the bucket of items with no tags, this parameter can be left out.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTags :: Lens.Lens' UsageAllocation (Core.Maybe (Core.NonEmpty Types.Tag))
uaTags = Lens.field @"tags"
{-# DEPRECATED uaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON UsageAllocation where
  toJSON UsageAllocation {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("AllocatedUsageQuantity" Core..= allocatedUsageQuantity),
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.FromJSON UsageAllocation where
  parseJSON =
    Core.withObject "UsageAllocation" Core.$
      \x ->
        UsageAllocation'
          Core.<$> (x Core..: "AllocatedUsageQuantity") Core.<*> (x Core..:? "Tags")
