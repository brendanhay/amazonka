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
    uaTags,
    uaAllocatedUsageQuantity,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Usage allocations allow you to split usage into buckets by tags.
--
-- Each UsageAllocation indicates the usage quantity for a specific set of tags.
--
-- /See:/ 'mkUsageAllocation' smart constructor.
data UsageAllocation = UsageAllocation'
  { tags ::
      Lude.Maybe (Lude.NonEmpty Tag),
    allocatedUsageQuantity :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsageAllocation' with the minimum fields required to make a request.
--
-- * 'allocatedUsageQuantity' - The total quantity allocated to this bucket of usage.
-- * 'tags' - The set of tags that define the bucket of usage. For the bucket of items with no tags, this parameter can be left out.
mkUsageAllocation ::
  -- | 'allocatedUsageQuantity'
  Lude.Natural ->
  UsageAllocation
mkUsageAllocation pAllocatedUsageQuantity_ =
  UsageAllocation'
    { tags = Lude.Nothing,
      allocatedUsageQuantity = pAllocatedUsageQuantity_
    }

-- | The set of tags that define the bucket of usage. For the bucket of items with no tags, this parameter can be left out.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaTags :: Lens.Lens' UsageAllocation (Lude.Maybe (Lude.NonEmpty Tag))
uaTags = Lens.lens (tags :: UsageAllocation -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: UsageAllocation)
{-# DEPRECATED uaTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The total quantity allocated to this bucket of usage.
--
-- /Note:/ Consider using 'allocatedUsageQuantity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAllocatedUsageQuantity :: Lens.Lens' UsageAllocation Lude.Natural
uaAllocatedUsageQuantity = Lens.lens (allocatedUsageQuantity :: UsageAllocation -> Lude.Natural) (\s a -> s {allocatedUsageQuantity = a} :: UsageAllocation)
{-# DEPRECATED uaAllocatedUsageQuantity "Use generic-lens or generic-optics with 'allocatedUsageQuantity' instead." #-}

instance Lude.FromJSON UsageAllocation where
  parseJSON =
    Lude.withObject
      "UsageAllocation"
      ( \x ->
          UsageAllocation'
            Lude.<$> (x Lude..:? "Tags") Lude.<*> (x Lude..: "AllocatedUsageQuantity")
      )

instance Lude.ToJSON UsageAllocation where
  toJSON UsageAllocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just
              ("AllocatedUsageQuantity" Lude..= allocatedUsageQuantity)
          ]
      )
