{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.UsageAllocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceMetering.Types.UsageAllocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MarketplaceMetering.Types.Tag

-- | Usage allocations allow you to split usage into buckets by tags.
--
-- Each UsageAllocation indicates the usage quantity for a specific set of
-- tags.
--
-- /See:/ 'newUsageAllocation' smart constructor.
data UsageAllocation = UsageAllocation'
  { -- | The set of tags that define the bucket of usage. For the bucket of items
    -- with no tags, this parameter can be left out.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The total quantity allocated to this bucket of usage.
    allocatedUsageQuantity :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UsageAllocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'usageAllocation_tags' - The set of tags that define the bucket of usage. For the bucket of items
-- with no tags, this parameter can be left out.
--
-- 'allocatedUsageQuantity', 'usageAllocation_allocatedUsageQuantity' - The total quantity allocated to this bucket of usage.
newUsageAllocation ::
  -- | 'allocatedUsageQuantity'
  Core.Natural ->
  UsageAllocation
newUsageAllocation pAllocatedUsageQuantity_ =
  UsageAllocation'
    { tags = Core.Nothing,
      allocatedUsageQuantity = pAllocatedUsageQuantity_
    }

-- | The set of tags that define the bucket of usage. For the bucket of items
-- with no tags, this parameter can be left out.
usageAllocation_tags :: Lens.Lens' UsageAllocation (Core.Maybe (Core.NonEmpty Tag))
usageAllocation_tags = Lens.lens (\UsageAllocation' {tags} -> tags) (\s@UsageAllocation' {} a -> s {tags = a} :: UsageAllocation) Core.. Lens.mapping Lens._Coerce

-- | The total quantity allocated to this bucket of usage.
usageAllocation_allocatedUsageQuantity :: Lens.Lens' UsageAllocation Core.Natural
usageAllocation_allocatedUsageQuantity = Lens.lens (\UsageAllocation' {allocatedUsageQuantity} -> allocatedUsageQuantity) (\s@UsageAllocation' {} a -> s {allocatedUsageQuantity = a} :: UsageAllocation)

instance Core.FromJSON UsageAllocation where
  parseJSON =
    Core.withObject
      "UsageAllocation"
      ( \x ->
          UsageAllocation'
            Core.<$> (x Core..:? "Tags")
            Core.<*> (x Core..: "AllocatedUsageQuantity")
      )

instance Core.Hashable UsageAllocation

instance Core.NFData UsageAllocation

instance Core.ToJSON UsageAllocation where
  toJSON UsageAllocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Tags" Core..=) Core.<$> tags,
            Core.Just
              ( "AllocatedUsageQuantity"
                  Core..= allocatedUsageQuantity
              )
          ]
      )
