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
-- Module      : Amazonka.MarketplaceMetering.Types.UsageAllocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceMetering.Types.UsageAllocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MarketplaceMetering.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Usage allocations allow you to split usage into buckets by tags.
--
-- Each @UsageAllocation@ indicates the usage quantity for a specific set
-- of tags.
--
-- /See:/ 'newUsageAllocation' smart constructor.
data UsageAllocation = UsageAllocation'
  { -- | The set of tags that define the bucket of usage. For the bucket of items
    -- with no tags, this parameter can be left out.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The total quantity allocated to this bucket of usage.
    allocatedUsageQuantity :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Natural ->
  UsageAllocation
newUsageAllocation pAllocatedUsageQuantity_ =
  UsageAllocation'
    { tags = Prelude.Nothing,
      allocatedUsageQuantity = pAllocatedUsageQuantity_
    }

-- | The set of tags that define the bucket of usage. For the bucket of items
-- with no tags, this parameter can be left out.
usageAllocation_tags :: Lens.Lens' UsageAllocation (Prelude.Maybe (Prelude.NonEmpty Tag))
usageAllocation_tags = Lens.lens (\UsageAllocation' {tags} -> tags) (\s@UsageAllocation' {} a -> s {tags = a} :: UsageAllocation) Prelude.. Lens.mapping Lens.coerced

-- | The total quantity allocated to this bucket of usage.
usageAllocation_allocatedUsageQuantity :: Lens.Lens' UsageAllocation Prelude.Natural
usageAllocation_allocatedUsageQuantity = Lens.lens (\UsageAllocation' {allocatedUsageQuantity} -> allocatedUsageQuantity) (\s@UsageAllocation' {} a -> s {allocatedUsageQuantity = a} :: UsageAllocation)

instance Core.FromJSON UsageAllocation where
  parseJSON =
    Core.withObject
      "UsageAllocation"
      ( \x ->
          UsageAllocation'
            Prelude.<$> (x Core..:? "Tags")
            Prelude.<*> (x Core..: "AllocatedUsageQuantity")
      )

instance Prelude.Hashable UsageAllocation where
  hashWithSalt _salt UsageAllocation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` allocatedUsageQuantity

instance Prelude.NFData UsageAllocation where
  rnf UsageAllocation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf allocatedUsageQuantity

instance Core.ToJSON UsageAllocation where
  toJSON UsageAllocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "AllocatedUsageQuantity"
                  Core..= allocatedUsageQuantity
              )
          ]
      )
