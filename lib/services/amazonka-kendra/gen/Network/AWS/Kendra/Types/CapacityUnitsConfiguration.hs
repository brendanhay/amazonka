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
-- Module      : Network.AWS.Kendra.Types.CapacityUnitsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.CapacityUnitsConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies capacity units configured for your enterprise edition index.
-- You can add and remove capacity units to tune an index to your
-- requirements.
--
-- /See:/ 'newCapacityUnitsConfiguration' smart constructor.
data CapacityUnitsConfiguration = CapacityUnitsConfiguration'
  { -- | The amount of extra storage capacity for an index. A single capacity
    -- unit provides 30 GB of storage space or 100,000 documents, whichever is
    -- reached first.
    storageCapacityUnits :: Prelude.Natural,
    -- | The amount of extra query capacity for an index and
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>
    -- capacity.
    --
    -- A single extra capacity unit for an index provides 0.1 queries per
    -- second or approximately 8,000 queries per day.
    --
    -- @GetQuerySuggestions@ capacity is five times the provisioned query
    -- capacity for an index, or the base capacity of 2.5 calls per second,
    -- whichever is higher. For example, the base capacity for an index is 0.1
    -- queries per second, and @GetQuerySuggestions@ capacity has a base of 2.5
    -- calls per second. If you add another 0.1 queries per second to total 0.2
    -- queries per second for an index, the @GetQuerySuggestions@ capacity is
    -- 2.5 calls per second (higher than five times 0.2 queries per second).
    queryCapacityUnits :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityUnitsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageCapacityUnits', 'capacityUnitsConfiguration_storageCapacityUnits' - The amount of extra storage capacity for an index. A single capacity
-- unit provides 30 GB of storage space or 100,000 documents, whichever is
-- reached first.
--
-- 'queryCapacityUnits', 'capacityUnitsConfiguration_queryCapacityUnits' - The amount of extra query capacity for an index and
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>
-- capacity.
--
-- A single extra capacity unit for an index provides 0.1 queries per
-- second or approximately 8,000 queries per day.
--
-- @GetQuerySuggestions@ capacity is five times the provisioned query
-- capacity for an index, or the base capacity of 2.5 calls per second,
-- whichever is higher. For example, the base capacity for an index is 0.1
-- queries per second, and @GetQuerySuggestions@ capacity has a base of 2.5
-- calls per second. If you add another 0.1 queries per second to total 0.2
-- queries per second for an index, the @GetQuerySuggestions@ capacity is
-- 2.5 calls per second (higher than five times 0.2 queries per second).
newCapacityUnitsConfiguration ::
  -- | 'storageCapacityUnits'
  Prelude.Natural ->
  -- | 'queryCapacityUnits'
  Prelude.Natural ->
  CapacityUnitsConfiguration
newCapacityUnitsConfiguration
  pStorageCapacityUnits_
  pQueryCapacityUnits_ =
    CapacityUnitsConfiguration'
      { storageCapacityUnits =
          pStorageCapacityUnits_,
        queryCapacityUnits = pQueryCapacityUnits_
      }

-- | The amount of extra storage capacity for an index. A single capacity
-- unit provides 30 GB of storage space or 100,000 documents, whichever is
-- reached first.
capacityUnitsConfiguration_storageCapacityUnits :: Lens.Lens' CapacityUnitsConfiguration Prelude.Natural
capacityUnitsConfiguration_storageCapacityUnits = Lens.lens (\CapacityUnitsConfiguration' {storageCapacityUnits} -> storageCapacityUnits) (\s@CapacityUnitsConfiguration' {} a -> s {storageCapacityUnits = a} :: CapacityUnitsConfiguration)

-- | The amount of extra query capacity for an index and
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>
-- capacity.
--
-- A single extra capacity unit for an index provides 0.1 queries per
-- second or approximately 8,000 queries per day.
--
-- @GetQuerySuggestions@ capacity is five times the provisioned query
-- capacity for an index, or the base capacity of 2.5 calls per second,
-- whichever is higher. For example, the base capacity for an index is 0.1
-- queries per second, and @GetQuerySuggestions@ capacity has a base of 2.5
-- calls per second. If you add another 0.1 queries per second to total 0.2
-- queries per second for an index, the @GetQuerySuggestions@ capacity is
-- 2.5 calls per second (higher than five times 0.2 queries per second).
capacityUnitsConfiguration_queryCapacityUnits :: Lens.Lens' CapacityUnitsConfiguration Prelude.Natural
capacityUnitsConfiguration_queryCapacityUnits = Lens.lens (\CapacityUnitsConfiguration' {queryCapacityUnits} -> queryCapacityUnits) (\s@CapacityUnitsConfiguration' {} a -> s {queryCapacityUnits = a} :: CapacityUnitsConfiguration)

instance Core.FromJSON CapacityUnitsConfiguration where
  parseJSON =
    Core.withObject
      "CapacityUnitsConfiguration"
      ( \x ->
          CapacityUnitsConfiguration'
            Prelude.<$> (x Core..: "StorageCapacityUnits")
            Prelude.<*> (x Core..: "QueryCapacityUnits")
      )

instance Prelude.Hashable CapacityUnitsConfiguration

instance Prelude.NFData CapacityUnitsConfiguration

instance Core.ToJSON CapacityUnitsConfiguration where
  toJSON CapacityUnitsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StorageCapacityUnits"
                  Core..= storageCapacityUnits
              ),
            Prelude.Just
              ("QueryCapacityUnits" Core..= queryCapacityUnits)
          ]
      )
