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
-- Module      : Amazonka.Kendra.Types.CapacityUnitsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.CapacityUnitsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies additional capacity units configured for your Enterprise
-- Edition index. You can add and remove capacity units to fit your usage
-- requirements.
--
-- /See:/ 'newCapacityUnitsConfiguration' smart constructor.
data CapacityUnitsConfiguration = CapacityUnitsConfiguration'
  { -- | The amount of extra storage capacity for an index. A single capacity
    -- unit provides 30 GB of storage space or 100,000 documents, whichever is
    -- reached first. You can add up to 100 extra capacity units.
    storageCapacityUnits :: Prelude.Natural,
    -- | The amount of extra query capacity for an index and
    -- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>
    -- capacity.
    --
    -- A single extra capacity unit for an index provides 0.1 queries per
    -- second or approximately 8,000 queries per day. You can add up to 100
    -- extra capacity units.
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
-- reached first. You can add up to 100 extra capacity units.
--
-- 'queryCapacityUnits', 'capacityUnitsConfiguration_queryCapacityUnits' - The amount of extra query capacity for an index and
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>
-- capacity.
--
-- A single extra capacity unit for an index provides 0.1 queries per
-- second or approximately 8,000 queries per day. You can add up to 100
-- extra capacity units.
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
-- reached first. You can add up to 100 extra capacity units.
capacityUnitsConfiguration_storageCapacityUnits :: Lens.Lens' CapacityUnitsConfiguration Prelude.Natural
capacityUnitsConfiguration_storageCapacityUnits = Lens.lens (\CapacityUnitsConfiguration' {storageCapacityUnits} -> storageCapacityUnits) (\s@CapacityUnitsConfiguration' {} a -> s {storageCapacityUnits = a} :: CapacityUnitsConfiguration)

-- | The amount of extra query capacity for an index and
-- <https://docs.aws.amazon.com/kendra/latest/dg/API_GetQuerySuggestions.html GetQuerySuggestions>
-- capacity.
--
-- A single extra capacity unit for an index provides 0.1 queries per
-- second or approximately 8,000 queries per day. You can add up to 100
-- extra capacity units.
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

instance Data.FromJSON CapacityUnitsConfiguration where
  parseJSON =
    Data.withObject
      "CapacityUnitsConfiguration"
      ( \x ->
          CapacityUnitsConfiguration'
            Prelude.<$> (x Data..: "StorageCapacityUnits")
            Prelude.<*> (x Data..: "QueryCapacityUnits")
      )

instance Prelude.Hashable CapacityUnitsConfiguration where
  hashWithSalt _salt CapacityUnitsConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` storageCapacityUnits
      `Prelude.hashWithSalt` queryCapacityUnits

instance Prelude.NFData CapacityUnitsConfiguration where
  rnf CapacityUnitsConfiguration' {..} =
    Prelude.rnf storageCapacityUnits
      `Prelude.seq` Prelude.rnf queryCapacityUnits

instance Data.ToJSON CapacityUnitsConfiguration where
  toJSON CapacityUnitsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StorageCapacityUnits"
                  Data..= storageCapacityUnits
              ),
            Prelude.Just
              ("QueryCapacityUnits" Data..= queryCapacityUnits)
          ]
      )
