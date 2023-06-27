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
-- Module      : Amazonka.KendraRanking.Types.CapacityUnitsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KendraRanking.Types.CapacityUnitsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sets additional capacity units configured for your rescore execution
-- plan. A rescore execution plan is an Amazon Kendra Intelligent Ranking
-- resource used for provisioning the @Rescore@ API. You can add and remove
-- capacity units to fit your usage requirements.
--
-- /See:/ 'newCapacityUnitsConfiguration' smart constructor.
data CapacityUnitsConfiguration = CapacityUnitsConfiguration'
  { -- | The amount of extra capacity for your rescore execution plan.
    --
    -- A single extra capacity unit for a rescore execution plan provides 0.01
    -- rescore requests per second. You can add up to 1000 extra capacity
    -- units.
    rescoreCapacityUnits :: Prelude.Natural
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
-- 'rescoreCapacityUnits', 'capacityUnitsConfiguration_rescoreCapacityUnits' - The amount of extra capacity for your rescore execution plan.
--
-- A single extra capacity unit for a rescore execution plan provides 0.01
-- rescore requests per second. You can add up to 1000 extra capacity
-- units.
newCapacityUnitsConfiguration ::
  -- | 'rescoreCapacityUnits'
  Prelude.Natural ->
  CapacityUnitsConfiguration
newCapacityUnitsConfiguration pRescoreCapacityUnits_ =
  CapacityUnitsConfiguration'
    { rescoreCapacityUnits =
        pRescoreCapacityUnits_
    }

-- | The amount of extra capacity for your rescore execution plan.
--
-- A single extra capacity unit for a rescore execution plan provides 0.01
-- rescore requests per second. You can add up to 1000 extra capacity
-- units.
capacityUnitsConfiguration_rescoreCapacityUnits :: Lens.Lens' CapacityUnitsConfiguration Prelude.Natural
capacityUnitsConfiguration_rescoreCapacityUnits = Lens.lens (\CapacityUnitsConfiguration' {rescoreCapacityUnits} -> rescoreCapacityUnits) (\s@CapacityUnitsConfiguration' {} a -> s {rescoreCapacityUnits = a} :: CapacityUnitsConfiguration)

instance Data.FromJSON CapacityUnitsConfiguration where
  parseJSON =
    Data.withObject
      "CapacityUnitsConfiguration"
      ( \x ->
          CapacityUnitsConfiguration'
            Prelude.<$> (x Data..: "RescoreCapacityUnits")
      )

instance Prelude.Hashable CapacityUnitsConfiguration where
  hashWithSalt _salt CapacityUnitsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` rescoreCapacityUnits

instance Prelude.NFData CapacityUnitsConfiguration where
  rnf CapacityUnitsConfiguration' {..} =
    Prelude.rnf rescoreCapacityUnits

instance Data.ToJSON CapacityUnitsConfiguration where
  toJSON CapacityUnitsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RescoreCapacityUnits"
                  Data..= rescoreCapacityUnits
              )
          ]
      )
