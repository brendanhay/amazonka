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
-- Module      : Amazonka.SageMaker.Types.TrafficPattern
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrafficPattern where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.Phase
import Amazonka.SageMaker.Types.TrafficType

-- | Defines the traffic pattern of the load test.
--
-- /See:/ 'newTrafficPattern' smart constructor.
data TrafficPattern = TrafficPattern'
  { -- | Defines the phases traffic specification.
    phases :: Prelude.Maybe (Prelude.NonEmpty Phase),
    -- | Defines the traffic patterns.
    trafficType :: Prelude.Maybe TrafficType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrafficPattern' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phases', 'trafficPattern_phases' - Defines the phases traffic specification.
--
-- 'trafficType', 'trafficPattern_trafficType' - Defines the traffic patterns.
newTrafficPattern ::
  TrafficPattern
newTrafficPattern =
  TrafficPattern'
    { phases = Prelude.Nothing,
      trafficType = Prelude.Nothing
    }

-- | Defines the phases traffic specification.
trafficPattern_phases :: Lens.Lens' TrafficPattern (Prelude.Maybe (Prelude.NonEmpty Phase))
trafficPattern_phases = Lens.lens (\TrafficPattern' {phases} -> phases) (\s@TrafficPattern' {} a -> s {phases = a} :: TrafficPattern) Prelude.. Lens.mapping Lens.coerced

-- | Defines the traffic patterns.
trafficPattern_trafficType :: Lens.Lens' TrafficPattern (Prelude.Maybe TrafficType)
trafficPattern_trafficType = Lens.lens (\TrafficPattern' {trafficType} -> trafficType) (\s@TrafficPattern' {} a -> s {trafficType = a} :: TrafficPattern)

instance Data.FromJSON TrafficPattern where
  parseJSON =
    Data.withObject
      "TrafficPattern"
      ( \x ->
          TrafficPattern'
            Prelude.<$> (x Data..:? "Phases")
            Prelude.<*> (x Data..:? "TrafficType")
      )

instance Prelude.Hashable TrafficPattern where
  hashWithSalt _salt TrafficPattern' {..} =
    _salt
      `Prelude.hashWithSalt` phases
      `Prelude.hashWithSalt` trafficType

instance Prelude.NFData TrafficPattern where
  rnf TrafficPattern' {..} =
    Prelude.rnf phases `Prelude.seq`
      Prelude.rnf trafficType

instance Data.ToJSON TrafficPattern where
  toJSON TrafficPattern' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Phases" Data..=) Prelude.<$> phases,
            ("TrafficType" Data..=) Prelude.<$> trafficType
          ]
      )
