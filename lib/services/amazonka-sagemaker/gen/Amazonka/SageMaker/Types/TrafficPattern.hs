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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrafficPattern where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.Phase
import Amazonka.SageMaker.Types.TrafficType

-- | Defines the traffic pattern of the load test.
--
-- /See:/ 'newTrafficPattern' smart constructor.
data TrafficPattern = TrafficPattern'
  { -- | Defines the traffic patterns.
    trafficType :: Prelude.Maybe TrafficType,
    -- | Defines the phases traffic specification.
    phases :: Prelude.Maybe (Prelude.NonEmpty Phase)
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
-- 'trafficType', 'trafficPattern_trafficType' - Defines the traffic patterns.
--
-- 'phases', 'trafficPattern_phases' - Defines the phases traffic specification.
newTrafficPattern ::
  TrafficPattern
newTrafficPattern =
  TrafficPattern'
    { trafficType = Prelude.Nothing,
      phases = Prelude.Nothing
    }

-- | Defines the traffic patterns.
trafficPattern_trafficType :: Lens.Lens' TrafficPattern (Prelude.Maybe TrafficType)
trafficPattern_trafficType = Lens.lens (\TrafficPattern' {trafficType} -> trafficType) (\s@TrafficPattern' {} a -> s {trafficType = a} :: TrafficPattern)

-- | Defines the phases traffic specification.
trafficPattern_phases :: Lens.Lens' TrafficPattern (Prelude.Maybe (Prelude.NonEmpty Phase))
trafficPattern_phases = Lens.lens (\TrafficPattern' {phases} -> phases) (\s@TrafficPattern' {} a -> s {phases = a} :: TrafficPattern) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TrafficPattern where
  parseJSON =
    Core.withObject
      "TrafficPattern"
      ( \x ->
          TrafficPattern'
            Prelude.<$> (x Core..:? "TrafficType")
            Prelude.<*> (x Core..:? "Phases")
      )

instance Prelude.Hashable TrafficPattern where
  hashWithSalt _salt TrafficPattern' {..} =
    _salt `Prelude.hashWithSalt` trafficType
      `Prelude.hashWithSalt` phases

instance Prelude.NFData TrafficPattern where
  rnf TrafficPattern' {..} =
    Prelude.rnf trafficType
      `Prelude.seq` Prelude.rnf phases

instance Core.ToJSON TrafficPattern where
  toJSON TrafficPattern' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TrafficType" Core..=) Prelude.<$> trafficType,
            ("Phases" Core..=) Prelude.<$> phases
          ]
      )
