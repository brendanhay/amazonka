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
-- Module      : Amazonka.IoT.Types.FleetMetricNameAndArn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.FleetMetricNameAndArn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The name and ARN of a fleet metric.
--
-- /See:/ 'newFleetMetricNameAndArn' smart constructor.
data FleetMetricNameAndArn = FleetMetricNameAndArn'
  { -- | The fleet metric name.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The fleet metric ARN.
    metricArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FleetMetricNameAndArn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'fleetMetricNameAndArn_metricName' - The fleet metric name.
--
-- 'metricArn', 'fleetMetricNameAndArn_metricArn' - The fleet metric ARN.
newFleetMetricNameAndArn ::
  FleetMetricNameAndArn
newFleetMetricNameAndArn =
  FleetMetricNameAndArn'
    { metricName =
        Prelude.Nothing,
      metricArn = Prelude.Nothing
    }

-- | The fleet metric name.
fleetMetricNameAndArn_metricName :: Lens.Lens' FleetMetricNameAndArn (Prelude.Maybe Prelude.Text)
fleetMetricNameAndArn_metricName = Lens.lens (\FleetMetricNameAndArn' {metricName} -> metricName) (\s@FleetMetricNameAndArn' {} a -> s {metricName = a} :: FleetMetricNameAndArn)

-- | The fleet metric ARN.
fleetMetricNameAndArn_metricArn :: Lens.Lens' FleetMetricNameAndArn (Prelude.Maybe Prelude.Text)
fleetMetricNameAndArn_metricArn = Lens.lens (\FleetMetricNameAndArn' {metricArn} -> metricArn) (\s@FleetMetricNameAndArn' {} a -> s {metricArn = a} :: FleetMetricNameAndArn)

instance Core.FromJSON FleetMetricNameAndArn where
  parseJSON =
    Core.withObject
      "FleetMetricNameAndArn"
      ( \x ->
          FleetMetricNameAndArn'
            Prelude.<$> (x Core..:? "metricName")
            Prelude.<*> (x Core..:? "metricArn")
      )

instance Prelude.Hashable FleetMetricNameAndArn where
  hashWithSalt _salt FleetMetricNameAndArn' {..} =
    _salt `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` metricArn

instance Prelude.NFData FleetMetricNameAndArn where
  rnf FleetMetricNameAndArn' {..} =
    Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf metricArn
