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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.FleetMetricNameAndArn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The name and ARN of a fleet metric.
--
-- /See:/ 'newFleetMetricNameAndArn' smart constructor.
data FleetMetricNameAndArn = FleetMetricNameAndArn'
  { -- | The fleet metric ARN.
    metricArn :: Prelude.Maybe Prelude.Text,
    -- | The fleet metric name.
    metricName :: Prelude.Maybe Prelude.Text
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
-- 'metricArn', 'fleetMetricNameAndArn_metricArn' - The fleet metric ARN.
--
-- 'metricName', 'fleetMetricNameAndArn_metricName' - The fleet metric name.
newFleetMetricNameAndArn ::
  FleetMetricNameAndArn
newFleetMetricNameAndArn =
  FleetMetricNameAndArn'
    { metricArn = Prelude.Nothing,
      metricName = Prelude.Nothing
    }

-- | The fleet metric ARN.
fleetMetricNameAndArn_metricArn :: Lens.Lens' FleetMetricNameAndArn (Prelude.Maybe Prelude.Text)
fleetMetricNameAndArn_metricArn = Lens.lens (\FleetMetricNameAndArn' {metricArn} -> metricArn) (\s@FleetMetricNameAndArn' {} a -> s {metricArn = a} :: FleetMetricNameAndArn)

-- | The fleet metric name.
fleetMetricNameAndArn_metricName :: Lens.Lens' FleetMetricNameAndArn (Prelude.Maybe Prelude.Text)
fleetMetricNameAndArn_metricName = Lens.lens (\FleetMetricNameAndArn' {metricName} -> metricName) (\s@FleetMetricNameAndArn' {} a -> s {metricName = a} :: FleetMetricNameAndArn)

instance Data.FromJSON FleetMetricNameAndArn where
  parseJSON =
    Data.withObject
      "FleetMetricNameAndArn"
      ( \x ->
          FleetMetricNameAndArn'
            Prelude.<$> (x Data..:? "metricArn")
            Prelude.<*> (x Data..:? "metricName")
      )

instance Prelude.Hashable FleetMetricNameAndArn where
  hashWithSalt _salt FleetMetricNameAndArn' {..} =
    _salt `Prelude.hashWithSalt` metricArn
      `Prelude.hashWithSalt` metricName

instance Prelude.NFData FleetMetricNameAndArn where
  rnf FleetMetricNameAndArn' {..} =
    Prelude.rnf metricArn
      `Prelude.seq` Prelude.rnf metricName
