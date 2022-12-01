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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyDetectorConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyDetectorConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LookoutMetrics.Types.Frequency
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a detector\'s configuration.
--
-- /See:/ 'newAnomalyDetectorConfig' smart constructor.
data AnomalyDetectorConfig = AnomalyDetectorConfig'
  { -- | The frequency at which the detector analyzes its source data.
    anomalyDetectorFrequency :: Prelude.Maybe Frequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyDetectorConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyDetectorFrequency', 'anomalyDetectorConfig_anomalyDetectorFrequency' - The frequency at which the detector analyzes its source data.
newAnomalyDetectorConfig ::
  AnomalyDetectorConfig
newAnomalyDetectorConfig =
  AnomalyDetectorConfig'
    { anomalyDetectorFrequency =
        Prelude.Nothing
    }

-- | The frequency at which the detector analyzes its source data.
anomalyDetectorConfig_anomalyDetectorFrequency :: Lens.Lens' AnomalyDetectorConfig (Prelude.Maybe Frequency)
anomalyDetectorConfig_anomalyDetectorFrequency = Lens.lens (\AnomalyDetectorConfig' {anomalyDetectorFrequency} -> anomalyDetectorFrequency) (\s@AnomalyDetectorConfig' {} a -> s {anomalyDetectorFrequency = a} :: AnomalyDetectorConfig)

instance Prelude.Hashable AnomalyDetectorConfig where
  hashWithSalt _salt AnomalyDetectorConfig' {..} =
    _salt
      `Prelude.hashWithSalt` anomalyDetectorFrequency

instance Prelude.NFData AnomalyDetectorConfig where
  rnf AnomalyDetectorConfig' {..} =
    Prelude.rnf anomalyDetectorFrequency

instance Core.ToJSON AnomalyDetectorConfig where
  toJSON AnomalyDetectorConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AnomalyDetectorFrequency" Core..=)
              Prelude.<$> anomalyDetectorFrequency
          ]
      )
