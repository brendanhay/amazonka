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
-- Module      : Network.AWS.IoTSiteWise.Types.MetricProcessingConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.MetricProcessingConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types.ComputeLocation
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The processing configuration for the given metric property. You can
-- configure metrics to be computed at the edge or in the Amazon Web
-- Services Cloud. By default, metrics are forwarded to the cloud.
--
-- /See:/ 'newMetricProcessingConfig' smart constructor.
data MetricProcessingConfig = MetricProcessingConfig'
  { -- | The compute location for the given metric property.
    computeLocation :: ComputeLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricProcessingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeLocation', 'metricProcessingConfig_computeLocation' - The compute location for the given metric property.
newMetricProcessingConfig ::
  -- | 'computeLocation'
  ComputeLocation ->
  MetricProcessingConfig
newMetricProcessingConfig pComputeLocation_ =
  MetricProcessingConfig'
    { computeLocation =
        pComputeLocation_
    }

-- | The compute location for the given metric property.
metricProcessingConfig_computeLocation :: Lens.Lens' MetricProcessingConfig ComputeLocation
metricProcessingConfig_computeLocation = Lens.lens (\MetricProcessingConfig' {computeLocation} -> computeLocation) (\s@MetricProcessingConfig' {} a -> s {computeLocation = a} :: MetricProcessingConfig)

instance Core.FromJSON MetricProcessingConfig where
  parseJSON =
    Core.withObject
      "MetricProcessingConfig"
      ( \x ->
          MetricProcessingConfig'
            Prelude.<$> (x Core..: "computeLocation")
      )

instance Prelude.Hashable MetricProcessingConfig

instance Prelude.NFData MetricProcessingConfig

instance Core.ToJSON MetricProcessingConfig where
  toJSON MetricProcessingConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("computeLocation" Core..= computeLocation)
          ]
      )
