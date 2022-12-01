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
-- Module      : Amazonka.IoTSiteWise.Types.MeasurementProcessingConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.MeasurementProcessingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.ForwardingConfig
import qualified Amazonka.Prelude as Prelude

-- | The processing configuration for the given measurement property. You can
-- configure measurements to be kept at the edge or forwarded to the Amazon
-- Web Services Cloud. By default, measurements are forwarded to the cloud.
--
-- /See:/ 'newMeasurementProcessingConfig' smart constructor.
data MeasurementProcessingConfig = MeasurementProcessingConfig'
  { -- | The forwarding configuration for the given measurement property.
    forwardingConfig :: ForwardingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeasurementProcessingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forwardingConfig', 'measurementProcessingConfig_forwardingConfig' - The forwarding configuration for the given measurement property.
newMeasurementProcessingConfig ::
  -- | 'forwardingConfig'
  ForwardingConfig ->
  MeasurementProcessingConfig
newMeasurementProcessingConfig pForwardingConfig_ =
  MeasurementProcessingConfig'
    { forwardingConfig =
        pForwardingConfig_
    }

-- | The forwarding configuration for the given measurement property.
measurementProcessingConfig_forwardingConfig :: Lens.Lens' MeasurementProcessingConfig ForwardingConfig
measurementProcessingConfig_forwardingConfig = Lens.lens (\MeasurementProcessingConfig' {forwardingConfig} -> forwardingConfig) (\s@MeasurementProcessingConfig' {} a -> s {forwardingConfig = a} :: MeasurementProcessingConfig)

instance Core.FromJSON MeasurementProcessingConfig where
  parseJSON =
    Core.withObject
      "MeasurementProcessingConfig"
      ( \x ->
          MeasurementProcessingConfig'
            Prelude.<$> (x Core..: "forwardingConfig")
      )

instance Prelude.Hashable MeasurementProcessingConfig where
  hashWithSalt _salt MeasurementProcessingConfig' {..} =
    _salt `Prelude.hashWithSalt` forwardingConfig

instance Prelude.NFData MeasurementProcessingConfig where
  rnf MeasurementProcessingConfig' {..} =
    Prelude.rnf forwardingConfig

instance Core.ToJSON MeasurementProcessingConfig where
  toJSON MeasurementProcessingConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("forwardingConfig" Core..= forwardingConfig)
          ]
      )
