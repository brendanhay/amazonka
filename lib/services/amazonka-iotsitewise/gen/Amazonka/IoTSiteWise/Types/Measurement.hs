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
-- Module      : Amazonka.IoTSiteWise.Types.Measurement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.Measurement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.MeasurementProcessingConfig
import qualified Amazonka.Prelude as Prelude

-- | Contains an asset measurement property. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/asset-properties.html#measurements Measurements>
-- in the /IoT SiteWise User Guide/.
--
-- /See:/ 'newMeasurement' smart constructor.
data Measurement = Measurement'
  { -- | The processing configuration for the given measurement property. You can
    -- configure measurements to be kept at the edge or forwarded to the Amazon
    -- Web Services Cloud. By default, measurements are forwarded to the cloud.
    processingConfig :: Prelude.Maybe MeasurementProcessingConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Measurement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'processingConfig', 'measurement_processingConfig' - The processing configuration for the given measurement property. You can
-- configure measurements to be kept at the edge or forwarded to the Amazon
-- Web Services Cloud. By default, measurements are forwarded to the cloud.
newMeasurement ::
  Measurement
newMeasurement =
  Measurement' {processingConfig = Prelude.Nothing}

-- | The processing configuration for the given measurement property. You can
-- configure measurements to be kept at the edge or forwarded to the Amazon
-- Web Services Cloud. By default, measurements are forwarded to the cloud.
measurement_processingConfig :: Lens.Lens' Measurement (Prelude.Maybe MeasurementProcessingConfig)
measurement_processingConfig = Lens.lens (\Measurement' {processingConfig} -> processingConfig) (\s@Measurement' {} a -> s {processingConfig = a} :: Measurement)

instance Data.FromJSON Measurement where
  parseJSON =
    Data.withObject
      "Measurement"
      ( \x ->
          Measurement'
            Prelude.<$> (x Data..:? "processingConfig")
      )

instance Prelude.Hashable Measurement where
  hashWithSalt _salt Measurement' {..} =
    _salt `Prelude.hashWithSalt` processingConfig

instance Prelude.NFData Measurement where
  rnf Measurement' {..} = Prelude.rnf processingConfig

instance Data.ToJSON Measurement where
  toJSON Measurement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("processingConfig" Data..=)
              Prelude.<$> processingConfig
          ]
      )
