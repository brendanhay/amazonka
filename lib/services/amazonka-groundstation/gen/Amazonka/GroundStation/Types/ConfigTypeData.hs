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
-- Module      : Amazonka.GroundStation.Types.ConfigTypeData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ConfigTypeData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.AntennaDownlinkConfig
import Amazonka.GroundStation.Types.AntennaDownlinkDemodDecodeConfig
import Amazonka.GroundStation.Types.AntennaUplinkConfig
import Amazonka.GroundStation.Types.DataflowEndpointConfig
import Amazonka.GroundStation.Types.S3RecordingConfig
import Amazonka.GroundStation.Types.TrackingConfig
import Amazonka.GroundStation.Types.UplinkEchoConfig
import qualified Amazonka.Prelude as Prelude

-- | Object containing the parameters of a @Config@.
--
-- See the subtype definitions for what each type of @Config@ contains.
--
-- /See:/ 'newConfigTypeData' smart constructor.
data ConfigTypeData = ConfigTypeData'
  { -- | Information about how AWS Ground Station should configure an antenna for
    -- downlink during a contact.
    antennaDownlinkConfig :: Prelude.Maybe AntennaDownlinkConfig,
    -- | Information about how AWS Ground Station should conﬁgure an antenna for
    -- downlink demod decode during a contact.
    antennaDownlinkDemodDecodeConfig :: Prelude.Maybe AntennaDownlinkDemodDecodeConfig,
    -- | Information about how AWS Ground Station should conﬁgure an antenna for
    -- uplink during a contact.
    antennaUplinkConfig :: Prelude.Maybe AntennaUplinkConfig,
    -- | Information about the dataflow endpoint @Config@.
    dataflowEndpointConfig :: Prelude.Maybe DataflowEndpointConfig,
    -- | Information about an S3 recording @Config@.
    s3RecordingConfig :: Prelude.Maybe S3RecordingConfig,
    -- | Object that determines whether tracking should be used during a contact
    -- executed with this @Config@ in the mission profile.
    trackingConfig :: Prelude.Maybe TrackingConfig,
    -- | Information about an uplink echo @Config@.
    --
    -- Parameters from the @AntennaUplinkConfig@, corresponding to the
    -- specified @AntennaUplinkConfigArn@, are used when this
    -- @UplinkEchoConfig@ is used in a contact.
    uplinkEchoConfig :: Prelude.Maybe UplinkEchoConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigTypeData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'antennaDownlinkConfig', 'configTypeData_antennaDownlinkConfig' - Information about how AWS Ground Station should configure an antenna for
-- downlink during a contact.
--
-- 'antennaDownlinkDemodDecodeConfig', 'configTypeData_antennaDownlinkDemodDecodeConfig' - Information about how AWS Ground Station should conﬁgure an antenna for
-- downlink demod decode during a contact.
--
-- 'antennaUplinkConfig', 'configTypeData_antennaUplinkConfig' - Information about how AWS Ground Station should conﬁgure an antenna for
-- uplink during a contact.
--
-- 'dataflowEndpointConfig', 'configTypeData_dataflowEndpointConfig' - Information about the dataflow endpoint @Config@.
--
-- 's3RecordingConfig', 'configTypeData_s3RecordingConfig' - Information about an S3 recording @Config@.
--
-- 'trackingConfig', 'configTypeData_trackingConfig' - Object that determines whether tracking should be used during a contact
-- executed with this @Config@ in the mission profile.
--
-- 'uplinkEchoConfig', 'configTypeData_uplinkEchoConfig' - Information about an uplink echo @Config@.
--
-- Parameters from the @AntennaUplinkConfig@, corresponding to the
-- specified @AntennaUplinkConfigArn@, are used when this
-- @UplinkEchoConfig@ is used in a contact.
newConfigTypeData ::
  ConfigTypeData
newConfigTypeData =
  ConfigTypeData'
    { antennaDownlinkConfig =
        Prelude.Nothing,
      antennaDownlinkDemodDecodeConfig = Prelude.Nothing,
      antennaUplinkConfig = Prelude.Nothing,
      dataflowEndpointConfig = Prelude.Nothing,
      s3RecordingConfig = Prelude.Nothing,
      trackingConfig = Prelude.Nothing,
      uplinkEchoConfig = Prelude.Nothing
    }

-- | Information about how AWS Ground Station should configure an antenna for
-- downlink during a contact.
configTypeData_antennaDownlinkConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe AntennaDownlinkConfig)
configTypeData_antennaDownlinkConfig = Lens.lens (\ConfigTypeData' {antennaDownlinkConfig} -> antennaDownlinkConfig) (\s@ConfigTypeData' {} a -> s {antennaDownlinkConfig = a} :: ConfigTypeData)

-- | Information about how AWS Ground Station should conﬁgure an antenna for
-- downlink demod decode during a contact.
configTypeData_antennaDownlinkDemodDecodeConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe AntennaDownlinkDemodDecodeConfig)
configTypeData_antennaDownlinkDemodDecodeConfig = Lens.lens (\ConfigTypeData' {antennaDownlinkDemodDecodeConfig} -> antennaDownlinkDemodDecodeConfig) (\s@ConfigTypeData' {} a -> s {antennaDownlinkDemodDecodeConfig = a} :: ConfigTypeData)

-- | Information about how AWS Ground Station should conﬁgure an antenna for
-- uplink during a contact.
configTypeData_antennaUplinkConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe AntennaUplinkConfig)
configTypeData_antennaUplinkConfig = Lens.lens (\ConfigTypeData' {antennaUplinkConfig} -> antennaUplinkConfig) (\s@ConfigTypeData' {} a -> s {antennaUplinkConfig = a} :: ConfigTypeData)

-- | Information about the dataflow endpoint @Config@.
configTypeData_dataflowEndpointConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe DataflowEndpointConfig)
configTypeData_dataflowEndpointConfig = Lens.lens (\ConfigTypeData' {dataflowEndpointConfig} -> dataflowEndpointConfig) (\s@ConfigTypeData' {} a -> s {dataflowEndpointConfig = a} :: ConfigTypeData)

-- | Information about an S3 recording @Config@.
configTypeData_s3RecordingConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe S3RecordingConfig)
configTypeData_s3RecordingConfig = Lens.lens (\ConfigTypeData' {s3RecordingConfig} -> s3RecordingConfig) (\s@ConfigTypeData' {} a -> s {s3RecordingConfig = a} :: ConfigTypeData)

-- | Object that determines whether tracking should be used during a contact
-- executed with this @Config@ in the mission profile.
configTypeData_trackingConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe TrackingConfig)
configTypeData_trackingConfig = Lens.lens (\ConfigTypeData' {trackingConfig} -> trackingConfig) (\s@ConfigTypeData' {} a -> s {trackingConfig = a} :: ConfigTypeData)

-- | Information about an uplink echo @Config@.
--
-- Parameters from the @AntennaUplinkConfig@, corresponding to the
-- specified @AntennaUplinkConfigArn@, are used when this
-- @UplinkEchoConfig@ is used in a contact.
configTypeData_uplinkEchoConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe UplinkEchoConfig)
configTypeData_uplinkEchoConfig = Lens.lens (\ConfigTypeData' {uplinkEchoConfig} -> uplinkEchoConfig) (\s@ConfigTypeData' {} a -> s {uplinkEchoConfig = a} :: ConfigTypeData)

instance Data.FromJSON ConfigTypeData where
  parseJSON =
    Data.withObject
      "ConfigTypeData"
      ( \x ->
          ConfigTypeData'
            Prelude.<$> (x Data..:? "antennaDownlinkConfig")
            Prelude.<*> (x Data..:? "antennaDownlinkDemodDecodeConfig")
            Prelude.<*> (x Data..:? "antennaUplinkConfig")
            Prelude.<*> (x Data..:? "dataflowEndpointConfig")
            Prelude.<*> (x Data..:? "s3RecordingConfig")
            Prelude.<*> (x Data..:? "trackingConfig")
            Prelude.<*> (x Data..:? "uplinkEchoConfig")
      )

instance Prelude.Hashable ConfigTypeData where
  hashWithSalt _salt ConfigTypeData' {..} =
    _salt
      `Prelude.hashWithSalt` antennaDownlinkConfig
      `Prelude.hashWithSalt` antennaDownlinkDemodDecodeConfig
      `Prelude.hashWithSalt` antennaUplinkConfig
      `Prelude.hashWithSalt` dataflowEndpointConfig
      `Prelude.hashWithSalt` s3RecordingConfig
      `Prelude.hashWithSalt` trackingConfig
      `Prelude.hashWithSalt` uplinkEchoConfig

instance Prelude.NFData ConfigTypeData where
  rnf ConfigTypeData' {..} =
    Prelude.rnf antennaDownlinkConfig
      `Prelude.seq` Prelude.rnf antennaDownlinkDemodDecodeConfig
      `Prelude.seq` Prelude.rnf antennaUplinkConfig
      `Prelude.seq` Prelude.rnf dataflowEndpointConfig
      `Prelude.seq` Prelude.rnf s3RecordingConfig
      `Prelude.seq` Prelude.rnf trackingConfig
      `Prelude.seq` Prelude.rnf uplinkEchoConfig

instance Data.ToJSON ConfigTypeData where
  toJSON ConfigTypeData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("antennaDownlinkConfig" Data..=)
              Prelude.<$> antennaDownlinkConfig,
            ("antennaDownlinkDemodDecodeConfig" Data..=)
              Prelude.<$> antennaDownlinkDemodDecodeConfig,
            ("antennaUplinkConfig" Data..=)
              Prelude.<$> antennaUplinkConfig,
            ("dataflowEndpointConfig" Data..=)
              Prelude.<$> dataflowEndpointConfig,
            ("s3RecordingConfig" Data..=)
              Prelude.<$> s3RecordingConfig,
            ("trackingConfig" Data..=)
              Prelude.<$> trackingConfig,
            ("uplinkEchoConfig" Data..=)
              Prelude.<$> uplinkEchoConfig
          ]
      )
