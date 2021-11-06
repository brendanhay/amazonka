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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ConfigTypeData where

import qualified Amazonka.Core as Core
import Amazonka.GroundStation.Types.AntennaDownlinkConfig
import Amazonka.GroundStation.Types.AntennaDownlinkDemodDecodeConfig
import Amazonka.GroundStation.Types.AntennaUplinkConfig
import Amazonka.GroundStation.Types.DataflowEndpointConfig
import Amazonka.GroundStation.Types.S3RecordingConfig
import Amazonka.GroundStation.Types.TrackingConfig
import Amazonka.GroundStation.Types.UplinkEchoConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Object containing the parameters of a @Config@.
--
-- See the subtype definitions for what each type of @Config@ contains.
--
-- /See:/ 'newConfigTypeData' smart constructor.
data ConfigTypeData = ConfigTypeData'
  { -- | Information about how AWS Ground Station should conﬁgure an antenna for
    -- downlink demod decode during a contact.
    antennaDownlinkDemodDecodeConfig :: Prelude.Maybe AntennaDownlinkDemodDecodeConfig,
    -- | Information about how AWS Ground Station should configure an antenna for
    -- downlink during a contact.
    antennaDownlinkConfig :: Prelude.Maybe AntennaDownlinkConfig,
    -- | Information about how AWS Ground Station should conﬁgure an antenna for
    -- uplink during a contact.
    antennaUplinkConfig :: Prelude.Maybe AntennaUplinkConfig,
    -- | Information about an uplink echo @Config@.
    --
    -- Parameters from the @AntennaUplinkConfig@, corresponding to the
    -- specified @AntennaUplinkConfigArn@, are used when this
    -- @UplinkEchoConfig@ is used in a contact.
    uplinkEchoConfig :: Prelude.Maybe UplinkEchoConfig,
    -- | Information about an S3 recording @Config@.
    s3RecordingConfig :: Prelude.Maybe S3RecordingConfig,
    -- | Object that determines whether tracking should be used during a contact
    -- executed with this @Config@ in the mission profile.
    trackingConfig :: Prelude.Maybe TrackingConfig,
    -- | Information about the dataflow endpoint @Config@.
    dataflowEndpointConfig :: Prelude.Maybe DataflowEndpointConfig
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
-- 'antennaDownlinkDemodDecodeConfig', 'configTypeData_antennaDownlinkDemodDecodeConfig' - Information about how AWS Ground Station should conﬁgure an antenna for
-- downlink demod decode during a contact.
--
-- 'antennaDownlinkConfig', 'configTypeData_antennaDownlinkConfig' - Information about how AWS Ground Station should configure an antenna for
-- downlink during a contact.
--
-- 'antennaUplinkConfig', 'configTypeData_antennaUplinkConfig' - Information about how AWS Ground Station should conﬁgure an antenna for
-- uplink during a contact.
--
-- 'uplinkEchoConfig', 'configTypeData_uplinkEchoConfig' - Information about an uplink echo @Config@.
--
-- Parameters from the @AntennaUplinkConfig@, corresponding to the
-- specified @AntennaUplinkConfigArn@, are used when this
-- @UplinkEchoConfig@ is used in a contact.
--
-- 's3RecordingConfig', 'configTypeData_s3RecordingConfig' - Information about an S3 recording @Config@.
--
-- 'trackingConfig', 'configTypeData_trackingConfig' - Object that determines whether tracking should be used during a contact
-- executed with this @Config@ in the mission profile.
--
-- 'dataflowEndpointConfig', 'configTypeData_dataflowEndpointConfig' - Information about the dataflow endpoint @Config@.
newConfigTypeData ::
  ConfigTypeData
newConfigTypeData =
  ConfigTypeData'
    { antennaDownlinkDemodDecodeConfig =
        Prelude.Nothing,
      antennaDownlinkConfig = Prelude.Nothing,
      antennaUplinkConfig = Prelude.Nothing,
      uplinkEchoConfig = Prelude.Nothing,
      s3RecordingConfig = Prelude.Nothing,
      trackingConfig = Prelude.Nothing,
      dataflowEndpointConfig = Prelude.Nothing
    }

-- | Information about how AWS Ground Station should conﬁgure an antenna for
-- downlink demod decode during a contact.
configTypeData_antennaDownlinkDemodDecodeConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe AntennaDownlinkDemodDecodeConfig)
configTypeData_antennaDownlinkDemodDecodeConfig = Lens.lens (\ConfigTypeData' {antennaDownlinkDemodDecodeConfig} -> antennaDownlinkDemodDecodeConfig) (\s@ConfigTypeData' {} a -> s {antennaDownlinkDemodDecodeConfig = a} :: ConfigTypeData)

-- | Information about how AWS Ground Station should configure an antenna for
-- downlink during a contact.
configTypeData_antennaDownlinkConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe AntennaDownlinkConfig)
configTypeData_antennaDownlinkConfig = Lens.lens (\ConfigTypeData' {antennaDownlinkConfig} -> antennaDownlinkConfig) (\s@ConfigTypeData' {} a -> s {antennaDownlinkConfig = a} :: ConfigTypeData)

-- | Information about how AWS Ground Station should conﬁgure an antenna for
-- uplink during a contact.
configTypeData_antennaUplinkConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe AntennaUplinkConfig)
configTypeData_antennaUplinkConfig = Lens.lens (\ConfigTypeData' {antennaUplinkConfig} -> antennaUplinkConfig) (\s@ConfigTypeData' {} a -> s {antennaUplinkConfig = a} :: ConfigTypeData)

-- | Information about an uplink echo @Config@.
--
-- Parameters from the @AntennaUplinkConfig@, corresponding to the
-- specified @AntennaUplinkConfigArn@, are used when this
-- @UplinkEchoConfig@ is used in a contact.
configTypeData_uplinkEchoConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe UplinkEchoConfig)
configTypeData_uplinkEchoConfig = Lens.lens (\ConfigTypeData' {uplinkEchoConfig} -> uplinkEchoConfig) (\s@ConfigTypeData' {} a -> s {uplinkEchoConfig = a} :: ConfigTypeData)

-- | Information about an S3 recording @Config@.
configTypeData_s3RecordingConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe S3RecordingConfig)
configTypeData_s3RecordingConfig = Lens.lens (\ConfigTypeData' {s3RecordingConfig} -> s3RecordingConfig) (\s@ConfigTypeData' {} a -> s {s3RecordingConfig = a} :: ConfigTypeData)

-- | Object that determines whether tracking should be used during a contact
-- executed with this @Config@ in the mission profile.
configTypeData_trackingConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe TrackingConfig)
configTypeData_trackingConfig = Lens.lens (\ConfigTypeData' {trackingConfig} -> trackingConfig) (\s@ConfigTypeData' {} a -> s {trackingConfig = a} :: ConfigTypeData)

-- | Information about the dataflow endpoint @Config@.
configTypeData_dataflowEndpointConfig :: Lens.Lens' ConfigTypeData (Prelude.Maybe DataflowEndpointConfig)
configTypeData_dataflowEndpointConfig = Lens.lens (\ConfigTypeData' {dataflowEndpointConfig} -> dataflowEndpointConfig) (\s@ConfigTypeData' {} a -> s {dataflowEndpointConfig = a} :: ConfigTypeData)

instance Core.FromJSON ConfigTypeData where
  parseJSON =
    Core.withObject
      "ConfigTypeData"
      ( \x ->
          ConfigTypeData'
            Prelude.<$> (x Core..:? "antennaDownlinkDemodDecodeConfig")
            Prelude.<*> (x Core..:? "antennaDownlinkConfig")
            Prelude.<*> (x Core..:? "antennaUplinkConfig")
            Prelude.<*> (x Core..:? "uplinkEchoConfig")
            Prelude.<*> (x Core..:? "s3RecordingConfig")
            Prelude.<*> (x Core..:? "trackingConfig")
            Prelude.<*> (x Core..:? "dataflowEndpointConfig")
      )

instance Prelude.Hashable ConfigTypeData

instance Prelude.NFData ConfigTypeData

instance Core.ToJSON ConfigTypeData where
  toJSON ConfigTypeData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("antennaDownlinkDemodDecodeConfig" Core..=)
              Prelude.<$> antennaDownlinkDemodDecodeConfig,
            ("antennaDownlinkConfig" Core..=)
              Prelude.<$> antennaDownlinkConfig,
            ("antennaUplinkConfig" Core..=)
              Prelude.<$> antennaUplinkConfig,
            ("uplinkEchoConfig" Core..=)
              Prelude.<$> uplinkEchoConfig,
            ("s3RecordingConfig" Core..=)
              Prelude.<$> s3RecordingConfig,
            ("trackingConfig" Core..=)
              Prelude.<$> trackingConfig,
            ("dataflowEndpointConfig" Core..=)
              Prelude.<$> dataflowEndpointConfig
          ]
      )
