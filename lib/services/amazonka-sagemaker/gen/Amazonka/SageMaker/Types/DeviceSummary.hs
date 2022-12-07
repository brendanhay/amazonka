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
-- Module      : Amazonka.SageMaker.Types.DeviceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DeviceSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EdgeModelSummary

-- | Summary of the device.
--
-- /See:/ 'newDeviceSummary' smart constructor.
data DeviceSummary = DeviceSummary'
  { -- | Models on the device.
    models :: Prelude.Maybe [EdgeModelSummary],
    -- | The Amazon Web Services Internet of Things (IoT) object thing name
    -- associated with the device..
    iotThingName :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet the device belongs to.
    deviceFleetName :: Prelude.Maybe Prelude.Text,
    -- | A description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last registration or de-reregistration.
    registrationTime :: Prelude.Maybe Data.POSIX,
    -- | The last heartbeat received from the device.
    latestHeartbeat :: Prelude.Maybe Data.POSIX,
    -- | Edge Manager agent version.
    agentVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the device.
    deviceName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of the device.
    deviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'models', 'deviceSummary_models' - Models on the device.
--
-- 'iotThingName', 'deviceSummary_iotThingName' - The Amazon Web Services Internet of Things (IoT) object thing name
-- associated with the device..
--
-- 'deviceFleetName', 'deviceSummary_deviceFleetName' - The name of the fleet the device belongs to.
--
-- 'description', 'deviceSummary_description' - A description of the device.
--
-- 'registrationTime', 'deviceSummary_registrationTime' - The timestamp of the last registration or de-reregistration.
--
-- 'latestHeartbeat', 'deviceSummary_latestHeartbeat' - The last heartbeat received from the device.
--
-- 'agentVersion', 'deviceSummary_agentVersion' - Edge Manager agent version.
--
-- 'deviceName', 'deviceSummary_deviceName' - The unique identifier of the device.
--
-- 'deviceArn', 'deviceSummary_deviceArn' - Amazon Resource Name (ARN) of the device.
newDeviceSummary ::
  -- | 'deviceName'
  Prelude.Text ->
  -- | 'deviceArn'
  Prelude.Text ->
  DeviceSummary
newDeviceSummary pDeviceName_ pDeviceArn_ =
  DeviceSummary'
    { models = Prelude.Nothing,
      iotThingName = Prelude.Nothing,
      deviceFleetName = Prelude.Nothing,
      description = Prelude.Nothing,
      registrationTime = Prelude.Nothing,
      latestHeartbeat = Prelude.Nothing,
      agentVersion = Prelude.Nothing,
      deviceName = pDeviceName_,
      deviceArn = pDeviceArn_
    }

-- | Models on the device.
deviceSummary_models :: Lens.Lens' DeviceSummary (Prelude.Maybe [EdgeModelSummary])
deviceSummary_models = Lens.lens (\DeviceSummary' {models} -> models) (\s@DeviceSummary' {} a -> s {models = a} :: DeviceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services Internet of Things (IoT) object thing name
-- associated with the device..
deviceSummary_iotThingName :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_iotThingName = Lens.lens (\DeviceSummary' {iotThingName} -> iotThingName) (\s@DeviceSummary' {} a -> s {iotThingName = a} :: DeviceSummary)

-- | The name of the fleet the device belongs to.
deviceSummary_deviceFleetName :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_deviceFleetName = Lens.lens (\DeviceSummary' {deviceFleetName} -> deviceFleetName) (\s@DeviceSummary' {} a -> s {deviceFleetName = a} :: DeviceSummary)

-- | A description of the device.
deviceSummary_description :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_description = Lens.lens (\DeviceSummary' {description} -> description) (\s@DeviceSummary' {} a -> s {description = a} :: DeviceSummary)

-- | The timestamp of the last registration or de-reregistration.
deviceSummary_registrationTime :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.UTCTime)
deviceSummary_registrationTime = Lens.lens (\DeviceSummary' {registrationTime} -> registrationTime) (\s@DeviceSummary' {} a -> s {registrationTime = a} :: DeviceSummary) Prelude.. Lens.mapping Data._Time

-- | The last heartbeat received from the device.
deviceSummary_latestHeartbeat :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.UTCTime)
deviceSummary_latestHeartbeat = Lens.lens (\DeviceSummary' {latestHeartbeat} -> latestHeartbeat) (\s@DeviceSummary' {} a -> s {latestHeartbeat = a} :: DeviceSummary) Prelude.. Lens.mapping Data._Time

-- | Edge Manager agent version.
deviceSummary_agentVersion :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_agentVersion = Lens.lens (\DeviceSummary' {agentVersion} -> agentVersion) (\s@DeviceSummary' {} a -> s {agentVersion = a} :: DeviceSummary)

-- | The unique identifier of the device.
deviceSummary_deviceName :: Lens.Lens' DeviceSummary Prelude.Text
deviceSummary_deviceName = Lens.lens (\DeviceSummary' {deviceName} -> deviceName) (\s@DeviceSummary' {} a -> s {deviceName = a} :: DeviceSummary)

-- | Amazon Resource Name (ARN) of the device.
deviceSummary_deviceArn :: Lens.Lens' DeviceSummary Prelude.Text
deviceSummary_deviceArn = Lens.lens (\DeviceSummary' {deviceArn} -> deviceArn) (\s@DeviceSummary' {} a -> s {deviceArn = a} :: DeviceSummary)

instance Data.FromJSON DeviceSummary where
  parseJSON =
    Data.withObject
      "DeviceSummary"
      ( \x ->
          DeviceSummary'
            Prelude.<$> (x Data..:? "Models" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IotThingName")
            Prelude.<*> (x Data..:? "DeviceFleetName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "RegistrationTime")
            Prelude.<*> (x Data..:? "LatestHeartbeat")
            Prelude.<*> (x Data..:? "AgentVersion")
            Prelude.<*> (x Data..: "DeviceName")
            Prelude.<*> (x Data..: "DeviceArn")
      )

instance Prelude.Hashable DeviceSummary where
  hashWithSalt _salt DeviceSummary' {..} =
    _salt `Prelude.hashWithSalt` models
      `Prelude.hashWithSalt` iotThingName
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` registrationTime
      `Prelude.hashWithSalt` latestHeartbeat
      `Prelude.hashWithSalt` agentVersion
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` deviceArn

instance Prelude.NFData DeviceSummary where
  rnf DeviceSummary' {..} =
    Prelude.rnf models
      `Prelude.seq` Prelude.rnf iotThingName
      `Prelude.seq` Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf registrationTime
      `Prelude.seq` Prelude.rnf latestHeartbeat
      `Prelude.seq` Prelude.rnf agentVersion
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf deviceArn
