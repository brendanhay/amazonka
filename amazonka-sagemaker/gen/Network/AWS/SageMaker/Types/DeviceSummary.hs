{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.DeviceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeviceSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.EdgeModelSummary

-- | Summary of the device.
--
-- /See:/ 'newDeviceSummary' smart constructor.
data DeviceSummary = DeviceSummary'
  { -- | The name of the fleet the device belongs to.
    deviceFleetName :: Prelude.Maybe Prelude.Text,
    -- | The last heartbeat received from the device.
    latestHeartbeat :: Prelude.Maybe Prelude.POSIX,
    -- | The timestamp of the last registration or de-reregistration.
    registrationTime :: Prelude.Maybe Prelude.POSIX,
    -- | Models on the device.
    models :: Prelude.Maybe [EdgeModelSummary],
    -- | The AWS Internet of Things (IoT) object thing name associated with the
    -- device..
    iotThingName :: Prelude.Maybe Prelude.Text,
    -- | A description of the device.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the device.
    deviceName :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of the device.
    deviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeviceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceFleetName', 'deviceSummary_deviceFleetName' - The name of the fleet the device belongs to.
--
-- 'latestHeartbeat', 'deviceSummary_latestHeartbeat' - The last heartbeat received from the device.
--
-- 'registrationTime', 'deviceSummary_registrationTime' - The timestamp of the last registration or de-reregistration.
--
-- 'models', 'deviceSummary_models' - Models on the device.
--
-- 'iotThingName', 'deviceSummary_iotThingName' - The AWS Internet of Things (IoT) object thing name associated with the
-- device..
--
-- 'description', 'deviceSummary_description' - A description of the device.
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
    { deviceFleetName = Prelude.Nothing,
      latestHeartbeat = Prelude.Nothing,
      registrationTime = Prelude.Nothing,
      models = Prelude.Nothing,
      iotThingName = Prelude.Nothing,
      description = Prelude.Nothing,
      deviceName = pDeviceName_,
      deviceArn = pDeviceArn_
    }

-- | The name of the fleet the device belongs to.
deviceSummary_deviceFleetName :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_deviceFleetName = Lens.lens (\DeviceSummary' {deviceFleetName} -> deviceFleetName) (\s@DeviceSummary' {} a -> s {deviceFleetName = a} :: DeviceSummary)

-- | The last heartbeat received from the device.
deviceSummary_latestHeartbeat :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.UTCTime)
deviceSummary_latestHeartbeat = Lens.lens (\DeviceSummary' {latestHeartbeat} -> latestHeartbeat) (\s@DeviceSummary' {} a -> s {latestHeartbeat = a} :: DeviceSummary) Prelude.. Lens.mapping Prelude._Time

-- | The timestamp of the last registration or de-reregistration.
deviceSummary_registrationTime :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.UTCTime)
deviceSummary_registrationTime = Lens.lens (\DeviceSummary' {registrationTime} -> registrationTime) (\s@DeviceSummary' {} a -> s {registrationTime = a} :: DeviceSummary) Prelude.. Lens.mapping Prelude._Time

-- | Models on the device.
deviceSummary_models :: Lens.Lens' DeviceSummary (Prelude.Maybe [EdgeModelSummary])
deviceSummary_models = Lens.lens (\DeviceSummary' {models} -> models) (\s@DeviceSummary' {} a -> s {models = a} :: DeviceSummary) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS Internet of Things (IoT) object thing name associated with the
-- device..
deviceSummary_iotThingName :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_iotThingName = Lens.lens (\DeviceSummary' {iotThingName} -> iotThingName) (\s@DeviceSummary' {} a -> s {iotThingName = a} :: DeviceSummary)

-- | A description of the device.
deviceSummary_description :: Lens.Lens' DeviceSummary (Prelude.Maybe Prelude.Text)
deviceSummary_description = Lens.lens (\DeviceSummary' {description} -> description) (\s@DeviceSummary' {} a -> s {description = a} :: DeviceSummary)

-- | The unique identifier of the device.
deviceSummary_deviceName :: Lens.Lens' DeviceSummary Prelude.Text
deviceSummary_deviceName = Lens.lens (\DeviceSummary' {deviceName} -> deviceName) (\s@DeviceSummary' {} a -> s {deviceName = a} :: DeviceSummary)

-- | Amazon Resource Name (ARN) of the device.
deviceSummary_deviceArn :: Lens.Lens' DeviceSummary Prelude.Text
deviceSummary_deviceArn = Lens.lens (\DeviceSummary' {deviceArn} -> deviceArn) (\s@DeviceSummary' {} a -> s {deviceArn = a} :: DeviceSummary)

instance Prelude.FromJSON DeviceSummary where
  parseJSON =
    Prelude.withObject
      "DeviceSummary"
      ( \x ->
          DeviceSummary'
            Prelude.<$> (x Prelude..:? "DeviceFleetName")
            Prelude.<*> (x Prelude..:? "LatestHeartbeat")
            Prelude.<*> (x Prelude..:? "RegistrationTime")
            Prelude.<*> (x Prelude..:? "Models" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "IotThingName")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..: "DeviceName")
            Prelude.<*> (x Prelude..: "DeviceArn")
      )

instance Prelude.Hashable DeviceSummary

instance Prelude.NFData DeviceSummary
