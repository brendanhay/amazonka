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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.EdgeModelSummary

-- | Summary of the device.
--
-- /See:/ 'newDeviceSummary' smart constructor.
data DeviceSummary = DeviceSummary'
  { -- | The name of the fleet the device belongs to.
    deviceFleetName :: Core.Maybe Core.Text,
    -- | The last heartbeat received from the device.
    latestHeartbeat :: Core.Maybe Core.POSIX,
    -- | The timestamp of the last registration or de-reregistration.
    registrationTime :: Core.Maybe Core.POSIX,
    -- | Models on the device.
    models :: Core.Maybe [EdgeModelSummary],
    -- | The AWS Internet of Things (IoT) object thing name associated with the
    -- device..
    iotThingName :: Core.Maybe Core.Text,
    -- | A description of the device.
    description :: Core.Maybe Core.Text,
    -- | The unique identifier of the device.
    deviceName :: Core.Text,
    -- | Amazon Resource Name (ARN) of the device.
    deviceArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'deviceArn'
  Core.Text ->
  DeviceSummary
newDeviceSummary pDeviceName_ pDeviceArn_ =
  DeviceSummary'
    { deviceFleetName = Core.Nothing,
      latestHeartbeat = Core.Nothing,
      registrationTime = Core.Nothing,
      models = Core.Nothing,
      iotThingName = Core.Nothing,
      description = Core.Nothing,
      deviceName = pDeviceName_,
      deviceArn = pDeviceArn_
    }

-- | The name of the fleet the device belongs to.
deviceSummary_deviceFleetName :: Lens.Lens' DeviceSummary (Core.Maybe Core.Text)
deviceSummary_deviceFleetName = Lens.lens (\DeviceSummary' {deviceFleetName} -> deviceFleetName) (\s@DeviceSummary' {} a -> s {deviceFleetName = a} :: DeviceSummary)

-- | The last heartbeat received from the device.
deviceSummary_latestHeartbeat :: Lens.Lens' DeviceSummary (Core.Maybe Core.UTCTime)
deviceSummary_latestHeartbeat = Lens.lens (\DeviceSummary' {latestHeartbeat} -> latestHeartbeat) (\s@DeviceSummary' {} a -> s {latestHeartbeat = a} :: DeviceSummary) Core.. Lens.mapping Core._Time

-- | The timestamp of the last registration or de-reregistration.
deviceSummary_registrationTime :: Lens.Lens' DeviceSummary (Core.Maybe Core.UTCTime)
deviceSummary_registrationTime = Lens.lens (\DeviceSummary' {registrationTime} -> registrationTime) (\s@DeviceSummary' {} a -> s {registrationTime = a} :: DeviceSummary) Core.. Lens.mapping Core._Time

-- | Models on the device.
deviceSummary_models :: Lens.Lens' DeviceSummary (Core.Maybe [EdgeModelSummary])
deviceSummary_models = Lens.lens (\DeviceSummary' {models} -> models) (\s@DeviceSummary' {} a -> s {models = a} :: DeviceSummary) Core.. Lens.mapping Lens._Coerce

-- | The AWS Internet of Things (IoT) object thing name associated with the
-- device..
deviceSummary_iotThingName :: Lens.Lens' DeviceSummary (Core.Maybe Core.Text)
deviceSummary_iotThingName = Lens.lens (\DeviceSummary' {iotThingName} -> iotThingName) (\s@DeviceSummary' {} a -> s {iotThingName = a} :: DeviceSummary)

-- | A description of the device.
deviceSummary_description :: Lens.Lens' DeviceSummary (Core.Maybe Core.Text)
deviceSummary_description = Lens.lens (\DeviceSummary' {description} -> description) (\s@DeviceSummary' {} a -> s {description = a} :: DeviceSummary)

-- | The unique identifier of the device.
deviceSummary_deviceName :: Lens.Lens' DeviceSummary Core.Text
deviceSummary_deviceName = Lens.lens (\DeviceSummary' {deviceName} -> deviceName) (\s@DeviceSummary' {} a -> s {deviceName = a} :: DeviceSummary)

-- | Amazon Resource Name (ARN) of the device.
deviceSummary_deviceArn :: Lens.Lens' DeviceSummary Core.Text
deviceSummary_deviceArn = Lens.lens (\DeviceSummary' {deviceArn} -> deviceArn) (\s@DeviceSummary' {} a -> s {deviceArn = a} :: DeviceSummary)

instance Core.FromJSON DeviceSummary where
  parseJSON =
    Core.withObject
      "DeviceSummary"
      ( \x ->
          DeviceSummary'
            Core.<$> (x Core..:? "DeviceFleetName")
            Core.<*> (x Core..:? "LatestHeartbeat")
            Core.<*> (x Core..:? "RegistrationTime")
            Core.<*> (x Core..:? "Models" Core..!= Core.mempty)
            Core.<*> (x Core..:? "IotThingName")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..: "DeviceName")
            Core.<*> (x Core..: "DeviceArn")
      )

instance Core.Hashable DeviceSummary

instance Core.NFData DeviceSummary
