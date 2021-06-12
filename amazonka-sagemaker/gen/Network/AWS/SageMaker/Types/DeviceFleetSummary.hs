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
-- Module      : Network.AWS.SageMaker.Types.DeviceFleetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DeviceFleetSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Summary of the device fleet.
--
-- /See:/ 'newDeviceFleetSummary' smart constructor.
data DeviceFleetSummary = DeviceFleetSummary'
  { -- | Timestamp of when the device fleet was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | Timestamp of when the device fleet was last updated.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | Amazon Resource Name (ARN) of the device fleet.
    deviceFleetArn :: Core.Text,
    -- | Name of the device fleet.
    deviceFleetName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeviceFleetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'deviceFleetSummary_creationTime' - Timestamp of when the device fleet was created.
--
-- 'lastModifiedTime', 'deviceFleetSummary_lastModifiedTime' - Timestamp of when the device fleet was last updated.
--
-- 'deviceFleetArn', 'deviceFleetSummary_deviceFleetArn' - Amazon Resource Name (ARN) of the device fleet.
--
-- 'deviceFleetName', 'deviceFleetSummary_deviceFleetName' - Name of the device fleet.
newDeviceFleetSummary ::
  -- | 'deviceFleetArn'
  Core.Text ->
  -- | 'deviceFleetName'
  Core.Text ->
  DeviceFleetSummary
newDeviceFleetSummary
  pDeviceFleetArn_
  pDeviceFleetName_ =
    DeviceFleetSummary'
      { creationTime = Core.Nothing,
        lastModifiedTime = Core.Nothing,
        deviceFleetArn = pDeviceFleetArn_,
        deviceFleetName = pDeviceFleetName_
      }

-- | Timestamp of when the device fleet was created.
deviceFleetSummary_creationTime :: Lens.Lens' DeviceFleetSummary (Core.Maybe Core.UTCTime)
deviceFleetSummary_creationTime = Lens.lens (\DeviceFleetSummary' {creationTime} -> creationTime) (\s@DeviceFleetSummary' {} a -> s {creationTime = a} :: DeviceFleetSummary) Core.. Lens.mapping Core._Time

-- | Timestamp of when the device fleet was last updated.
deviceFleetSummary_lastModifiedTime :: Lens.Lens' DeviceFleetSummary (Core.Maybe Core.UTCTime)
deviceFleetSummary_lastModifiedTime = Lens.lens (\DeviceFleetSummary' {lastModifiedTime} -> lastModifiedTime) (\s@DeviceFleetSummary' {} a -> s {lastModifiedTime = a} :: DeviceFleetSummary) Core.. Lens.mapping Core._Time

-- | Amazon Resource Name (ARN) of the device fleet.
deviceFleetSummary_deviceFleetArn :: Lens.Lens' DeviceFleetSummary Core.Text
deviceFleetSummary_deviceFleetArn = Lens.lens (\DeviceFleetSummary' {deviceFleetArn} -> deviceFleetArn) (\s@DeviceFleetSummary' {} a -> s {deviceFleetArn = a} :: DeviceFleetSummary)

-- | Name of the device fleet.
deviceFleetSummary_deviceFleetName :: Lens.Lens' DeviceFleetSummary Core.Text
deviceFleetSummary_deviceFleetName = Lens.lens (\DeviceFleetSummary' {deviceFleetName} -> deviceFleetName) (\s@DeviceFleetSummary' {} a -> s {deviceFleetName = a} :: DeviceFleetSummary)

instance Core.FromJSON DeviceFleetSummary where
  parseJSON =
    Core.withObject
      "DeviceFleetSummary"
      ( \x ->
          DeviceFleetSummary'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..: "DeviceFleetArn")
            Core.<*> (x Core..: "DeviceFleetName")
      )

instance Core.Hashable DeviceFleetSummary

instance Core.NFData DeviceFleetSummary
