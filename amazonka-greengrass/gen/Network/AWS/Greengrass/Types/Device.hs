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
-- Module      : Network.AWS.Greengrass.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Device where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | If true, the device\'s local shadow will be automatically synced with
    -- the cloud.
    syncShadow :: Core.Maybe Core.Bool,
    -- | The thing ARN of the device.
    thingArn :: Core.Text,
    -- | A descriptive or arbitrary ID for the device. This value must be unique
    -- within the device definition version. Max length is 128 characters with
    -- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Core.Text,
    -- | The ARN of the certificate associated with the device.
    certificateArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'syncShadow', 'device_syncShadow' - If true, the device\'s local shadow will be automatically synced with
-- the cloud.
--
-- 'thingArn', 'device_thingArn' - The thing ARN of the device.
--
-- 'id', 'device_id' - A descriptive or arbitrary ID for the device. This value must be unique
-- within the device definition version. Max length is 128 characters with
-- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
--
-- 'certificateArn', 'device_certificateArn' - The ARN of the certificate associated with the device.
newDevice ::
  -- | 'thingArn'
  Core.Text ->
  -- | 'id'
  Core.Text ->
  -- | 'certificateArn'
  Core.Text ->
  Device
newDevice pThingArn_ pId_ pCertificateArn_ =
  Device'
    { syncShadow = Core.Nothing,
      thingArn = pThingArn_,
      id = pId_,
      certificateArn = pCertificateArn_
    }

-- | If true, the device\'s local shadow will be automatically synced with
-- the cloud.
device_syncShadow :: Lens.Lens' Device (Core.Maybe Core.Bool)
device_syncShadow = Lens.lens (\Device' {syncShadow} -> syncShadow) (\s@Device' {} a -> s {syncShadow = a} :: Device)

-- | The thing ARN of the device.
device_thingArn :: Lens.Lens' Device Core.Text
device_thingArn = Lens.lens (\Device' {thingArn} -> thingArn) (\s@Device' {} a -> s {thingArn = a} :: Device)

-- | A descriptive or arbitrary ID for the device. This value must be unique
-- within the device definition version. Max length is 128 characters with
-- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
device_id :: Lens.Lens' Device Core.Text
device_id = Lens.lens (\Device' {id} -> id) (\s@Device' {} a -> s {id = a} :: Device)

-- | The ARN of the certificate associated with the device.
device_certificateArn :: Lens.Lens' Device Core.Text
device_certificateArn = Lens.lens (\Device' {certificateArn} -> certificateArn) (\s@Device' {} a -> s {certificateArn = a} :: Device)

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Core.<$> (x Core..:? "SyncShadow")
            Core.<*> (x Core..: "ThingArn")
            Core.<*> (x Core..: "Id")
            Core.<*> (x Core..: "CertificateArn")
      )

instance Core.Hashable Device

instance Core.NFData Device

instance Core.ToJSON Device where
  toJSON Device' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SyncShadow" Core..=) Core.<$> syncShadow,
            Core.Just ("ThingArn" Core..= thingArn),
            Core.Just ("Id" Core..= id),
            Core.Just ("CertificateArn" Core..= certificateArn)
          ]
      )
