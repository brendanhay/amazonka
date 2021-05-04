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
-- Module      : Network.AWS.Greengrass.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.Device where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a device.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | If true, the device\'s local shadow will be automatically synced with
    -- the cloud.
    syncShadow :: Prelude.Maybe Prelude.Bool,
    -- | The thing ARN of the device.
    thingArn :: Prelude.Text,
    -- | A descriptive or arbitrary ID for the device. This value must be unique
    -- within the device definition version. Max length is 128 characters with
    -- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
    id :: Prelude.Text,
    -- | The ARN of the certificate associated with the device.
    certificateArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'certificateArn'
  Prelude.Text ->
  Device
newDevice pThingArn_ pId_ pCertificateArn_ =
  Device'
    { syncShadow = Prelude.Nothing,
      thingArn = pThingArn_,
      id = pId_,
      certificateArn = pCertificateArn_
    }

-- | If true, the device\'s local shadow will be automatically synced with
-- the cloud.
device_syncShadow :: Lens.Lens' Device (Prelude.Maybe Prelude.Bool)
device_syncShadow = Lens.lens (\Device' {syncShadow} -> syncShadow) (\s@Device' {} a -> s {syncShadow = a} :: Device)

-- | The thing ARN of the device.
device_thingArn :: Lens.Lens' Device Prelude.Text
device_thingArn = Lens.lens (\Device' {thingArn} -> thingArn) (\s@Device' {} a -> s {thingArn = a} :: Device)

-- | A descriptive or arbitrary ID for the device. This value must be unique
-- within the device definition version. Max length is 128 characters with
-- pattern \'\'[a-zA-Z0-9:_-]+\'\'.
device_id :: Lens.Lens' Device Prelude.Text
device_id = Lens.lens (\Device' {id} -> id) (\s@Device' {} a -> s {id = a} :: Device)

-- | The ARN of the certificate associated with the device.
device_certificateArn :: Lens.Lens' Device Prelude.Text
device_certificateArn = Lens.lens (\Device' {certificateArn} -> certificateArn) (\s@Device' {} a -> s {certificateArn = a} :: Device)

instance Prelude.FromJSON Device where
  parseJSON =
    Prelude.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Prelude..:? "SyncShadow")
            Prelude.<*> (x Prelude..: "ThingArn")
            Prelude.<*> (x Prelude..: "Id")
            Prelude.<*> (x Prelude..: "CertificateArn")
      )

instance Prelude.Hashable Device

instance Prelude.NFData Device

instance Prelude.ToJSON Device where
  toJSON Device' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("SyncShadow" Prelude..=) Prelude.<$> syncShadow,
            Prelude.Just ("ThingArn" Prelude..= thingArn),
            Prelude.Just ("Id" Prelude..= id),
            Prelude.Just
              ("CertificateArn" Prelude..= certificateArn)
          ]
      )
