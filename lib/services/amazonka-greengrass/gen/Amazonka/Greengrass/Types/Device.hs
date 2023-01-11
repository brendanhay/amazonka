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
-- Module      : Amazonka.Greengrass.Types.Device
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.Device where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON Device where
  parseJSON =
    Data.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Data..:? "SyncShadow")
            Prelude.<*> (x Data..: "ThingArn")
            Prelude.<*> (x Data..: "Id")
            Prelude.<*> (x Data..: "CertificateArn")
      )

instance Prelude.Hashable Device where
  hashWithSalt _salt Device' {..} =
    _salt `Prelude.hashWithSalt` syncShadow
      `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` certificateArn

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf syncShadow
      `Prelude.seq` Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf certificateArn

instance Data.ToJSON Device where
  toJSON Device' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SyncShadow" Data..=) Prelude.<$> syncShadow,
            Prelude.Just ("ThingArn" Data..= thingArn),
            Prelude.Just ("Id" Data..= id),
            Prelude.Just
              ("CertificateArn" Data..= certificateArn)
          ]
      )
