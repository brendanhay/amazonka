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
-- Module      : Amazonka.ImageBuilder.Types.InstanceBlockDeviceMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.InstanceBlockDeviceMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.EbsInstanceBlockDeviceSpecification
import qualified Amazonka.Prelude as Prelude

-- | Defines block device mappings for the instance used to configure your
-- image.
--
-- /See:/ 'newInstanceBlockDeviceMapping' smart constructor.
data InstanceBlockDeviceMapping = InstanceBlockDeviceMapping'
  { -- | The device to which these mappings apply.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | Use to manage Amazon EBS-specific configuration for this mapping.
    ebs :: Prelude.Maybe EbsInstanceBlockDeviceSpecification,
    -- | Use to remove a mapping from the base image.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | Use to manage instance ephemeral devices.
    virtualName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceBlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceName', 'instanceBlockDeviceMapping_deviceName' - The device to which these mappings apply.
--
-- 'ebs', 'instanceBlockDeviceMapping_ebs' - Use to manage Amazon EBS-specific configuration for this mapping.
--
-- 'noDevice', 'instanceBlockDeviceMapping_noDevice' - Use to remove a mapping from the base image.
--
-- 'virtualName', 'instanceBlockDeviceMapping_virtualName' - Use to manage instance ephemeral devices.
newInstanceBlockDeviceMapping ::
  InstanceBlockDeviceMapping
newInstanceBlockDeviceMapping =
  InstanceBlockDeviceMapping'
    { deviceName =
        Prelude.Nothing,
      ebs = Prelude.Nothing,
      noDevice = Prelude.Nothing,
      virtualName = Prelude.Nothing
    }

-- | The device to which these mappings apply.
instanceBlockDeviceMapping_deviceName :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMapping_deviceName = Lens.lens (\InstanceBlockDeviceMapping' {deviceName} -> deviceName) (\s@InstanceBlockDeviceMapping' {} a -> s {deviceName = a} :: InstanceBlockDeviceMapping)

-- | Use to manage Amazon EBS-specific configuration for this mapping.
instanceBlockDeviceMapping_ebs :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe EbsInstanceBlockDeviceSpecification)
instanceBlockDeviceMapping_ebs = Lens.lens (\InstanceBlockDeviceMapping' {ebs} -> ebs) (\s@InstanceBlockDeviceMapping' {} a -> s {ebs = a} :: InstanceBlockDeviceMapping)

-- | Use to remove a mapping from the base image.
instanceBlockDeviceMapping_noDevice :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMapping_noDevice = Lens.lens (\InstanceBlockDeviceMapping' {noDevice} -> noDevice) (\s@InstanceBlockDeviceMapping' {} a -> s {noDevice = a} :: InstanceBlockDeviceMapping)

-- | Use to manage instance ephemeral devices.
instanceBlockDeviceMapping_virtualName :: Lens.Lens' InstanceBlockDeviceMapping (Prelude.Maybe Prelude.Text)
instanceBlockDeviceMapping_virtualName = Lens.lens (\InstanceBlockDeviceMapping' {virtualName} -> virtualName) (\s@InstanceBlockDeviceMapping' {} a -> s {virtualName = a} :: InstanceBlockDeviceMapping)

instance Data.FromJSON InstanceBlockDeviceMapping where
  parseJSON =
    Data.withObject
      "InstanceBlockDeviceMapping"
      ( \x ->
          InstanceBlockDeviceMapping'
            Prelude.<$> (x Data..:? "deviceName")
            Prelude.<*> (x Data..:? "ebs")
            Prelude.<*> (x Data..:? "noDevice")
            Prelude.<*> (x Data..:? "virtualName")
      )

instance Prelude.Hashable InstanceBlockDeviceMapping where
  hashWithSalt _salt InstanceBlockDeviceMapping' {..} =
    _salt
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` ebs
      `Prelude.hashWithSalt` noDevice
      `Prelude.hashWithSalt` virtualName

instance Prelude.NFData InstanceBlockDeviceMapping where
  rnf InstanceBlockDeviceMapping' {..} =
    Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf ebs
      `Prelude.seq` Prelude.rnf noDevice
      `Prelude.seq` Prelude.rnf virtualName

instance Data.ToJSON InstanceBlockDeviceMapping where
  toJSON InstanceBlockDeviceMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("deviceName" Data..=) Prelude.<$> deviceName,
            ("ebs" Data..=) Prelude.<$> ebs,
            ("noDevice" Data..=) Prelude.<$> noDevice,
            ("virtualName" Data..=) Prelude.<$> virtualName
          ]
      )
