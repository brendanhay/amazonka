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
-- Module      : Amazonka.AutoScaling.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.BlockDeviceMapping where

import Amazonka.AutoScaling.Types.Ebs
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a block device mapping.
--
-- /See:/ 'newBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { -- | Information to attach an EBS volume to an instance at launch.
    ebs :: Prelude.Maybe Ebs,
    -- | Setting this value to @true@ prevents a volume that is included in the
    -- block device mapping of the AMI from being mapped to the specified
    -- device name at launch.
    --
    -- If @NoDevice@ is @true@ for the root device, instances might fail the
    -- EC2 health check. In that case, Amazon EC2 Auto Scaling launches
    -- replacement instances.
    noDevice :: Prelude.Maybe Prelude.Bool,
    -- | The name of the instance store volume (virtual device) to attach to an
    -- instance at launch. The name must be in the form ephemeral/X/ where /X/
    -- is a number starting from zero (0), for example, @ephemeral0@.
    virtualName :: Prelude.Maybe Prelude.Text,
    -- | The device name assigned to the volume (for example, @\/dev\/sdh@ or
    -- @xvdh@). For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device naming on Linux instances>
    -- in the /Amazon EC2 User Guide for Linux Instances/.
    --
    -- To define a block device mapping, set the device name and exactly one of
    -- the following properties: @Ebs@, @NoDevice@, or @VirtualName@.
    deviceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlockDeviceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ebs', 'blockDeviceMapping_ebs' - Information to attach an EBS volume to an instance at launch.
--
-- 'noDevice', 'blockDeviceMapping_noDevice' - Setting this value to @true@ prevents a volume that is included in the
-- block device mapping of the AMI from being mapped to the specified
-- device name at launch.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the
-- EC2 health check. In that case, Amazon EC2 Auto Scaling launches
-- replacement instances.
--
-- 'virtualName', 'blockDeviceMapping_virtualName' - The name of the instance store volume (virtual device) to attach to an
-- instance at launch. The name must be in the form ephemeral/X/ where /X/
-- is a number starting from zero (0), for example, @ephemeral0@.
--
-- 'deviceName', 'blockDeviceMapping_deviceName' - The device name assigned to the volume (for example, @\/dev\/sdh@ or
-- @xvdh@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device naming on Linux instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- To define a block device mapping, set the device name and exactly one of
-- the following properties: @Ebs@, @NoDevice@, or @VirtualName@.
newBlockDeviceMapping ::
  -- | 'deviceName'
  Prelude.Text ->
  BlockDeviceMapping
newBlockDeviceMapping pDeviceName_ =
  BlockDeviceMapping'
    { ebs = Prelude.Nothing,
      noDevice = Prelude.Nothing,
      virtualName = Prelude.Nothing,
      deviceName = pDeviceName_
    }

-- | Information to attach an EBS volume to an instance at launch.
blockDeviceMapping_ebs :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Ebs)
blockDeviceMapping_ebs = Lens.lens (\BlockDeviceMapping' {ebs} -> ebs) (\s@BlockDeviceMapping' {} a -> s {ebs = a} :: BlockDeviceMapping)

-- | Setting this value to @true@ prevents a volume that is included in the
-- block device mapping of the AMI from being mapped to the specified
-- device name at launch.
--
-- If @NoDevice@ is @true@ for the root device, instances might fail the
-- EC2 health check. In that case, Amazon EC2 Auto Scaling launches
-- replacement instances.
blockDeviceMapping_noDevice :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Prelude.Bool)
blockDeviceMapping_noDevice = Lens.lens (\BlockDeviceMapping' {noDevice} -> noDevice) (\s@BlockDeviceMapping' {} a -> s {noDevice = a} :: BlockDeviceMapping)

-- | The name of the instance store volume (virtual device) to attach to an
-- instance at launch. The name must be in the form ephemeral/X/ where /X/
-- is a number starting from zero (0), for example, @ephemeral0@.
blockDeviceMapping_virtualName :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Prelude.Text)
blockDeviceMapping_virtualName = Lens.lens (\BlockDeviceMapping' {virtualName} -> virtualName) (\s@BlockDeviceMapping' {} a -> s {virtualName = a} :: BlockDeviceMapping)

-- | The device name assigned to the volume (for example, @\/dev\/sdh@ or
-- @xvdh@). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/device_naming.html Device naming on Linux instances>
-- in the /Amazon EC2 User Guide for Linux Instances/.
--
-- To define a block device mapping, set the device name and exactly one of
-- the following properties: @Ebs@, @NoDevice@, or @VirtualName@.
blockDeviceMapping_deviceName :: Lens.Lens' BlockDeviceMapping Prelude.Text
blockDeviceMapping_deviceName = Lens.lens (\BlockDeviceMapping' {deviceName} -> deviceName) (\s@BlockDeviceMapping' {} a -> s {deviceName = a} :: BlockDeviceMapping)

instance Data.FromXML BlockDeviceMapping where
  parseXML x =
    BlockDeviceMapping'
      Prelude.<$> (x Data..@? "Ebs")
      Prelude.<*> (x Data..@? "NoDevice")
      Prelude.<*> (x Data..@? "VirtualName")
      Prelude.<*> (x Data..@ "DeviceName")

instance Prelude.Hashable BlockDeviceMapping where
  hashWithSalt _salt BlockDeviceMapping' {..} =
    _salt
      `Prelude.hashWithSalt` ebs
      `Prelude.hashWithSalt` noDevice
      `Prelude.hashWithSalt` virtualName
      `Prelude.hashWithSalt` deviceName

instance Prelude.NFData BlockDeviceMapping where
  rnf BlockDeviceMapping' {..} =
    Prelude.rnf ebs `Prelude.seq`
      Prelude.rnf noDevice `Prelude.seq`
        Prelude.rnf virtualName `Prelude.seq`
          Prelude.rnf deviceName

instance Data.ToQuery BlockDeviceMapping where
  toQuery BlockDeviceMapping' {..} =
    Prelude.mconcat
      [ "Ebs" Data.=: ebs,
        "NoDevice" Data.=: noDevice,
        "VirtualName" Data.=: virtualName,
        "DeviceName" Data.=: deviceName
      ]
