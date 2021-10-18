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
-- Module      : Network.AWS.OpsWorks.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.BlockDeviceMapping where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.EbsBlockDevice
import qualified Network.AWS.Prelude as Prelude

-- | Describes a block device mapping. This data type maps directly to the
-- Amazon EC2
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>
-- data type.
--
-- /See:/ 'newBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { -- | The virtual device name. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>.
    virtualName :: Prelude.Maybe Prelude.Text,
    -- | Suppresses the specified device included in the AMI\'s block device
    -- mapping.
    noDevice :: Prelude.Maybe Prelude.Text,
    -- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume
    -- when the instance is launched.
    ebs :: Prelude.Maybe EbsBlockDevice,
    -- | The device name that is exposed to the instance, such as @\/dev\/sdh@.
    -- For the root device, you can use the explicit device name or you can set
    -- this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the
    -- correct device name.
    deviceName :: Prelude.Maybe Prelude.Text
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
-- 'virtualName', 'blockDeviceMapping_virtualName' - The virtual device name. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>.
--
-- 'noDevice', 'blockDeviceMapping_noDevice' - Suppresses the specified device included in the AMI\'s block device
-- mapping.
--
-- 'ebs', 'blockDeviceMapping_ebs' - An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume
-- when the instance is launched.
--
-- 'deviceName', 'blockDeviceMapping_deviceName' - The device name that is exposed to the instance, such as @\/dev\/sdh@.
-- For the root device, you can use the explicit device name or you can set
-- this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the
-- correct device name.
newBlockDeviceMapping ::
  BlockDeviceMapping
newBlockDeviceMapping =
  BlockDeviceMapping'
    { virtualName = Prelude.Nothing,
      noDevice = Prelude.Nothing,
      ebs = Prelude.Nothing,
      deviceName = Prelude.Nothing
    }

-- | The virtual device name. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping>.
blockDeviceMapping_virtualName :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Prelude.Text)
blockDeviceMapping_virtualName = Lens.lens (\BlockDeviceMapping' {virtualName} -> virtualName) (\s@BlockDeviceMapping' {} a -> s {virtualName = a} :: BlockDeviceMapping)

-- | Suppresses the specified device included in the AMI\'s block device
-- mapping.
blockDeviceMapping_noDevice :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Prelude.Text)
blockDeviceMapping_noDevice = Lens.lens (\BlockDeviceMapping' {noDevice} -> noDevice) (\s@BlockDeviceMapping' {} a -> s {noDevice = a} :: BlockDeviceMapping)

-- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume
-- when the instance is launched.
blockDeviceMapping_ebs :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe EbsBlockDevice)
blockDeviceMapping_ebs = Lens.lens (\BlockDeviceMapping' {ebs} -> ebs) (\s@BlockDeviceMapping' {} a -> s {ebs = a} :: BlockDeviceMapping)

-- | The device name that is exposed to the instance, such as @\/dev\/sdh@.
-- For the root device, you can use the explicit device name or you can set
-- this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the
-- correct device name.
blockDeviceMapping_deviceName :: Lens.Lens' BlockDeviceMapping (Prelude.Maybe Prelude.Text)
blockDeviceMapping_deviceName = Lens.lens (\BlockDeviceMapping' {deviceName} -> deviceName) (\s@BlockDeviceMapping' {} a -> s {deviceName = a} :: BlockDeviceMapping)

instance Core.FromJSON BlockDeviceMapping where
  parseJSON =
    Core.withObject
      "BlockDeviceMapping"
      ( \x ->
          BlockDeviceMapping'
            Prelude.<$> (x Core..:? "VirtualName")
            Prelude.<*> (x Core..:? "NoDevice")
            Prelude.<*> (x Core..:? "Ebs")
            Prelude.<*> (x Core..:? "DeviceName")
      )

instance Prelude.Hashable BlockDeviceMapping

instance Prelude.NFData BlockDeviceMapping

instance Core.ToJSON BlockDeviceMapping where
  toJSON BlockDeviceMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VirtualName" Core..=) Prelude.<$> virtualName,
            ("NoDevice" Core..=) Prelude.<$> noDevice,
            ("Ebs" Core..=) Prelude.<$> ebs,
            ("DeviceName" Core..=) Prelude.<$> deviceName
          ]
      )
