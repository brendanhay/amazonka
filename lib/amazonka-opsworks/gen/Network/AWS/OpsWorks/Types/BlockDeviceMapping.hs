{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.BlockDeviceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.BlockDeviceMapping
  ( BlockDeviceMapping (..),

    -- * Smart constructor
    mkBlockDeviceMapping,

    -- * Lenses
    bdmVirtualName,
    bdmNoDevice,
    bdmEBS,
    bdmDeviceName,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types.EBSBlockDevice
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device mapping. This data type maps directly to the Amazon EC2 <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> data type.
--
-- /See:/ 'mkBlockDeviceMapping' smart constructor.
data BlockDeviceMapping = BlockDeviceMapping'
  { -- | The virtual device name. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> .
    virtualName :: Lude.Maybe Lude.Text,
    -- | Suppresses the specified device included in the AMI's block device mapping.
    noDevice :: Lude.Maybe Lude.Text,
    -- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume when the instance is launched.
    ebs :: Lude.Maybe EBSBlockDevice,
    -- | The device name that is exposed to the instance, such as @/dev/sdh@ . For the root device, you can use the explicit device name or you can set this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the correct device name.
    deviceName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BlockDeviceMapping' with the minimum fields required to make a request.
--
-- * 'virtualName' - The virtual device name. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> .
-- * 'noDevice' - Suppresses the specified device included in the AMI's block device mapping.
-- * 'ebs' - An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume when the instance is launched.
-- * 'deviceName' - The device name that is exposed to the instance, such as @/dev/sdh@ . For the root device, you can use the explicit device name or you can set this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the correct device name.
mkBlockDeviceMapping ::
  BlockDeviceMapping
mkBlockDeviceMapping =
  BlockDeviceMapping'
    { virtualName = Lude.Nothing,
      noDevice = Lude.Nothing,
      ebs = Lude.Nothing,
      deviceName = Lude.Nothing
    }

-- | The virtual device name. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_BlockDeviceMapping.html BlockDeviceMapping> .
--
-- /Note:/ Consider using 'virtualName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmVirtualName :: Lens.Lens' BlockDeviceMapping (Lude.Maybe Lude.Text)
bdmVirtualName = Lens.lens (virtualName :: BlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {virtualName = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmVirtualName "Use generic-lens or generic-optics with 'virtualName' instead." #-}

-- | Suppresses the specified device included in the AMI's block device mapping.
--
-- /Note:/ Consider using 'noDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmNoDevice :: Lens.Lens' BlockDeviceMapping (Lude.Maybe Lude.Text)
bdmNoDevice = Lens.lens (noDevice :: BlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {noDevice = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmNoDevice "Use generic-lens or generic-optics with 'noDevice' instead." #-}

-- | An @EBSBlockDevice@ that defines how to configure an Amazon EBS volume when the instance is launched.
--
-- /Note:/ Consider using 'ebs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmEBS :: Lens.Lens' BlockDeviceMapping (Lude.Maybe EBSBlockDevice)
bdmEBS = Lens.lens (ebs :: BlockDeviceMapping -> Lude.Maybe EBSBlockDevice) (\s a -> s {ebs = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmEBS "Use generic-lens or generic-optics with 'ebs' instead." #-}

-- | The device name that is exposed to the instance, such as @/dev/sdh@ . For the root device, you can use the explicit device name or you can set this parameter to @ROOT_DEVICE@ and AWS OpsWorks Stacks will provide the correct device name.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdmDeviceName :: Lens.Lens' BlockDeviceMapping (Lude.Maybe Lude.Text)
bdmDeviceName = Lens.lens (deviceName :: BlockDeviceMapping -> Lude.Maybe Lude.Text) (\s a -> s {deviceName = a} :: BlockDeviceMapping)
{-# DEPRECATED bdmDeviceName "Use generic-lens or generic-optics with 'deviceName' instead." #-}

instance Lude.FromJSON BlockDeviceMapping where
  parseJSON =
    Lude.withObject
      "BlockDeviceMapping"
      ( \x ->
          BlockDeviceMapping'
            Lude.<$> (x Lude..:? "VirtualName")
            Lude.<*> (x Lude..:? "NoDevice")
            Lude.<*> (x Lude..:? "Ebs")
            Lude.<*> (x Lude..:? "DeviceName")
      )

instance Lude.ToJSON BlockDeviceMapping where
  toJSON BlockDeviceMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("VirtualName" Lude..=) Lude.<$> virtualName,
            ("NoDevice" Lude..=) Lude.<$> noDevice,
            ("Ebs" Lude..=) Lude.<$> ebs,
            ("DeviceName" Lude..=) Lude.<$> deviceName
          ]
      )
