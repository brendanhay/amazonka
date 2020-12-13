{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEBSBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateEBSBlockDevice
  ( LaunchTemplateEBSBlockDevice (..),

    -- * Smart constructor
    mkLaunchTemplateEBSBlockDevice,

    -- * Lenses
    ltebdDeleteOnTermination,
    ltebdVolumeSize,
    ltebdIOPS,
    ltebdEncrypted,
    ltebdKMSKeyId,
    ltebdVolumeType,
    ltebdSnapshotId,
  )
where

import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device for an EBS volume.
--
-- /See:/ 'mkLaunchTemplateEBSBlockDevice' smart constructor.
data LaunchTemplateEBSBlockDevice = LaunchTemplateEBSBlockDevice'
  { -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Lude.Maybe Lude.Bool,
    -- | The size of the volume, in GiB.
    volumeSize :: Lude.Maybe Lude.Int,
    -- | The number of I/O operations per second (IOPS) that the volume supports.
    iops :: Lude.Maybe Lude.Int,
    -- | Indicates whether the EBS volume is encrypted.
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The volume type.
    volumeType :: Lude.Maybe VolumeType,
    -- | The ID of the snapshot.
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateEBSBlockDevice' with the minimum fields required to make a request.
--
-- * 'deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
-- * 'volumeSize' - The size of the volume, in GiB.
-- * 'iops' - The number of I/O operations per second (IOPS) that the volume supports.
-- * 'encrypted' - Indicates whether the EBS volume is encrypted.
-- * 'kmsKeyId' - The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
-- * 'volumeType' - The volume type.
-- * 'snapshotId' - The ID of the snapshot.
mkLaunchTemplateEBSBlockDevice ::
  LaunchTemplateEBSBlockDevice
mkLaunchTemplateEBSBlockDevice =
  LaunchTemplateEBSBlockDevice'
    { deleteOnTermination = Lude.Nothing,
      volumeSize = Lude.Nothing,
      iops = Lude.Nothing,
      encrypted = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      volumeType = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdDeleteOnTermination :: Lens.Lens' LaunchTemplateEBSBlockDevice (Lude.Maybe Lude.Bool)
ltebdDeleteOnTermination = Lens.lens (deleteOnTermination :: LaunchTemplateEBSBlockDevice -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: LaunchTemplateEBSBlockDevice)
{-# DEPRECATED ltebdDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The size of the volume, in GiB.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdVolumeSize :: Lens.Lens' LaunchTemplateEBSBlockDevice (Lude.Maybe Lude.Int)
ltebdVolumeSize = Lens.lens (volumeSize :: LaunchTemplateEBSBlockDevice -> Lude.Maybe Lude.Int) (\s a -> s {volumeSize = a} :: LaunchTemplateEBSBlockDevice)
{-# DEPRECATED ltebdVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The number of I/O operations per second (IOPS) that the volume supports.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdIOPS :: Lens.Lens' LaunchTemplateEBSBlockDevice (Lude.Maybe Lude.Int)
ltebdIOPS = Lens.lens (iops :: LaunchTemplateEBSBlockDevice -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: LaunchTemplateEBSBlockDevice)
{-# DEPRECATED ltebdIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Indicates whether the EBS volume is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdEncrypted :: Lens.Lens' LaunchTemplateEBSBlockDevice (Lude.Maybe Lude.Bool)
ltebdEncrypted = Lens.lens (encrypted :: LaunchTemplateEBSBlockDevice -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: LaunchTemplateEBSBlockDevice)
{-# DEPRECATED ltebdEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The ARN of the AWS Key Management Service (AWS KMS) CMK used for encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdKMSKeyId :: Lens.Lens' LaunchTemplateEBSBlockDevice (Lude.Maybe Lude.Text)
ltebdKMSKeyId = Lens.lens (kmsKeyId :: LaunchTemplateEBSBlockDevice -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: LaunchTemplateEBSBlockDevice)
{-# DEPRECATED ltebdKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The volume type.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdVolumeType :: Lens.Lens' LaunchTemplateEBSBlockDevice (Lude.Maybe VolumeType)
ltebdVolumeType = Lens.lens (volumeType :: LaunchTemplateEBSBlockDevice -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: LaunchTemplateEBSBlockDevice)
{-# DEPRECATED ltebdVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdSnapshotId :: Lens.Lens' LaunchTemplateEBSBlockDevice (Lude.Maybe Lude.Text)
ltebdSnapshotId = Lens.lens (snapshotId :: LaunchTemplateEBSBlockDevice -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: LaunchTemplateEBSBlockDevice)
{-# DEPRECATED ltebdSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML LaunchTemplateEBSBlockDevice where
  parseXML x =
    LaunchTemplateEBSBlockDevice'
      Lude.<$> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "volumeSize")
      Lude.<*> (x Lude..@? "iops")
      Lude.<*> (x Lude..@? "encrypted")
      Lude.<*> (x Lude..@? "kmsKeyId")
      Lude.<*> (x Lude..@? "volumeType")
      Lude.<*> (x Lude..@? "snapshotId")
