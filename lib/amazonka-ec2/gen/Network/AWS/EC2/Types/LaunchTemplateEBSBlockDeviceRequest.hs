{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateEBSBlockDeviceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateEBSBlockDeviceRequest
  ( LaunchTemplateEBSBlockDeviceRequest (..),

    -- * Smart constructor
    mkLaunchTemplateEBSBlockDeviceRequest,

    -- * Lenses
    ltebdrDeleteOnTermination,
    ltebdrVolumeSize,
    ltebdrIOPS,
    ltebdrEncrypted,
    ltebdrKMSKeyId,
    ltebdrVolumeType,
    ltebdrSnapshotId,
  )
where

import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The parameters for a block device for an EBS volume.
--
-- /See:/ 'mkLaunchTemplateEBSBlockDeviceRequest' smart constructor.
data LaunchTemplateEBSBlockDeviceRequest = LaunchTemplateEBSBlockDeviceRequest'
  { -- | Indicates whether the EBS volume is deleted on instance termination.
    deleteOnTermination :: Lude.Maybe Lude.Bool,
    -- | The size of the volume, in GiB.
    --
    -- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
    volumeSize :: Lude.Maybe Lude.Int,
    -- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
    iops :: Lude.Maybe Lude.Int,
    -- | Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The ARN of the symmetric AWS Key Management Service (AWS KMS) CMK used for encryption.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The volume type.
    volumeType :: Lude.Maybe VolumeType,
    -- | The ID of the snapshot.
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LaunchTemplateEBSBlockDeviceRequest' with the minimum fields required to make a request.
--
-- * 'deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination.
-- * 'volumeSize' - The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- * 'iops' - The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
-- * 'encrypted' - Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
-- * 'kmsKeyId' - The ARN of the symmetric AWS Key Management Service (AWS KMS) CMK used for encryption.
-- * 'volumeType' - The volume type.
-- * 'snapshotId' - The ID of the snapshot.
mkLaunchTemplateEBSBlockDeviceRequest ::
  LaunchTemplateEBSBlockDeviceRequest
mkLaunchTemplateEBSBlockDeviceRequest =
  LaunchTemplateEBSBlockDeviceRequest'
    { deleteOnTermination =
        Lude.Nothing,
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
ltebdrDeleteOnTermination :: Lens.Lens' LaunchTemplateEBSBlockDeviceRequest (Lude.Maybe Lude.Bool)
ltebdrDeleteOnTermination = Lens.lens (deleteOnTermination :: LaunchTemplateEBSBlockDeviceRequest -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: LaunchTemplateEBSBlockDeviceRequest)
{-# DEPRECATED ltebdrDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrVolumeSize :: Lens.Lens' LaunchTemplateEBSBlockDeviceRequest (Lude.Maybe Lude.Int)
ltebdrVolumeSize = Lens.lens (volumeSize :: LaunchTemplateEBSBlockDeviceRequest -> Lude.Maybe Lude.Int) (\s a -> s {volumeSize = a} :: LaunchTemplateEBSBlockDeviceRequest)
{-# DEPRECATED ltebdrVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrIOPS :: Lens.Lens' LaunchTemplateEBSBlockDeviceRequest (Lude.Maybe Lude.Int)
ltebdrIOPS = Lens.lens (iops :: LaunchTemplateEBSBlockDeviceRequest -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: LaunchTemplateEBSBlockDeviceRequest)
{-# DEPRECATED ltebdrIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Indicates whether the EBS volume is encrypted. Encrypted volumes can only be attached to instances that support Amazon EBS encryption. If you are creating a volume from a snapshot, you can't specify an encryption value.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrEncrypted :: Lens.Lens' LaunchTemplateEBSBlockDeviceRequest (Lude.Maybe Lude.Bool)
ltebdrEncrypted = Lens.lens (encrypted :: LaunchTemplateEBSBlockDeviceRequest -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: LaunchTemplateEBSBlockDeviceRequest)
{-# DEPRECATED ltebdrEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The ARN of the symmetric AWS Key Management Service (AWS KMS) CMK used for encryption.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrKMSKeyId :: Lens.Lens' LaunchTemplateEBSBlockDeviceRequest (Lude.Maybe Lude.Text)
ltebdrKMSKeyId = Lens.lens (kmsKeyId :: LaunchTemplateEBSBlockDeviceRequest -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: LaunchTemplateEBSBlockDeviceRequest)
{-# DEPRECATED ltebdrKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The volume type.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrVolumeType :: Lens.Lens' LaunchTemplateEBSBlockDeviceRequest (Lude.Maybe VolumeType)
ltebdrVolumeType = Lens.lens (volumeType :: LaunchTemplateEBSBlockDeviceRequest -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: LaunchTemplateEBSBlockDeviceRequest)
{-# DEPRECATED ltebdrVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltebdrSnapshotId :: Lens.Lens' LaunchTemplateEBSBlockDeviceRequest (Lude.Maybe Lude.Text)
ltebdrSnapshotId = Lens.lens (snapshotId :: LaunchTemplateEBSBlockDeviceRequest -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: LaunchTemplateEBSBlockDeviceRequest)
{-# DEPRECATED ltebdrSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.ToQuery LaunchTemplateEBSBlockDeviceRequest where
  toQuery LaunchTemplateEBSBlockDeviceRequest' {..} =
    Lude.mconcat
      [ "DeleteOnTermination" Lude.=: deleteOnTermination,
        "VolumeSize" Lude.=: volumeSize,
        "Iops" Lude.=: iops,
        "Encrypted" Lude.=: encrypted,
        "KmsKeyId" Lude.=: kmsKeyId,
        "VolumeType" Lude.=: volumeType,
        "SnapshotId" Lude.=: snapshotId
      ]
