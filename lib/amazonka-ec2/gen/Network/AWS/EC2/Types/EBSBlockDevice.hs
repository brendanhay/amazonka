-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSBlockDevice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSBlockDevice
  ( EBSBlockDevice (..),

    -- * Smart constructor
    mkEBSBlockDevice,

    -- * Lenses
    ebdDeleteOnTermination,
    ebdVolumeSize,
    ebdIOPS,
    ebdEncrypted,
    ebdKMSKeyId,
    ebdVolumeType,
    ebdSnapshotId,
  )
where

import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a block device for an EBS volume.
--
-- /See:/ 'mkEBSBlockDevice' smart constructor.
data EBSBlockDevice = EBSBlockDevice'
  { deleteOnTermination ::
      Lude.Maybe Lude.Bool,
    volumeSize :: Lude.Maybe Lude.Int,
    iops :: Lude.Maybe Lude.Int,
    encrypted :: Lude.Maybe Lude.Bool,
    kmsKeyId :: Lude.Maybe Lude.Text,
    volumeType :: Lude.Maybe VolumeType,
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSBlockDevice' with the minimum fields required to make a request.
--
-- * 'deleteOnTermination' - Indicates whether the EBS volume is deleted on instance termination. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination> in the Amazon Elastic Compute Cloud User Guide.
-- * 'encrypted' - Indicates whether the encryption state of an EBS volume is changed while being restored from a backing snapshot. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- In no case can you remove encryption from an encrypted volume.
-- Encrypted volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
-- This parameter is not returned by .
-- * 'iops' - The number of I/O operations per second (IOPS) that the volume supports. For @io1@ and @io2@ volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes in most Regions. Maximum @io1@ and @io2@ IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
-- * 'kmsKeyId' - Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer managed CMK under which the EBS volume is encrypted.
--
-- This parameter is only supported on @BlockDeviceMapping@ objects called by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
-- * 'snapshotId' - The ID of the snapshot.
-- * 'volumeSize' - The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ and @io2@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- * 'volumeType' - The volume type. If you set the type to @io1@ or @io2@ , you must also specify the __Iops__ parameter. If you set the type to @gp2@ , @st1@ , @sc1@ , or @standard@ , you must omit the __Iops__ parameter.
--
-- Default: @gp2@
mkEBSBlockDevice ::
  EBSBlockDevice
mkEBSBlockDevice =
  EBSBlockDevice'
    { deleteOnTermination = Lude.Nothing,
      volumeSize = Lude.Nothing,
      iops = Lude.Nothing,
      encrypted = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      volumeType = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | Indicates whether the EBS volume is deleted on instance termination. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/terminating-instances.html#preserving-volumes-on-termination Preserving Amazon EBS volumes on instance termination> in the Amazon Elastic Compute Cloud User Guide.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdDeleteOnTermination :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Bool)
ebdDeleteOnTermination = Lens.lens (deleteOnTermination :: EBSBlockDevice -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: EBSBlockDevice)
{-# DEPRECATED ebdDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The size of the volume, in GiB.
--
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- Constraints: 1-16384 for General Purpose SSD (@gp2@ ), 4-16384 for Provisioned IOPS SSD (@io1@ and @io2@ ), 500-16384 for Throughput Optimized HDD (@st1@ ), 500-16384 for Cold HDD (@sc1@ ), and 1-1024 for Magnetic (@standard@ ) volumes. If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
--
-- /Note:/ Consider using 'volumeSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeSize :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Int)
ebdVolumeSize = Lens.lens (volumeSize :: EBSBlockDevice -> Lude.Maybe Lude.Int) (\s a -> s {volumeSize = a} :: EBSBlockDevice)
{-# DEPRECATED ebdVolumeSize "Use generic-lens or generic-optics with 'volumeSize' instead." #-}

-- | The number of I/O operations per second (IOPS) that the volume supports. For @io1@ and @io2@ volumes, this represents the number of IOPS that are provisioned for the volume. For @gp2@ volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes in most Regions. Maximum @io1@ and @io2@ IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> in the /Amazon Elastic Compute Cloud User Guide/ .
-- Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdIOPS :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Int)
ebdIOPS = Lens.lens (iops :: EBSBlockDevice -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: EBSBlockDevice)
{-# DEPRECATED ebdIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | Indicates whether the encryption state of an EBS volume is changed while being restored from a backing snapshot. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-parameters Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- In no case can you remove encryption from an encrypted volume.
-- Encrypted volumes can only be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
-- This parameter is not returned by .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdEncrypted :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Bool)
ebdEncrypted = Lens.lens (encrypted :: EBSBlockDevice -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: EBSBlockDevice)
{-# DEPRECATED ebdEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | Identifier (key ID, key alias, ID ARN, or alias ARN) for a customer managed CMK under which the EBS volume is encrypted.
--
-- This parameter is only supported on @BlockDeviceMapping@ objects called by <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> , <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotFleet.html RequestSpotFleet> , and <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestSpotInstances.html RequestSpotInstances> .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdKMSKeyId :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Text)
ebdKMSKeyId = Lens.lens (kmsKeyId :: EBSBlockDevice -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: EBSBlockDevice)
{-# DEPRECATED ebdKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The volume type. If you set the type to @io1@ or @io2@ , you must also specify the __Iops__ parameter. If you set the type to @gp2@ , @st1@ , @sc1@ , or @standard@ , you must omit the __Iops__ parameter.
--
-- Default: @gp2@
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdVolumeType :: Lens.Lens' EBSBlockDevice (Lude.Maybe VolumeType)
ebdVolumeType = Lens.lens (volumeType :: EBSBlockDevice -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: EBSBlockDevice)
{-# DEPRECATED ebdVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The ID of the snapshot.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ebdSnapshotId :: Lens.Lens' EBSBlockDevice (Lude.Maybe Lude.Text)
ebdSnapshotId = Lens.lens (snapshotId :: EBSBlockDevice -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: EBSBlockDevice)
{-# DEPRECATED ebdSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML EBSBlockDevice where
  parseXML x =
    EBSBlockDevice'
      Lude.<$> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "volumeSize")
      Lude.<*> (x Lude..@? "iops")
      Lude.<*> (x Lude..@? "encrypted")
      Lude.<*> (x Lude..@? "KmsKeyId")
      Lude.<*> (x Lude..@? "volumeType")
      Lude.<*> (x Lude..@? "snapshotId")

instance Lude.ToQuery EBSBlockDevice where
  toQuery EBSBlockDevice' {..} =
    Lude.mconcat
      [ "DeleteOnTermination" Lude.=: deleteOnTermination,
        "VolumeSize" Lude.=: volumeSize,
        "Iops" Lude.=: iops,
        "Encrypted" Lude.=: encrypted,
        "KmsKeyId" Lude.=: kmsKeyId,
        "VolumeType" Lude.=: volumeType,
        "SnapshotId" Lude.=: snapshotId
      ]
