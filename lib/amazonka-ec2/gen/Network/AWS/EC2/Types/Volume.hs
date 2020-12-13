{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Volume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Volume
  ( Volume (..),

    -- * Smart constructor
    mkVolume,

    -- * Lenses
    vFastRestored,
    vState,
    vMultiAttachEnabled,
    vAttachments,
    vSize,
    vIOPS,
    vOutpostARN,
    vEncrypted,
    vKMSKeyId,
    vAvailabilityZone,
    vVolumeId,
    vVolumeType,
    vCreateTime,
    vTags,
    vSnapshotId,
  )
where

import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VolumeAttachment
import Network.AWS.EC2.Types.VolumeState
import Network.AWS.EC2.Types.VolumeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a volume.
--
-- /See:/ 'mkVolume' smart constructor.
data Volume = Volume'
  { -- | Indicates whether the volume was created using fast snapshot restore.
    fastRestored :: Lude.Maybe Lude.Bool,
    -- | The volume state.
    state :: VolumeState,
    -- | Indicates whether Amazon EBS Multi-Attach is enabled.
    multiAttachEnabled :: Lude.Maybe Lude.Bool,
    -- | Information about the volume attachments.
    attachments :: Lude.Maybe [VolumeAttachment],
    -- | The size of the volume, in GiBs.
    size :: Lude.Int,
    -- | The number of I/O operations per second (IOPS) that the volume supports. For Provisioned IOPS SSD volumes, this represents the number of IOPS that are provisioned for the volume. For General Purpose SSD volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes, in most Regions. The maximum IOPS for @io1@ and @io2@ of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS.
    -- Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
    iops :: Lude.Maybe Lude.Int,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | Indicates whether the volume is encrypted.
    encrypted :: Lude.Bool,
    -- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the volume.
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The Availability Zone for the volume.
    availabilityZone :: Lude.Text,
    -- | The ID of the volume.
    volumeId :: Lude.Text,
    -- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
    volumeType :: VolumeType,
    -- | The time stamp when volume creation was initiated.
    createTime :: Lude.DateTime,
    -- | Any tags assigned to the volume.
    tags :: Lude.Maybe [Tag],
    -- | The snapshot from which the volume was created, if applicable.
    snapshotId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Volume' with the minimum fields required to make a request.
--
-- * 'fastRestored' - Indicates whether the volume was created using fast snapshot restore.
-- * 'state' - The volume state.
-- * 'multiAttachEnabled' - Indicates whether Amazon EBS Multi-Attach is enabled.
-- * 'attachments' - Information about the volume attachments.
-- * 'size' - The size of the volume, in GiBs.
-- * 'iops' - The number of I/O operations per second (IOPS) that the volume supports. For Provisioned IOPS SSD volumes, this represents the number of IOPS that are provisioned for the volume. For General Purpose SSD volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes, in most Regions. The maximum IOPS for @io1@ and @io2@ of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS.
-- Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'encrypted' - Indicates whether the volume is encrypted.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the volume.
-- * 'availabilityZone' - The Availability Zone for the volume.
-- * 'volumeId' - The ID of the volume.
-- * 'volumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
-- * 'createTime' - The time stamp when volume creation was initiated.
-- * 'tags' - Any tags assigned to the volume.
-- * 'snapshotId' - The snapshot from which the volume was created, if applicable.
mkVolume ::
  -- | 'state'
  VolumeState ->
  -- | 'size'
  Lude.Int ->
  -- | 'encrypted'
  Lude.Bool ->
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'volumeId'
  Lude.Text ->
  -- | 'volumeType'
  VolumeType ->
  -- | 'createTime'
  Lude.DateTime ->
  -- | 'snapshotId'
  Lude.Text ->
  Volume
mkVolume
  pState_
  pSize_
  pEncrypted_
  pAvailabilityZone_
  pVolumeId_
  pVolumeType_
  pCreateTime_
  pSnapshotId_ =
    Volume'
      { fastRestored = Lude.Nothing,
        state = pState_,
        multiAttachEnabled = Lude.Nothing,
        attachments = Lude.Nothing,
        size = pSize_,
        iops = Lude.Nothing,
        outpostARN = Lude.Nothing,
        encrypted = pEncrypted_,
        kmsKeyId = Lude.Nothing,
        availabilityZone = pAvailabilityZone_,
        volumeId = pVolumeId_,
        volumeType = pVolumeType_,
        createTime = pCreateTime_,
        tags = Lude.Nothing,
        snapshotId = pSnapshotId_
      }

-- | Indicates whether the volume was created using fast snapshot restore.
--
-- /Note:/ Consider using 'fastRestored' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vFastRestored :: Lens.Lens' Volume (Lude.Maybe Lude.Bool)
vFastRestored = Lens.lens (fastRestored :: Volume -> Lude.Maybe Lude.Bool) (\s a -> s {fastRestored = a} :: Volume)
{-# DEPRECATED vFastRestored "Use generic-lens or generic-optics with 'fastRestored' instead." #-}

-- | The volume state.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vState :: Lens.Lens' Volume VolumeState
vState = Lens.lens (state :: Volume -> VolumeState) (\s a -> s {state = a} :: Volume)
{-# DEPRECATED vState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Indicates whether Amazon EBS Multi-Attach is enabled.
--
-- /Note:/ Consider using 'multiAttachEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vMultiAttachEnabled :: Lens.Lens' Volume (Lude.Maybe Lude.Bool)
vMultiAttachEnabled = Lens.lens (multiAttachEnabled :: Volume -> Lude.Maybe Lude.Bool) (\s a -> s {multiAttachEnabled = a} :: Volume)
{-# DEPRECATED vMultiAttachEnabled "Use generic-lens or generic-optics with 'multiAttachEnabled' instead." #-}

-- | Information about the volume attachments.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vAttachments :: Lens.Lens' Volume (Lude.Maybe [VolumeAttachment])
vAttachments = Lens.lens (attachments :: Volume -> Lude.Maybe [VolumeAttachment]) (\s a -> s {attachments = a} :: Volume)
{-# DEPRECATED vAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The size of the volume, in GiBs.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSize :: Lens.Lens' Volume Lude.Int
vSize = Lens.lens (size :: Volume -> Lude.Int) (\s a -> s {size = a} :: Volume)
{-# DEPRECATED vSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The number of I/O operations per second (IOPS) that the volume supports. For Provisioned IOPS SSD volumes, this represents the number of IOPS that are provisioned for the volume. For General Purpose SSD volumes, this represents the baseline performance of the volume and the rate at which the volume accumulates I/O credits for bursting. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Constraints: Range is 100-16,000 IOPS for @gp2@ volumes and 100 to 64,000 IOPS for @io1@ and @io2@ volumes, in most Regions. The maximum IOPS for @io1@ and @io2@ of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS.
-- Condition: This parameter is required for requests to create @io1@ and @io2@ volumes; it is not used in requests to create @gp2@ , @st1@ , @sc1@ , or @standard@ volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vIOPS :: Lens.Lens' Volume (Lude.Maybe Lude.Int)
vIOPS = Lens.lens (iops :: Volume -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: Volume)
{-# DEPRECATED vIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vOutpostARN :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vOutpostARN = Lens.lens (outpostARN :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: Volume)
{-# DEPRECATED vOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Indicates whether the volume is encrypted.
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vEncrypted :: Lens.Lens' Volume Lude.Bool
vEncrypted = Lens.lens (encrypted :: Volume -> Lude.Bool) (\s a -> s {encrypted = a} :: Volume)
{-# DEPRECATED vEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Key Management Service (AWS KMS) customer master key (CMK) that was used to protect the volume encryption key for the volume.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vKMSKeyId :: Lens.Lens' Volume (Lude.Maybe Lude.Text)
vKMSKeyId = Lens.lens (kmsKeyId :: Volume -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: Volume)
{-# DEPRECATED vKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Availability Zone for the volume.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vAvailabilityZone :: Lens.Lens' Volume Lude.Text
vAvailabilityZone = Lens.lens (availabilityZone :: Volume -> Lude.Text) (\s a -> s {availabilityZone = a} :: Volume)
{-# DEPRECATED vAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vVolumeId :: Lens.Lens' Volume Lude.Text
vVolumeId = Lens.lens (volumeId :: Volume -> Lude.Text) (\s a -> s {volumeId = a} :: Volume)
{-# DEPRECATED vVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vVolumeType :: Lens.Lens' Volume VolumeType
vVolumeType = Lens.lens (volumeType :: Volume -> VolumeType) (\s a -> s {volumeType = a} :: Volume)
{-# DEPRECATED vVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | The time stamp when volume creation was initiated.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vCreateTime :: Lens.Lens' Volume Lude.DateTime
vCreateTime = Lens.lens (createTime :: Volume -> Lude.DateTime) (\s a -> s {createTime = a} :: Volume)
{-# DEPRECATED vCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | Any tags assigned to the volume.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vTags :: Lens.Lens' Volume (Lude.Maybe [Tag])
vTags = Lens.lens (tags :: Volume -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Volume)
{-# DEPRECATED vTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The snapshot from which the volume was created, if applicable.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSnapshotId :: Lens.Lens' Volume Lude.Text
vSnapshotId = Lens.lens (snapshotId :: Volume -> Lude.Text) (\s a -> s {snapshotId = a} :: Volume)
{-# DEPRECATED vSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.FromXML Volume where
  parseXML x =
    Volume'
      Lude.<$> (x Lude..@? "fastRestored")
      Lude.<*> (x Lude..@ "status")
      Lude.<*> (x Lude..@? "multiAttachEnabled")
      Lude.<*> ( x Lude..@? "attachmentSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "size")
      Lude.<*> (x Lude..@? "iops")
      Lude.<*> (x Lude..@? "outpostArn")
      Lude.<*> (x Lude..@ "encrypted")
      Lude.<*> (x Lude..@? "kmsKeyId")
      Lude.<*> (x Lude..@ "availabilityZone")
      Lude.<*> (x Lude..@ "volumeId")
      Lude.<*> (x Lude..@ "volumeType")
      Lude.<*> (x Lude..@ "createTime")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@ "snapshotId")
