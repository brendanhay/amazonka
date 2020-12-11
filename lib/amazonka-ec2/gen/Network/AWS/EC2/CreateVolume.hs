{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EBS volume that can be attached to an instance in the same Availability Zone. The volume is created in the regional endpoint that you send the HTTP request to. For more information see <https://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
-- You can create a new empty volume or restore a volume from an EBS snapshot. Any AWS Marketplace product codes from the snapshot are propagated to the volume.
-- You can create encrypted volumes. Encrypted volumes must be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are also automatically encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
-- You can tag your volumes during creation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources> in the /Amazon Elastic Compute Cloud User Guide/ .
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html Creating an Amazon EBS volume> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateVolume
  ( -- * Creating a request
    CreateVolume (..),
    mkCreateVolume,

    -- ** Request lenses
    cvvMultiAttachEnabled,
    cvvSize,
    cvvIOPS,
    cvvOutpostARN,
    cvvEncrypted,
    cvvTagSpecifications,
    cvvKMSKeyId,
    cvvVolumeType,
    cvvDryRun,
    cvvSnapshotId,
    cvvAvailabilityZone,

    -- * Destructuring the response
    Volume (..),
    mkVolume,

    -- ** Response lenses
    vFastRestored,
    vMultiAttachEnabled,
    vAttachments,
    vIOPS,
    vOutpostARN,
    vKMSKeyId,
    vTags,
    vAvailabilityZone,
    vCreateTime,
    vEncrypted,
    vSize,
    vSnapshotId,
    vState,
    vVolumeId,
    vVolumeType,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVolume' smart constructor.
data CreateVolume = CreateVolume'
  { multiAttachEnabled ::
      Lude.Maybe Lude.Bool,
    size :: Lude.Maybe Lude.Int,
    iops :: Lude.Maybe Lude.Int,
    outpostARN :: Lude.Maybe Lude.Text,
    encrypted :: Lude.Maybe Lude.Bool,
    tagSpecifications :: Lude.Maybe [TagSpecification],
    kmsKeyId :: Lude.Maybe Lude.Text,
    volumeType :: Lude.Maybe VolumeType,
    dryRun :: Lude.Maybe Lude.Bool,
    snapshotId :: Lude.Maybe Lude.Text,
    availabilityZone :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVolume' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Availability Zone in which to create the volume.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'encrypted' - Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
-- * 'iops' - The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
-- * 'kmsKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
-- * 'multiAttachEnabled' - Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'size' - The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size.
--
-- Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ and @io2@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- * 'snapshotId' - The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
-- * 'tagSpecifications' - The tags to apply to the volume during creation.
-- * 'volumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
--
-- Default: @gp2@
mkCreateVolume ::
  -- | 'availabilityZone'
  Lude.Text ->
  CreateVolume
mkCreateVolume pAvailabilityZone_ =
  CreateVolume'
    { multiAttachEnabled = Lude.Nothing,
      size = Lude.Nothing,
      iops = Lude.Nothing,
      outpostARN = Lude.Nothing,
      encrypted = Lude.Nothing,
      tagSpecifications = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      volumeType = Lude.Nothing,
      dryRun = Lude.Nothing,
      snapshotId = Lude.Nothing,
      availabilityZone = pAvailabilityZone_
    }

-- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'multiAttachEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvMultiAttachEnabled :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Bool)
cvvMultiAttachEnabled = Lens.lens (multiAttachEnabled :: CreateVolume -> Lude.Maybe Lude.Bool) (\s a -> s {multiAttachEnabled = a} :: CreateVolume)
{-# DEPRECATED cvvMultiAttachEnabled "Use generic-lens or generic-optics with 'multiAttachEnabled' instead." #-}

-- | The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size.
--
-- Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ and @io2@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvSize :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Int)
cvvSize = Lens.lens (size :: CreateVolume -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: CreateVolume)
{-# DEPRECATED cvvSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvIOPS :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Int)
cvvIOPS = Lens.lens (iops :: CreateVolume -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: CreateVolume)
{-# DEPRECATED cvvIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvOutpostARN :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Text)
cvvOutpostARN = Lens.lens (outpostARN :: CreateVolume -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: CreateVolume)
{-# DEPRECATED cvvOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvEncrypted :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Bool)
cvvEncrypted = Lens.lens (encrypted :: CreateVolume -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: CreateVolume)
{-# DEPRECATED cvvEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The tags to apply to the volume during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvTagSpecifications :: Lens.Lens' CreateVolume (Lude.Maybe [TagSpecification])
cvvTagSpecifications = Lens.lens (tagSpecifications :: CreateVolume -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateVolume)
{-# DEPRECATED cvvTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ .
--
-- You can specify the CMK using any of the following:
--
--     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Key alias. For example, alias/ExampleAlias.
--
--
--     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.
--
--
--     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias.
--
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvKMSKeyId :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Text)
cvvKMSKeyId = Lens.lens (kmsKeyId :: CreateVolume -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateVolume)
{-# DEPRECATED cvvKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
--
-- Default: @gp2@
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvVolumeType :: Lens.Lens' CreateVolume (Lude.Maybe VolumeType)
cvvVolumeType = Lens.lens (volumeType :: CreateVolume -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: CreateVolume)
{-# DEPRECATED cvvVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvDryRun :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Bool)
cvvDryRun = Lens.lens (dryRun :: CreateVolume -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVolume)
{-# DEPRECATED cvvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvSnapshotId :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Text)
cvvSnapshotId = Lens.lens (snapshotId :: CreateVolume -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateVolume)
{-# DEPRECATED cvvSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The Availability Zone in which to create the volume.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvvAvailabilityZone :: Lens.Lens' CreateVolume Lude.Text
cvvAvailabilityZone = Lens.lens (availabilityZone :: CreateVolume -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateVolume)
{-# DEPRECATED cvvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.AWSRequest CreateVolume where
  type Rs CreateVolume = Volume
  request = Req.postQuery ec2Service
  response = Res.receiveXML (\s h x -> Lude.parseXML x)

instance Lude.ToHeaders CreateVolume where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateVolume where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateVolume where
  toQuery CreateVolume' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateVolume" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "MultiAttachEnabled" Lude.=: multiAttachEnabled,
        "Size" Lude.=: size,
        "Iops" Lude.=: iops,
        "OutpostArn" Lude.=: outpostARN,
        "Encrypted" Lude.=: encrypted,
        Lude.toQuery
          (Lude.toQueryList "TagSpecification" Lude.<$> tagSpecifications),
        "KmsKeyId" Lude.=: kmsKeyId,
        "VolumeType" Lude.=: volumeType,
        "DryRun" Lude.=: dryRun,
        "SnapshotId" Lude.=: snapshotId,
        "AvailabilityZone" Lude.=: availabilityZone
      ]
