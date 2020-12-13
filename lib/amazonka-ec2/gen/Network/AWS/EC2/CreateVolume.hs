{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    cvMultiAttachEnabled,
    cvSize,
    cvIOPS,
    cvOutpostARN,
    cvEncrypted,
    cvTagSpecifications,
    cvKMSKeyId,
    cvAvailabilityZone,
    cvVolumeType,
    cvDryRun,
    cvSnapshotId,

    -- * Destructuring the response
    Volume (..),
    mkVolume,

    -- ** Response lenses
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateVolume' smart constructor.
data CreateVolume = CreateVolume'
  { -- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
    multiAttachEnabled :: Lude.Maybe Lude.Bool,
    -- | The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size.
    --
    -- Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ and @io2@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
    -- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
    size :: Lude.Maybe Lude.Int,
    -- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
    iops :: Lude.Maybe Lude.Int,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostARN :: Lude.Maybe Lude.Text,
    -- | Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
    encrypted :: Lude.Maybe Lude.Bool,
    -- | The tags to apply to the volume during creation.
    tagSpecifications :: Lude.Maybe [TagSpecification],
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
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | The Availability Zone in which to create the volume.
    availabilityZone :: Lude.Text,
    -- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
    --
    -- Default: @gp2@
    volumeType :: Lude.Maybe VolumeType,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
    snapshotId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateVolume' with the minimum fields required to make a request.
--
-- * 'multiAttachEnabled' - Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'size' - The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size.
--
-- Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ and @io2@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
-- * 'iops' - The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
-- * 'outpostARN' - The Amazon Resource Name (ARN) of the Outpost.
-- * 'encrypted' - Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
-- * 'tagSpecifications' - The tags to apply to the volume during creation.
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
-- * 'availabilityZone' - The Availability Zone in which to create the volume.
-- * 'volumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
--
-- Default: @gp2@
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'snapshotId' - The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
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
      availabilityZone = pAvailabilityZone_,
      volumeType = Lude.Nothing,
      dryRun = Lude.Nothing,
      snapshotId = Lude.Nothing
    }

-- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'multiAttachEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvMultiAttachEnabled :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Bool)
cvMultiAttachEnabled = Lens.lens (multiAttachEnabled :: CreateVolume -> Lude.Maybe Lude.Bool) (\s a -> s {multiAttachEnabled = a} :: CreateVolume)
{-# DEPRECATED cvMultiAttachEnabled "Use generic-lens or generic-optics with 'multiAttachEnabled' instead." #-}

-- | The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size.
--
-- Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ and @io2@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size.
-- Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- /Note:/ Consider using 'size' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvSize :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Int)
cvSize = Lens.lens (size :: CreateVolume -> Lude.Maybe Lude.Int) (\s a -> s {size = a} :: CreateVolume)
{-# DEPRECATED cvSize "Use generic-lens or generic-optics with 'size' instead." #-}

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- /Note:/ Consider using 'iops' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvIOPS :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Int)
cvIOPS = Lens.lens (iops :: CreateVolume -> Lude.Maybe Lude.Int) (\s a -> s {iops = a} :: CreateVolume)
{-# DEPRECATED cvIOPS "Use generic-lens or generic-optics with 'iops' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvOutpostARN :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Text)
cvOutpostARN = Lens.lens (outpostARN :: CreateVolume -> Lude.Maybe Lude.Text) (\s a -> s {outpostARN = a} :: CreateVolume)
{-# DEPRECATED cvOutpostARN "Use generic-lens or generic-optics with 'outpostARN' instead." #-}

-- | Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
--
-- /Note:/ Consider using 'encrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvEncrypted :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Bool)
cvEncrypted = Lens.lens (encrypted :: CreateVolume -> Lude.Maybe Lude.Bool) (\s a -> s {encrypted = a} :: CreateVolume)
{-# DEPRECATED cvEncrypted "Use generic-lens or generic-optics with 'encrypted' instead." #-}

-- | The tags to apply to the volume during creation.
--
-- /Note:/ Consider using 'tagSpecifications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvTagSpecifications :: Lens.Lens' CreateVolume (Lude.Maybe [TagSpecification])
cvTagSpecifications = Lens.lens (tagSpecifications :: CreateVolume -> Lude.Maybe [TagSpecification]) (\s a -> s {tagSpecifications = a} :: CreateVolume)
{-# DEPRECATED cvTagSpecifications "Use generic-lens or generic-optics with 'tagSpecifications' instead." #-}

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
cvKMSKeyId :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Text)
cvKMSKeyId = Lens.lens (kmsKeyId :: CreateVolume -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateVolume)
{-# DEPRECATED cvKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Availability Zone in which to create the volume.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvAvailabilityZone :: Lens.Lens' CreateVolume Lude.Text
cvAvailabilityZone = Lens.lens (availabilityZone :: CreateVolume -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateVolume)
{-# DEPRECATED cvAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes.
--
-- Default: @gp2@
--
-- /Note:/ Consider using 'volumeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvVolumeType :: Lens.Lens' CreateVolume (Lude.Maybe VolumeType)
cvVolumeType = Lens.lens (volumeType :: CreateVolume -> Lude.Maybe VolumeType) (\s a -> s {volumeType = a} :: CreateVolume)
{-# DEPRECATED cvVolumeType "Use generic-lens or generic-optics with 'volumeType' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvDryRun :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Bool)
cvDryRun = Lens.lens (dryRun :: CreateVolume -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: CreateVolume)
{-# DEPRECATED cvDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvSnapshotId :: Lens.Lens' CreateVolume (Lude.Maybe Lude.Text)
cvSnapshotId = Lens.lens (snapshotId :: CreateVolume -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: CreateVolume)
{-# DEPRECATED cvSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

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
        "AvailabilityZone" Lude.=: availabilityZone,
        "VolumeType" Lude.=: volumeType,
        "DryRun" Lude.=: dryRun,
        "SnapshotId" Lude.=: snapshotId
      ]
