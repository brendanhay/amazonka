{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EBS volume that can be attached to an instance in the same
-- Availability Zone.
--
-- You can create a new empty volume or restore a volume from an EBS
-- snapshot. Any AWS Marketplace product codes from the snapshot are
-- propagated to the volume.
--
-- You can create encrypted volumes. Encrypted volumes must be attached to
-- instances that support Amazon EBS encryption. Volumes that are created
-- from encrypted snapshots are also automatically encrypted. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You can tag your volumes during creation. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html Creating an Amazon EBS volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateVolume
  ( -- * Creating a Request
    CreateVolume (..),
    newCreateVolume,

    -- * Request Lenses
    createVolume_multiAttachEnabled,
    createVolume_tagSpecifications,
    createVolume_dryRun,
    createVolume_encrypted,
    createVolume_outpostArn,
    createVolume_throughput,
    createVolume_kmsKeyId,
    createVolume_snapshotId,
    createVolume_volumeType,
    createVolume_iops,
    createVolume_size,
    createVolume_availabilityZone,

    -- * Destructuring the Response
    Volume (..),
    newVolume,

    -- * Response Lenses
    volume_multiAttachEnabled,
    volume_fastRestored,
    volume_outpostArn,
    volume_throughput,
    volume_kmsKeyId,
    volume_tags,
    volume_iops,
    volume_attachments,
    volume_availabilityZone,
    volume_createTime,
    volume_encrypted,
    volume_size,
    volume_snapshotId,
    volume_state,
    volume_volumeId,
    volume_volumeType,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateVolume' smart constructor.
data CreateVolume = CreateVolume'
  { -- | Indicates whether to enable Amazon EBS Multi-Attach. If you enable
    -- Multi-Attach, you can attach the volume to up to 16
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>
    -- in the same Availability Zone. This parameter is supported with @io1@
    -- and @io2@ volumes only. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    multiAttachEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the volume during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the volume should be encrypted. The effect of setting
    -- the encryption state to @true@ depends on the volume origin (new or from
    -- a snapshot), starting encryption state, ownership, and whether
    -- encryption by default is enabled. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- Encrypted Amazon EBS volumes must be attached to instances that support
    -- Amazon EBS encryption. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The throughput to provision for a volume, with a maximum of 1,000
    -- MiB\/s.
    --
    -- This parameter is valid only for @gp3@ volumes.
    --
    -- Valid Range: Minimum value of 125. Maximum value of 1000.
    throughput :: Prelude.Maybe Prelude.Int,
    -- | The identifier of the AWS Key Management Service (AWS KMS) customer
    -- master key (CMK) to use for Amazon EBS encryption. If this parameter is
    -- not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is
    -- specified, the encrypted state must be @true@.
    --
    -- You can specify the CMK using any of the following:
    --
    -- -   Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
    --
    -- -   Key alias. For example, alias\/ExampleAlias.
    --
    -- -   Key ARN. For example,
    --     arn:aws:kms:us-east-1:012345678910:key\/1234abcd-12ab-34cd-56ef-1234567890ab.
    --
    -- -   Alias ARN. For example,
    --     arn:aws:kms:us-east-1:012345678910:alias\/ExampleAlias.
    --
    -- AWS authenticates the CMK asynchronously. Therefore, if you specify an
    -- ID, alias, or ARN that is not valid, the action can appear to complete,
    -- but eventually fails.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The snapshot from which to create the volume. You must specify either a
    -- snapshot ID or a volume size.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The volume type. This parameter can be one of the following values:
    --
    -- -   General Purpose SSD: @gp2@ | @gp3@
    --
    -- -   Provisioned IOPS SSD: @io1@ | @io2@
    --
    -- -   Throughput Optimized HDD: @st1@
    --
    -- -   Cold HDD: @sc1@
    --
    -- -   Magnetic: @standard@
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    --
    -- Default: @gp2@
    volumeType :: Prelude.Maybe VolumeType,
    -- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
    -- @io2@ volumes, this represents the number of IOPS that are provisioned
    -- for the volume. For @gp2@ volumes, this represents the baseline
    -- performance of the volume and the rate at which the volume accumulates
    -- I\/O credits for bursting.
    --
    -- The following are the supported values for each volume type:
    --
    -- -   @gp3@: 3,000-16,000 IOPS
    --
    -- -   @io1@: 100-64,000 IOPS
    --
    -- -   @io2@: 100-64,000 IOPS
    --
    -- For @io1@ and @io2@ volumes, we guarantee 64,000 IOPS only for
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
    -- Other instance families guarantee performance up to 32,000 IOPS.
    --
    -- This parameter is required for @io1@ and @io2@ volumes. The default for
    -- @gp3@ volumes is 3,000 IOPS. This parameter is not supported for @gp2@,
    -- @st1@, @sc1@, or @standard@ volumes.
    iops :: Prelude.Maybe Prelude.Int,
    -- | The size of the volume, in GiBs. You must specify either a snapshot ID
    -- or a volume size. If you specify a snapshot, the default is the snapshot
    -- size. You can specify a volume size that is equal to or larger than the
    -- snapshot size.
    --
    -- The following are the supported volumes sizes for each volume type:
    --
    -- -   @gp2@ and @gp3@: 1-16,384
    --
    -- -   @io1@ and @io2@: 4-16,384
    --
    -- -   @st1@ and @sc1@: 125-16,384
    --
    -- -   @standard@: 1-1,024
    size :: Prelude.Maybe Prelude.Int,
    -- | The Availability Zone in which to create the volume.
    availabilityZone :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVolume' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'multiAttachEnabled', 'createVolume_multiAttachEnabled' - Indicates whether to enable Amazon EBS Multi-Attach. If you enable
-- Multi-Attach, you can attach the volume to up to 16
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>
-- in the same Availability Zone. This parameter is supported with @io1@
-- and @io2@ volumes only. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'tagSpecifications', 'createVolume_tagSpecifications' - The tags to apply to the volume during creation.
--
-- 'dryRun', 'createVolume_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'encrypted', 'createVolume_encrypted' - Indicates whether the volume should be encrypted. The effect of setting
-- the encryption state to @true@ depends on the volume origin (new or from
-- a snapshot), starting encryption state, ownership, and whether
-- encryption by default is enabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Encrypted Amazon EBS volumes must be attached to instances that support
-- Amazon EBS encryption. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
--
-- 'outpostArn', 'createVolume_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'throughput', 'createVolume_throughput' - The throughput to provision for a volume, with a maximum of 1,000
-- MiB\/s.
--
-- This parameter is valid only for @gp3@ volumes.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
--
-- 'kmsKeyId', 'createVolume_kmsKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) to use for Amazon EBS encryption. If this parameter is
-- not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is
-- specified, the encrypted state must be @true@.
--
-- You can specify the CMK using any of the following:
--
-- -   Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Key alias. For example, alias\/ExampleAlias.
--
-- -   Key ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:key\/1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Alias ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:alias\/ExampleAlias.
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an
-- ID, alias, or ARN that is not valid, the action can appear to complete,
-- but eventually fails.
--
-- 'snapshotId', 'createVolume_snapshotId' - The snapshot from which to create the volume. You must specify either a
-- snapshot ID or a volume size.
--
-- 'volumeType', 'createVolume_volumeType' - The volume type. This parameter can be one of the following values:
--
-- -   General Purpose SSD: @gp2@ | @gp3@
--
-- -   Provisioned IOPS SSD: @io1@ | @io2@
--
-- -   Throughput Optimized HDD: @st1@
--
-- -   Cold HDD: @sc1@
--
-- -   Magnetic: @standard@
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: @gp2@
--
-- 'iops', 'createVolume_iops' - The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- -   @io2@: 100-64,000 IOPS
--
-- For @io1@ and @io2@ volumes, we guarantee 64,000 IOPS only for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS.
--
-- This parameter is required for @io1@ and @io2@ volumes. The default for
-- @gp3@ volumes is 3,000 IOPS. This parameter is not supported for @gp2@,
-- @st1@, @sc1@, or @standard@ volumes.
--
-- 'size', 'createVolume_size' - The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size. If you specify a snapshot, the default is the snapshot
-- size. You can specify a volume size that is equal to or larger than the
-- snapshot size.
--
-- The following are the supported volumes sizes for each volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
--
-- 'availabilityZone', 'createVolume_availabilityZone' - The Availability Zone in which to create the volume.
newCreateVolume ::
  -- | 'availabilityZone'
  Prelude.Text ->
  CreateVolume
newCreateVolume pAvailabilityZone_ =
  CreateVolume'
    { multiAttachEnabled = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      throughput = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      snapshotId = Prelude.Nothing,
      volumeType = Prelude.Nothing,
      iops = Prelude.Nothing,
      size = Prelude.Nothing,
      availabilityZone = pAvailabilityZone_
    }

-- | Indicates whether to enable Amazon EBS Multi-Attach. If you enable
-- Multi-Attach, you can attach the volume to up to 16
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>
-- in the same Availability Zone. This parameter is supported with @io1@
-- and @io2@ volumes only. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach>
-- in the /Amazon Elastic Compute Cloud User Guide/.
createVolume_multiAttachEnabled :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Bool)
createVolume_multiAttachEnabled = Lens.lens (\CreateVolume' {multiAttachEnabled} -> multiAttachEnabled) (\s@CreateVolume' {} a -> s {multiAttachEnabled = a} :: CreateVolume)

-- | The tags to apply to the volume during creation.
createVolume_tagSpecifications :: Lens.Lens' CreateVolume (Prelude.Maybe [TagSpecification])
createVolume_tagSpecifications = Lens.lens (\CreateVolume' {tagSpecifications} -> tagSpecifications) (\s@CreateVolume' {} a -> s {tagSpecifications = a} :: CreateVolume) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVolume_dryRun :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Bool)
createVolume_dryRun = Lens.lens (\CreateVolume' {dryRun} -> dryRun) (\s@CreateVolume' {} a -> s {dryRun = a} :: CreateVolume)

-- | Indicates whether the volume should be encrypted. The effect of setting
-- the encryption state to @true@ depends on the volume origin (new or from
-- a snapshot), starting encryption state, ownership, and whether
-- encryption by default is enabled. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Encrypted Amazon EBS volumes must be attached to instances that support
-- Amazon EBS encryption. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types>.
createVolume_encrypted :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Bool)
createVolume_encrypted = Lens.lens (\CreateVolume' {encrypted} -> encrypted) (\s@CreateVolume' {} a -> s {encrypted = a} :: CreateVolume)

-- | The Amazon Resource Name (ARN) of the Outpost.
createVolume_outpostArn :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Text)
createVolume_outpostArn = Lens.lens (\CreateVolume' {outpostArn} -> outpostArn) (\s@CreateVolume' {} a -> s {outpostArn = a} :: CreateVolume)

-- | The throughput to provision for a volume, with a maximum of 1,000
-- MiB\/s.
--
-- This parameter is valid only for @gp3@ volumes.
--
-- Valid Range: Minimum value of 125. Maximum value of 1000.
createVolume_throughput :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Int)
createVolume_throughput = Lens.lens (\CreateVolume' {throughput} -> throughput) (\s@CreateVolume' {} a -> s {throughput = a} :: CreateVolume)

-- | The identifier of the AWS Key Management Service (AWS KMS) customer
-- master key (CMK) to use for Amazon EBS encryption. If this parameter is
-- not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is
-- specified, the encrypted state must be @true@.
--
-- You can specify the CMK using any of the following:
--
-- -   Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Key alias. For example, alias\/ExampleAlias.
--
-- -   Key ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:key\/1234abcd-12ab-34cd-56ef-1234567890ab.
--
-- -   Alias ARN. For example,
--     arn:aws:kms:us-east-1:012345678910:alias\/ExampleAlias.
--
-- AWS authenticates the CMK asynchronously. Therefore, if you specify an
-- ID, alias, or ARN that is not valid, the action can appear to complete,
-- but eventually fails.
createVolume_kmsKeyId :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Text)
createVolume_kmsKeyId = Lens.lens (\CreateVolume' {kmsKeyId} -> kmsKeyId) (\s@CreateVolume' {} a -> s {kmsKeyId = a} :: CreateVolume)

-- | The snapshot from which to create the volume. You must specify either a
-- snapshot ID or a volume size.
createVolume_snapshotId :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Text)
createVolume_snapshotId = Lens.lens (\CreateVolume' {snapshotId} -> snapshotId) (\s@CreateVolume' {} a -> s {snapshotId = a} :: CreateVolume)

-- | The volume type. This parameter can be one of the following values:
--
-- -   General Purpose SSD: @gp2@ | @gp3@
--
-- -   Provisioned IOPS SSD: @io1@ | @io2@
--
-- -   Throughput Optimized HDD: @st1@
--
-- -   Cold HDD: @sc1@
--
-- -   Magnetic: @standard@
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Default: @gp2@
createVolume_volumeType :: Lens.Lens' CreateVolume (Prelude.Maybe VolumeType)
createVolume_volumeType = Lens.lens (\CreateVolume' {volumeType} -> volumeType) (\s@CreateVolume' {} a -> s {volumeType = a} :: CreateVolume)

-- | The number of I\/O operations per second (IOPS). For @gp3@, @io1@, and
-- @io2@ volumes, this represents the number of IOPS that are provisioned
-- for the volume. For @gp2@ volumes, this represents the baseline
-- performance of the volume and the rate at which the volume accumulates
-- I\/O credits for bursting.
--
-- The following are the supported values for each volume type:
--
-- -   @gp3@: 3,000-16,000 IOPS
--
-- -   @io1@: 100-64,000 IOPS
--
-- -   @io2@: 100-64,000 IOPS
--
-- For @io1@ and @io2@ volumes, we guarantee 64,000 IOPS only for
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Instances built on the Nitro System>.
-- Other instance families guarantee performance up to 32,000 IOPS.
--
-- This parameter is required for @io1@ and @io2@ volumes. The default for
-- @gp3@ volumes is 3,000 IOPS. This parameter is not supported for @gp2@,
-- @st1@, @sc1@, or @standard@ volumes.
createVolume_iops :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Int)
createVolume_iops = Lens.lens (\CreateVolume' {iops} -> iops) (\s@CreateVolume' {} a -> s {iops = a} :: CreateVolume)

-- | The size of the volume, in GiBs. You must specify either a snapshot ID
-- or a volume size. If you specify a snapshot, the default is the snapshot
-- size. You can specify a volume size that is equal to or larger than the
-- snapshot size.
--
-- The following are the supported volumes sizes for each volume type:
--
-- -   @gp2@ and @gp3@: 1-16,384
--
-- -   @io1@ and @io2@: 4-16,384
--
-- -   @st1@ and @sc1@: 125-16,384
--
-- -   @standard@: 1-1,024
createVolume_size :: Lens.Lens' CreateVolume (Prelude.Maybe Prelude.Int)
createVolume_size = Lens.lens (\CreateVolume' {size} -> size) (\s@CreateVolume' {} a -> s {size = a} :: CreateVolume)

-- | The Availability Zone in which to create the volume.
createVolume_availabilityZone :: Lens.Lens' CreateVolume Prelude.Text
createVolume_availabilityZone = Lens.lens (\CreateVolume' {availabilityZone} -> availabilityZone) (\s@CreateVolume' {} a -> s {availabilityZone = a} :: CreateVolume)

instance Prelude.AWSRequest CreateVolume where
  type Rs CreateVolume = Volume
  request = Request.postQuery defaultService
  response =
    Response.receiveXML (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable CreateVolume

instance Prelude.NFData CreateVolume

instance Prelude.ToHeaders CreateVolume where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateVolume where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateVolume where
  toQuery CreateVolume' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateVolume" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "MultiAttachEnabled" Prelude.=: multiAttachEnabled,
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "Encrypted" Prelude.=: encrypted,
        "OutpostArn" Prelude.=: outpostArn,
        "Throughput" Prelude.=: throughput,
        "KmsKeyId" Prelude.=: kmsKeyId,
        "SnapshotId" Prelude.=: snapshotId,
        "VolumeType" Prelude.=: volumeType,
        "Iops" Prelude.=: iops,
        "Size" Prelude.=: size,
        "AvailabilityZone" Prelude.=: availabilityZone
      ]
