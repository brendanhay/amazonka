{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
--
-- You can create a new empty volume or restore a volume from an EBS snapshot. Any AWS Marketplace product codes from the snapshot are propagated to the volume.
--
-- You can create encrypted volumes. Encrypted volumes must be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are also automatically encrypted. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can tag your volumes during creation. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging your Amazon EC2 resources> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html Creating an Amazon EBS volume> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.CreateVolume
  ( -- * Creating a Request
    createVolume,
    CreateVolume,

    -- * Request Lenses
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

    -- * Destructuring the Response
    volume,
    Volume,

    -- * Response Lenses
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
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVolume' smart constructor.
data CreateVolume = CreateVolume'
  { _cvvMultiAttachEnabled ::
      !(Maybe Bool),
    _cvvSize :: !(Maybe Int),
    _cvvIOPS :: !(Maybe Int),
    _cvvOutpostARN :: !(Maybe Text),
    _cvvEncrypted :: !(Maybe Bool),
    _cvvTagSpecifications :: !(Maybe [TagSpecification]),
    _cvvKMSKeyId :: !(Maybe Text),
    _cvvVolumeType :: !(Maybe VolumeType),
    _cvvDryRun :: !(Maybe Bool),
    _cvvSnapshotId :: !(Maybe Text),
    _cvvAvailabilityZone :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvvMultiAttachEnabled' - Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'cvvSize' - The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size. Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ and @io2@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'cvvIOPS' - The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
--
-- * 'cvvOutpostARN' - The Amazon Resource Name (ARN) of the Outpost.
--
-- * 'cvvEncrypted' - Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default> in the /Amazon Elastic Compute Cloud User Guide/ . Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
--
-- * 'cvvTagSpecifications' - The tags to apply to the volume during creation.
--
-- * 'cvvKMSKeyId' - The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
--
-- * 'cvvVolumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Default: @gp2@
--
-- * 'cvvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvvSnapshotId' - The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
--
-- * 'cvvAvailabilityZone' - The Availability Zone in which to create the volume.
createVolume ::
  -- | 'cvvAvailabilityZone'
  Text ->
  CreateVolume
createVolume pAvailabilityZone_ =
  CreateVolume'
    { _cvvMultiAttachEnabled = Nothing,
      _cvvSize = Nothing,
      _cvvIOPS = Nothing,
      _cvvOutpostARN = Nothing,
      _cvvEncrypted = Nothing,
      _cvvTagSpecifications = Nothing,
      _cvvKMSKeyId = Nothing,
      _cvvVolumeType = Nothing,
      _cvvDryRun = Nothing,
      _cvvSnapshotId = Nothing,
      _cvvAvailabilityZone = pAvailabilityZone_
    }

-- | Specifies whether to enable Amazon EBS Multi-Attach. If you enable Multi-Attach, you can attach the volume to up to 16 <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> in the same Availability Zone. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-volumes-multi.html Amazon EBS Multi-Attach> in the /Amazon Elastic Compute Cloud User Guide/ .
cvvMultiAttachEnabled :: Lens' CreateVolume (Maybe Bool)
cvvMultiAttachEnabled = lens _cvvMultiAttachEnabled (\s a -> s {_cvvMultiAttachEnabled = a})

-- | The size of the volume, in GiBs. You must specify either a snapshot ID or a volume size. Constraints: 1-16,384 for @gp2@ , 4-16,384 for @io1@ and @io2@ , 500-16,384 for @st1@ , 500-16,384 for @sc1@ , and 1-1,024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
cvvSize :: Lens' CreateVolume (Maybe Int)
cvvSize = lens _cvvSize (\s a -> s {_cvvSize = a})

-- | The number of I/O operations per second (IOPS) to provision for an @io1@ or @io2@ volume, with a maximum ratio of 50 IOPS/GiB for @io1@ , and 500 IOPS/GiB for @io2@ . Range is 100 to 64,000 IOPS for volumes in most Regions. Maximum IOPS of 64,000 is guaranteed only on <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html#ec2-nitro-instances Nitro-based instances> . Other instance families guarantee performance up to 32,000 IOPS. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS volume types> in the /Amazon Elastic Compute Cloud User Guide/ . This parameter is valid only for Provisioned IOPS SSD (@io1@ and @io2@ ) volumes.
cvvIOPS :: Lens' CreateVolume (Maybe Int)
cvvIOPS = lens _cvvIOPS (\s a -> s {_cvvIOPS = a})

-- | The Amazon Resource Name (ARN) of the Outpost.
cvvOutpostARN :: Lens' CreateVolume (Maybe Text)
cvvOutpostARN = lens _cvvOutpostARN (\s a -> s {_cvvOutpostARN = a})

-- | Specifies whether the volume should be encrypted. The effect of setting the encryption state to @true@ depends on the volume origin (new or from a snapshot), starting encryption state, ownership, and whether encryption by default is enabled. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#encryption-by-default Encryption by default> in the /Amazon Elastic Compute Cloud User Guide/ . Encrypted Amazon EBS volumes must be attached to instances that support Amazon EBS encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
cvvEncrypted :: Lens' CreateVolume (Maybe Bool)
cvvEncrypted = lens _cvvEncrypted (\s a -> s {_cvvEncrypted = a})

-- | The tags to apply to the volume during creation.
cvvTagSpecifications :: Lens' CreateVolume [TagSpecification]
cvvTagSpecifications = lens _cvvTagSpecifications (\s a -> s {_cvvTagSpecifications = a}) . _Default . _Coerce

-- | The identifier of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use for Amazon EBS encryption. If this parameter is not specified, your AWS managed CMK for EBS is used. If @KmsKeyId@ is specified, the encrypted state must be @true@ . You can specify the CMK using any of the following:     * Key ID. For example, 1234abcd-12ab-34cd-56ef-1234567890ab.     * Key alias. For example, alias/ExampleAlias.     * Key ARN. For example, arn:aws:kms:us-east-1:012345678910:key/1234abcd-12ab-34cd-56ef-1234567890ab.     * Alias ARN. For example, arn:aws:kms:us-east-1:012345678910:alias/ExampleAlias. AWS authenticates the CMK asynchronously. Therefore, if you specify an ID, alias, or ARN that is not valid, the action can appear to complete, but eventually fails.
cvvKMSKeyId :: Lens' CreateVolume (Maybe Text)
cvvKMSKeyId = lens _cvvKMSKeyId (\s a -> s {_cvvKMSKeyId = a})

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ or @io2@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Default: @gp2@
cvvVolumeType :: Lens' CreateVolume (Maybe VolumeType)
cvvVolumeType = lens _cvvVolumeType (\s a -> s {_cvvVolumeType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvvDryRun :: Lens' CreateVolume (Maybe Bool)
cvvDryRun = lens _cvvDryRun (\s a -> s {_cvvDryRun = a})

-- | The snapshot from which to create the volume. You must specify either a snapshot ID or a volume size.
cvvSnapshotId :: Lens' CreateVolume (Maybe Text)
cvvSnapshotId = lens _cvvSnapshotId (\s a -> s {_cvvSnapshotId = a})

-- | The Availability Zone in which to create the volume.
cvvAvailabilityZone :: Lens' CreateVolume Text
cvvAvailabilityZone = lens _cvvAvailabilityZone (\s a -> s {_cvvAvailabilityZone = a})

instance AWSRequest CreateVolume where
  type Rs CreateVolume = Volume
  request = postQuery ec2
  response = receiveXML (\s h x -> parseXML x)

instance Hashable CreateVolume

instance NFData CreateVolume

instance ToHeaders CreateVolume where
  toHeaders = const mempty

instance ToPath CreateVolume where
  toPath = const "/"

instance ToQuery CreateVolume where
  toQuery CreateVolume' {..} =
    mconcat
      [ "Action" =: ("CreateVolume" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "MultiAttachEnabled" =: _cvvMultiAttachEnabled,
        "Size" =: _cvvSize,
        "Iops" =: _cvvIOPS,
        "OutpostArn" =: _cvvOutpostARN,
        "Encrypted" =: _cvvEncrypted,
        toQuery (toQueryList "TagSpecification" <$> _cvvTagSpecifications),
        "KmsKeyId" =: _cvvKMSKeyId,
        "VolumeType" =: _cvvVolumeType,
        "DryRun" =: _cvvDryRun,
        "SnapshotId" =: _cvvSnapshotId,
        "AvailabilityZone" =: _cvvAvailabilityZone
      ]
