{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EBS volume that can be attached to an instance in the same Availability Zone. The volume is created in the regional endpoint that you send the HTTP request to. For more information see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints> .
--
--
-- You can create a new empty volume or restore a volume from an EBS snapshot. Any AWS Marketplace product codes from the snapshot are propagated to the volume.
--
-- You can create encrypted volumes with the @Encrypted@ parameter. Encrypted volumes may only be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are also automatically encrypted. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can tag your volumes during creation. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_Tags.html Tagging Your Amazon EC2 Resources> .
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html Creating an Amazon EBS Volume> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreateVolume
    (
    -- * Creating a Request
      createVolume
    , CreateVolume
    -- * Request Lenses
    , cvvSize
    , cvvIOPS
    , cvvEncrypted
    , cvvTagSpecifications
    , cvvKMSKeyId
    , cvvVolumeType
    , cvvDryRun
    , cvvSnapshotId
    , cvvAvailabilityZone

    -- * Destructuring the Response
    , volume
    , Volume
    -- * Response Lenses
    , vAttachments
    , vIOPS
    , vKMSKeyId
    , vTags
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vSize
    , vSnapshotId
    , vState
    , vVolumeId
    , vVolumeType
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateVolume.
--
--
--
-- /See:/ 'createVolume' smart constructor.
data CreateVolume = CreateVolume'
  { _cvvSize              :: !(Maybe Int)
  , _cvvIOPS              :: !(Maybe Int)
  , _cvvEncrypted         :: !(Maybe Bool)
  , _cvvTagSpecifications :: !(Maybe [TagSpecification])
  , _cvvKMSKeyId          :: !(Maybe Text)
  , _cvvVolumeType        :: !(Maybe VolumeType)
  , _cvvDryRun            :: !(Maybe Bool)
  , _cvvSnapshotId        :: !(Maybe Text)
  , _cvvAvailabilityZone  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvvSize' - The size of the volume, in GiBs. Constraints: 1-16384 for @gp2@ , 4-16384 for @io1@ , 500-16384 for @st1@ , 500-16384 for @sc1@ , and 1-1024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'cvvIOPS' - The number of I/O operations per second (IOPS) to provision for the volume, with a maximum ratio of 50 IOPS/GiB. Range is 100 to 32000 IOPS for volumes in most regions. For exceptions, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> . This parameter is valid only for Provisioned IOPS SSD (io1) volumes.
--
-- * 'cvvEncrypted' - Specifies whether the volume should be encrypted. Encrypted Amazon EBS volumes may only be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or vice versa. If your AMI uses encrypted volumes, you can only launch it on supported instance types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'cvvTagSpecifications' - The tags to apply to the volume during creation.
--
-- * 'cvvKMSKeyId' - An identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .      * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. The action will eventually fail.
--
-- * 'cvvVolumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Defaults: If no volume type is specified, the default is @standard@ in us-east-1, eu-west-1, eu-central-1, us-west-2, us-west-1, sa-east-1, ap-northeast-1, ap-northeast-2, ap-southeast-1, ap-southeast-2, ap-south-1, us-gov-west-1, and cn-north-1. In all other regions, EBS defaults to @gp2@ .
--
-- * 'cvvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvvSnapshotId' - The snapshot from which to create the volume.
--
-- * 'cvvAvailabilityZone' - The Availability Zone in which to create the volume. Use 'DescribeAvailabilityZones' to list the Availability Zones that are currently available to you.
createVolume
    :: Text -- ^ 'cvvAvailabilityZone'
    -> CreateVolume
createVolume pAvailabilityZone_ =
  CreateVolume'
    { _cvvSize = Nothing
    , _cvvIOPS = Nothing
    , _cvvEncrypted = Nothing
    , _cvvTagSpecifications = Nothing
    , _cvvKMSKeyId = Nothing
    , _cvvVolumeType = Nothing
    , _cvvDryRun = Nothing
    , _cvvSnapshotId = Nothing
    , _cvvAvailabilityZone = pAvailabilityZone_
    }


-- | The size of the volume, in GiBs. Constraints: 1-16384 for @gp2@ , 4-16384 for @io1@ , 500-16384 for @st1@ , 500-16384 for @sc1@ , and 1-1024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
cvvSize :: Lens' CreateVolume (Maybe Int)
cvvSize = lens _cvvSize (\ s a -> s{_cvvSize = a})

-- | The number of I/O operations per second (IOPS) to provision for the volume, with a maximum ratio of 50 IOPS/GiB. Range is 100 to 32000 IOPS for volumes in most regions. For exceptions, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBS Volume Types> . This parameter is valid only for Provisioned IOPS SSD (io1) volumes.
cvvIOPS :: Lens' CreateVolume (Maybe Int)
cvvIOPS = lens _cvvIOPS (\ s a -> s{_cvvIOPS = a})

-- | Specifies whether the volume should be encrypted. Encrypted Amazon EBS volumes may only be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or vice versa. If your AMI uses encrypted volumes, you can only launch it on supported instance types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
cvvEncrypted :: Lens' CreateVolume (Maybe Bool)
cvvEncrypted = lens _cvvEncrypted (\ s a -> s{_cvvEncrypted = a})

-- | The tags to apply to the volume during creation.
cvvTagSpecifications :: Lens' CreateVolume [TagSpecification]
cvvTagSpecifications = lens _cvvTagSpecifications (\ s a -> s{_cvvTagSpecifications = a}) . _Default . _Coerce

-- | An identifier for the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.  The CMK identifier may be provided in any of the following formats:      * Key ID     * Key alias     * ARN using key ID. The ID ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ .      * ARN using key alias. The alias ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @alias@ namespace, and then the CMK alias. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :alias//ExampleAlias/ .  AWS parses @KmsKeyId@ asynchronously, meaning that the action you call may appear to complete even though you provided an invalid identifier. The action will eventually fail.
cvvKMSKeyId :: Lens' CreateVolume (Maybe Text)
cvvKMSKeyId = lens _cvvKMSKeyId (\ s a -> s{_cvvKMSKeyId = a})

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Defaults: If no volume type is specified, the default is @standard@ in us-east-1, eu-west-1, eu-central-1, us-west-2, us-west-1, sa-east-1, ap-northeast-1, ap-northeast-2, ap-southeast-1, ap-southeast-2, ap-south-1, us-gov-west-1, and cn-north-1. In all other regions, EBS defaults to @gp2@ .
cvvVolumeType :: Lens' CreateVolume (Maybe VolumeType)
cvvVolumeType = lens _cvvVolumeType (\ s a -> s{_cvvVolumeType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvvDryRun :: Lens' CreateVolume (Maybe Bool)
cvvDryRun = lens _cvvDryRun (\ s a -> s{_cvvDryRun = a})

-- | The snapshot from which to create the volume.
cvvSnapshotId :: Lens' CreateVolume (Maybe Text)
cvvSnapshotId = lens _cvvSnapshotId (\ s a -> s{_cvvSnapshotId = a})

-- | The Availability Zone in which to create the volume. Use 'DescribeAvailabilityZones' to list the Availability Zones that are currently available to you.
cvvAvailabilityZone :: Lens' CreateVolume Text
cvvAvailabilityZone = lens _cvvAvailabilityZone (\ s a -> s{_cvvAvailabilityZone = a})

instance AWSRequest CreateVolume where
        type Rs CreateVolume = Volume
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable CreateVolume where

instance NFData CreateVolume where

instance ToHeaders CreateVolume where
        toHeaders = const mempty

instance ToPath CreateVolume where
        toPath = const "/"

instance ToQuery CreateVolume where
        toQuery CreateVolume'{..}
          = mconcat
              ["Action" =: ("CreateVolume" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Size" =: _cvvSize, "Iops" =: _cvvIOPS,
               "Encrypted" =: _cvvEncrypted,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _cvvTagSpecifications),
               "KmsKeyId" =: _cvvKMSKeyId,
               "VolumeType" =: _cvvVolumeType,
               "DryRun" =: _cvvDryRun,
               "SnapshotId" =: _cvvSnapshotId,
               "AvailabilityZone" =: _cvvAvailabilityZone]
