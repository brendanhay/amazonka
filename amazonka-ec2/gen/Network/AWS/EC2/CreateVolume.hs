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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , creSize
    , creIOPS
    , creEncrypted
    , creTagSpecifications
    , creKMSKeyId
    , creVolumeType
    , creDryRun
    , creSnapshotId
    , creAvailabilityZone

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

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for CreateVolume.
--
--
--
-- /See:/ 'createVolume' smart constructor.
data CreateVolume = CreateVolume'
    { _creSize              :: !(Maybe Int)
    , _creIOPS              :: !(Maybe Int)
    , _creEncrypted         :: !(Maybe Bool)
    , _creTagSpecifications :: !(Maybe [TagSpecification])
    , _creKMSKeyId          :: !(Maybe Text)
    , _creVolumeType        :: !(Maybe VolumeType)
    , _creDryRun            :: !(Maybe Bool)
    , _creSnapshotId        :: !(Maybe Text)
    , _creAvailabilityZone  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'creSize' - The size of the volume, in GiBs. Constraints: 1-16384 for @gp2@ , 4-16384 for @io1@ , 500-16384 for @st1@ , 500-16384 for @sc1@ , and 1-1024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
--
-- * 'creIOPS' - Only valid for Provisioned IOPS SSD volumes. The number of I/O operations per second (IOPS) to provision for the volume, with a maximum ratio of 50 IOPS/GiB. Constraint: Range is 100 to 20000 for Provisioned IOPS SSD volumes
--
-- * 'creEncrypted' - Specifies whether the volume should be encrypted. Encrypted Amazon EBS volumes may only be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or vice versa. If your AMI uses encrypted volumes, you can only launch it on supported instance types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'creTagSpecifications' - The tags to apply to the volume during creation.
--
-- * 'creKMSKeyId' - The full ARN of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. The ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ . If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
--
-- * 'creVolumeType' - The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Default: @standard@
--
-- * 'creDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'creSnapshotId' - The snapshot from which to create the volume.
--
-- * 'creAvailabilityZone' - The Availability Zone in which to create the volume. Use 'DescribeAvailabilityZones' to list the Availability Zones that are currently available to you.
createVolume
    :: Text -- ^ 'creAvailabilityZone'
    -> CreateVolume
createVolume pAvailabilityZone_ =
    CreateVolume'
    { _creSize = Nothing
    , _creIOPS = Nothing
    , _creEncrypted = Nothing
    , _creTagSpecifications = Nothing
    , _creKMSKeyId = Nothing
    , _creVolumeType = Nothing
    , _creDryRun = Nothing
    , _creSnapshotId = Nothing
    , _creAvailabilityZone = pAvailabilityZone_
    }

-- | The size of the volume, in GiBs. Constraints: 1-16384 for @gp2@ , 4-16384 for @io1@ , 500-16384 for @st1@ , 500-16384 for @sc1@ , and 1-1024 for @standard@ . If you specify a snapshot, the volume size must be equal to or larger than the snapshot size. Default: If you're creating the volume from a snapshot and don't specify a volume size, the default is the snapshot size.
creSize :: Lens' CreateVolume (Maybe Int)
creSize = lens _creSize (\ s a -> s{_creSize = a});

-- | Only valid for Provisioned IOPS SSD volumes. The number of I/O operations per second (IOPS) to provision for the volume, with a maximum ratio of 50 IOPS/GiB. Constraint: Range is 100 to 20000 for Provisioned IOPS SSD volumes
creIOPS :: Lens' CreateVolume (Maybe Int)
creIOPS = lens _creIOPS (\ s a -> s{_creIOPS = a});

-- | Specifies whether the volume should be encrypted. Encrypted Amazon EBS volumes may only be attached to instances that support Amazon EBS encryption. Volumes that are created from encrypted snapshots are automatically encrypted. There is no way to create an encrypted volume from an unencrypted snapshot or vice versa. If your AMI uses encrypted volumes, you can only launch it on supported instance types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
creEncrypted :: Lens' CreateVolume (Maybe Bool)
creEncrypted = lens _creEncrypted (\ s a -> s{_creEncrypted = a});

-- | The tags to apply to the volume during creation.
creTagSpecifications :: Lens' CreateVolume [TagSpecification]
creTagSpecifications = lens _creTagSpecifications (\ s a -> s{_creTagSpecifications = a}) . _Default . _Coerce;

-- | The full ARN of the AWS Key Management Service (AWS KMS) customer master key (CMK) to use when creating the encrypted volume. This parameter is only required if you want to use a non-default CMK; if this parameter is not specified, the default CMK for EBS is used. The ARN contains the @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS account ID of the CMK owner, the @key@ namespace, and then the CMK ID. For example, arn:aws:kms:/us-east-1/ :/012345678910/ :key//abcd1234-a123-456a-a12b-a123b4cd56ef/ . If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
creKMSKeyId :: Lens' CreateVolume (Maybe Text)
creKMSKeyId = lens _creKMSKeyId (\ s a -> s{_creKMSKeyId = a});

-- | The volume type. This can be @gp2@ for General Purpose SSD, @io1@ for Provisioned IOPS SSD, @st1@ for Throughput Optimized HDD, @sc1@ for Cold HDD, or @standard@ for Magnetic volumes. Default: @standard@
creVolumeType :: Lens' CreateVolume (Maybe VolumeType)
creVolumeType = lens _creVolumeType (\ s a -> s{_creVolumeType = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
creDryRun :: Lens' CreateVolume (Maybe Bool)
creDryRun = lens _creDryRun (\ s a -> s{_creDryRun = a});

-- | The snapshot from which to create the volume.
creSnapshotId :: Lens' CreateVolume (Maybe Text)
creSnapshotId = lens _creSnapshotId (\ s a -> s{_creSnapshotId = a});

-- | The Availability Zone in which to create the volume. Use 'DescribeAvailabilityZones' to list the Availability Zones that are currently available to you.
creAvailabilityZone :: Lens' CreateVolume Text
creAvailabilityZone = lens _creAvailabilityZone (\ s a -> s{_creAvailabilityZone = a});

instance AWSRequest CreateVolume where
        type Rs CreateVolume = Volume
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable CreateVolume

instance NFData CreateVolume

instance ToHeaders CreateVolume where
        toHeaders = const mempty

instance ToPath CreateVolume where
        toPath = const "/"

instance ToQuery CreateVolume where
        toQuery CreateVolume'{..}
          = mconcat
              ["Action" =: ("CreateVolume" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Size" =: _creSize, "Iops" =: _creIOPS,
               "Encrypted" =: _creEncrypted,
               toQuery
                 (toQueryList "TagSpecification" <$>
                    _creTagSpecifications),
               "KmsKeyId" =: _creKMSKeyId,
               "VolumeType" =: _creVolumeType,
               "DryRun" =: _creDryRun,
               "SnapshotId" =: _creSnapshotId,
               "AvailabilityZone" =: _creAvailabilityZone]
