{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an EBS volume that can be attached to an instance in the same
-- Availability Zone. The volume is created in the regional endpoint that
-- you send the HTTP request to. For more information see
-- <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- You can create a new empty volume or restore a volume from an EBS
-- snapshot. Any AWS Marketplace product codes from the snapshot are
-- propagated to the volume.
--
-- You can create encrypted volumes with the @Encrypted@ parameter.
-- Encrypted volumes may only be attached to instances that support Amazon
-- EBS encryption. Volumes that are created from encrypted snapshots are
-- also automatically encrypted. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html Creating or Restoring an Amazon EBS Volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVolume.html>
module Network.AWS.EC2.CreateVolume
    (
    -- * Request
      CreateVolume
    -- ** Request constructor
    , createVolume
    -- ** Request lenses
    , cSize
    , cIOPS
    , cEncrypted
    , cKMSKeyId
    , cVolumeType
    , cDryRun
    , cSnapshotId
    , cAvailabilityZone

    -- * Response
    , Volume
    -- ** Response constructor
    , volume
    -- ** Response lenses
    , volAttachments
    , volIOPS
    , volKMSKeyId
    , volTags
    , volAvailabilityZone
    , volCreateTime
    , volEncrypted
    , volSize
    , volSnapshotId
    , volState
    , volVolumeId
    , volVolumeType
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cSize'
--
-- * 'cIOPS'
--
-- * 'cEncrypted'
--
-- * 'cKMSKeyId'
--
-- * 'cVolumeType'
--
-- * 'cDryRun'
--
-- * 'cSnapshotId'
--
-- * 'cAvailabilityZone'
data CreateVolume = CreateVolume'
    { _cSize             :: Maybe Int
    , _cIOPS             :: Maybe Int
    , _cEncrypted        :: Maybe Bool
    , _cKMSKeyId         :: Maybe Text
    , _cVolumeType       :: Maybe VolumeType
    , _cDryRun           :: Maybe Bool
    , _cSnapshotId       :: Maybe Text
    , _cAvailabilityZone :: Text
    } deriving (Eq,Read,Show)

-- | 'CreateVolume' smart constructor.
createVolume :: Text -> CreateVolume
createVolume pAvailabilityZone =
    CreateVolume'
    { _cSize = Nothing
    , _cIOPS = Nothing
    , _cEncrypted = Nothing
    , _cKMSKeyId = Nothing
    , _cVolumeType = Nothing
    , _cDryRun = Nothing
    , _cSnapshotId = Nothing
    , _cAvailabilityZone = pAvailabilityZone
    }

-- | The size of the volume, in GiBs.
--
-- Constraints: @1-1024@ for @standard@ volumes, @1-16384@ for @gp2@
-- volumes, and @4-16384@ for @io1@ volumes. If you specify a snapshot, the
-- volume size must be equal to or larger than the snapshot size.
--
-- Default: If you\'re creating the volume from a snapshot and don\'t
-- specify a volume size, the default is the snapshot size.
cSize :: Lens' CreateVolume (Maybe Int)
cSize = lens _cSize (\ s a -> s{_cSize = a});

-- | Only valid for Provisioned IOPS (SSD) volumes. The number of I\/O
-- operations per second (IOPS) to provision for the volume, with a maximum
-- ratio of 30 IOPS\/GiB.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes
cIOPS :: Lens' CreateVolume (Maybe Int)
cIOPS = lens _cIOPS (\ s a -> s{_cIOPS = a});

-- | Specifies whether the volume should be encrypted. Encrypted Amazon EBS
-- volumes may only be attached to instances that support Amazon EBS
-- encryption. Volumes that are created from encrypted snapshots are
-- automatically encrypted. There is no way to create an encrypted volume
-- from an unencrypted snapshot or vice versa. If your AMI uses encrypted
-- volumes, you can only launch it on supported instance types. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
cEncrypted :: Lens' CreateVolume (Maybe Bool)
cEncrypted = lens _cEncrypted (\ s a -> s{_cEncrypted = a});

-- | The full ARN of the AWS Key Management Service (KMS) master key to use
-- when creating the encrypted volume. This parameter is only required if
-- you want to use a non-default master key; if this parameter is not
-- specified, the default master key is used. The ARN contains the
-- @arn:aws:kms@ namespace, followed by the region of the master key, the
-- AWS account ID of the master key owner, the @key@ namespace, and then
-- the master key ID. For example,
-- arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
cKMSKeyId :: Lens' CreateVolume (Maybe Text)
cKMSKeyId = lens _cKMSKeyId (\ s a -> s{_cKMSKeyId = a});

-- | The volume type. This can be @gp2@ for General Purpose (SSD) volumes,
-- @io1@ for Provisioned IOPS (SSD) volumes, or @standard@ for Magnetic
-- volumes.
--
-- Default: @standard@
cVolumeType :: Lens' CreateVolume (Maybe VolumeType)
cVolumeType = lens _cVolumeType (\ s a -> s{_cVolumeType = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cDryRun :: Lens' CreateVolume (Maybe Bool)
cDryRun = lens _cDryRun (\ s a -> s{_cDryRun = a});

-- | The snapshot from which to create the volume.
cSnapshotId :: Lens' CreateVolume (Maybe Text)
cSnapshotId = lens _cSnapshotId (\ s a -> s{_cSnapshotId = a});

-- | The Availability Zone in which to create the volume. Use
-- DescribeAvailabilityZones to list the Availability Zones that are
-- currently available to you.
cAvailabilityZone :: Lens' CreateVolume Text
cAvailabilityZone = lens _cAvailabilityZone (\ s a -> s{_cAvailabilityZone = a});

instance AWSRequest CreateVolume where
        type Sv CreateVolume = EC2
        type Rs CreateVolume = Volume
        request = post
        response = receiveXML (\ s h x -> parseXML x)

instance ToHeaders CreateVolume where
        toHeaders = const mempty

instance ToPath CreateVolume where
        toPath = const "/"

instance ToQuery CreateVolume where
        toQuery CreateVolume'{..}
          = mconcat
              ["Action" =: ("CreateVolume" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "Size" =: _cSize, "Iops" =: _cIOPS,
               "Encrypted" =: _cEncrypted, "KmsKeyId" =: _cKMSKeyId,
               "VolumeType" =: _cVolumeType, "DryRun" =: _cDryRun,
               "SnapshotId" =: _cSnapshotId,
               "AvailabilityZone" =: _cAvailabilityZone]
