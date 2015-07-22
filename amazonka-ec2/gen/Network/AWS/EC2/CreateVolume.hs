{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an EBS volume that can be attached to an instance in the same
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
    , crqSize
    , crqIOPS
    , crqEncrypted
    , crqKMSKeyId
    , crqVolumeType
    , crqDryRun
    , crqSnapshotId
    , crqAvailabilityZone

    -- * Response
    , Volume
    -- ** Response constructor
    , volume
    -- ** Response lenses
    , crsAttachments
    , crsIOPS
    , crsKMSKeyId
    , crsTags
    , crsAvailabilityZone
    , crsCreateTime
    , crsEncrypted
    , crsSize
    , crsSnapshotId
    , crsState
    , crsVolumeId
    , crsVolumeType
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crqSize'
--
-- * 'crqIOPS'
--
-- * 'crqEncrypted'
--
-- * 'crqKMSKeyId'
--
-- * 'crqVolumeType'
--
-- * 'crqDryRun'
--
-- * 'crqSnapshotId'
--
-- * 'crqAvailabilityZone'
data CreateVolume = CreateVolume'
    { _crqSize             :: !(Maybe Int)
    , _crqIOPS             :: !(Maybe Int)
    , _crqEncrypted        :: !(Maybe Bool)
    , _crqKMSKeyId         :: !(Maybe Text)
    , _crqVolumeType       :: !(Maybe VolumeType)
    , _crqDryRun           :: !(Maybe Bool)
    , _crqSnapshotId       :: !(Maybe Text)
    , _crqAvailabilityZone :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVolume' smart constructor.
createVolume :: Text -> CreateVolume
createVolume pAvailabilityZone =
    CreateVolume'
    { _crqSize = Nothing
    , _crqIOPS = Nothing
    , _crqEncrypted = Nothing
    , _crqKMSKeyId = Nothing
    , _crqVolumeType = Nothing
    , _crqDryRun = Nothing
    , _crqSnapshotId = Nothing
    , _crqAvailabilityZone = pAvailabilityZone
    }

-- | The size of the volume, in GiBs.
--
-- Constraints: @1-1024@ for @standard@ volumes, @1-16384@ for @gp2@
-- volumes, and @4-16384@ for @io1@ volumes. If you specify a snapshot, the
-- volume size must be equal to or larger than the snapshot size.
--
-- Default: If you\'re creating the volume from a snapshot and don\'t
-- specify a volume size, the default is the snapshot size.
crqSize :: Lens' CreateVolume (Maybe Int)
crqSize = lens _crqSize (\ s a -> s{_crqSize = a});

-- | Only valid for Provisioned IOPS (SSD) volumes. The number of I\/O
-- operations per second (IOPS) to provision for the volume, with a maximum
-- ratio of 30 IOPS\/GiB.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes
crqIOPS :: Lens' CreateVolume (Maybe Int)
crqIOPS = lens _crqIOPS (\ s a -> s{_crqIOPS = a});

-- | Specifies whether the volume should be encrypted. Encrypted Amazon EBS
-- volumes may only be attached to instances that support Amazon EBS
-- encryption. Volumes that are created from encrypted snapshots are
-- automatically encrypted. There is no way to create an encrypted volume
-- from an unencrypted snapshot or vice versa. If your AMI uses encrypted
-- volumes, you can only launch it on supported instance types. For more
-- information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
crqEncrypted :: Lens' CreateVolume (Maybe Bool)
crqEncrypted = lens _crqEncrypted (\ s a -> s{_crqEncrypted = a});

-- | The full ARN of the AWS Key Management Service (KMS) Customer Master Key
-- (CMK) to use when creating the encrypted volume. This parameter is only
-- required if you want to use a non-default CMK; if this parameter is not
-- specified, the default CMK for EBS is used. The ARN contains the
-- @arn:aws:kms@ namespace, followed by the region of the CMK, the AWS
-- account ID of the CMK owner, the @key@ namespace, and then the CMK ID.
-- For example,
-- arn:aws:kms:/us-east-1/:/012345678910/:key\//abcd1234-a123-456a-a12b-a123b4cd56ef/.
-- If a @KmsKeyId@ is specified, the @Encrypted@ flag must also be set.
crqKMSKeyId :: Lens' CreateVolume (Maybe Text)
crqKMSKeyId = lens _crqKMSKeyId (\ s a -> s{_crqKMSKeyId = a});

-- | The volume type. This can be @gp2@ for General Purpose (SSD) volumes,
-- @io1@ for Provisioned IOPS (SSD) volumes, or @standard@ for Magnetic
-- volumes.
--
-- Default: @standard@
crqVolumeType :: Lens' CreateVolume (Maybe VolumeType)
crqVolumeType = lens _crqVolumeType (\ s a -> s{_crqVolumeType = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
crqDryRun :: Lens' CreateVolume (Maybe Bool)
crqDryRun = lens _crqDryRun (\ s a -> s{_crqDryRun = a});

-- | The snapshot from which to create the volume.
crqSnapshotId :: Lens' CreateVolume (Maybe Text)
crqSnapshotId = lens _crqSnapshotId (\ s a -> s{_crqSnapshotId = a});

-- | The Availability Zone in which to create the volume. Use
-- DescribeAvailabilityZones to list the Availability Zones that are
-- currently available to you.
crqAvailabilityZone :: Lens' CreateVolume Text
crqAvailabilityZone = lens _crqAvailabilityZone (\ s a -> s{_crqAvailabilityZone = a});

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
               "Size" =: _crqSize, "Iops" =: _crqIOPS,
               "Encrypted" =: _crqEncrypted,
               "KmsKeyId" =: _crqKMSKeyId,
               "VolumeType" =: _crqVolumeType,
               "DryRun" =: _crqDryRun,
               "SnapshotId" =: _crqSnapshotId,
               "AvailabilityZone" =: _crqAvailabilityZone]
