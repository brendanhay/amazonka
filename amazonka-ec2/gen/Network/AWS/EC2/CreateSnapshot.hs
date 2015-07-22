{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreateSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of an EBS volume and stores it in Amazon S3. You can
-- use snapshots for backups, to make copies of EBS volumes, and to save
-- data before shutting down an instance.
--
-- When a snapshot is created, any AWS Marketplace product codes that are
-- associated with the source volume are propagated to the snapshot.
--
-- You can take a snapshot of an attached volume that is in use. However,
-- snapshots only capture data that has been written to your EBS volume at
-- the time the snapshot command is issued; this may exclude any data that
-- has been cached by any applications or the operating system. If you can
-- pause any file systems on the volume long enough to take a snapshot,
-- your snapshot should be complete. However, if you cannot pause all file
-- writes to the volume, you should unmount the volume from within the
-- instance, issue the snapshot command, and then remount the volume to
-- ensure a consistent and complete snapshot. You may remount and use your
-- volume while the snapshot status is @pending@.
--
-- To create a snapshot for EBS volumes that serve as root devices, you
-- should stop the instance before taking the snapshot.
--
-- Snapshots that are taken from encrypted volumes are automatically
-- encrypted. Volumes that are created from encrypted snapshots are also
-- automatically encrypted. Your encrypted volumes and any associated
-- snapshots always remain protected.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AmazonEBS.html Amazon Elastic Block Store>
-- and
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSnapshot.html>
module Network.AWS.EC2.CreateSnapshot
    (
    -- * Request
      CreateSnapshot
    -- ** Request constructor
    , createSnapshot
    -- ** Request lenses
    , cssDryRun
    , cssDescription
    , cssVolumeId

    -- * Response
    , Snapshot
    -- ** Response constructor
    , snapshot
    -- ** Response lenses
    , snaOwnerAlias
    , snaKMSKeyId
    , snaTags
    , snaSnapshotId
    , snaOwnerId
    , snaVolumeId
    , snaVolumeSize
    , snaDescription
    , snaStartTime
    , snaProgress
    , snaState
    , snaEncrypted
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cssDryRun'
--
-- * 'cssDescription'
--
-- * 'cssVolumeId'
data CreateSnapshot = CreateSnapshot'
    { _cssDryRun      :: !(Maybe Bool)
    , _cssDescription :: !(Maybe Text)
    , _cssVolumeId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateSnapshot' smart constructor.
createSnapshot :: Text -> CreateSnapshot
createSnapshot pVolumeId =
    CreateSnapshot'
    { _cssDryRun = Nothing
    , _cssDescription = Nothing
    , _cssVolumeId = pVolumeId
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cssDryRun :: Lens' CreateSnapshot (Maybe Bool)
cssDryRun = lens _cssDryRun (\ s a -> s{_cssDryRun = a});

-- | A description for the snapshot.
cssDescription :: Lens' CreateSnapshot (Maybe Text)
cssDescription = lens _cssDescription (\ s a -> s{_cssDescription = a});

-- | The ID of the EBS volume.
cssVolumeId :: Lens' CreateSnapshot Text
cssVolumeId = lens _cssVolumeId (\ s a -> s{_cssVolumeId = a});

instance AWSRequest CreateSnapshot where
        type Sv CreateSnapshot = EC2
        type Rs CreateSnapshot = Snapshot
        request = post
        response = receiveXML (\ s h x -> parseXML x)

instance ToHeaders CreateSnapshot where
        toHeaders = const mempty

instance ToPath CreateSnapshot where
        toPath = const "/"

instance ToQuery CreateSnapshot where
        toQuery CreateSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateSnapshot" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cssDryRun,
               "Description" =: _cssDescription,
               "VolumeId" =: _cssVolumeId]
