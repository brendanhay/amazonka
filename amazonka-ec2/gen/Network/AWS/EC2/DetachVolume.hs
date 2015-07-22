{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DetachVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Detaches an EBS volume from an instance. Make sure to unmount any file
-- systems on the device within your operating system before detaching the
-- volume. Failure to do so results in the volume being stuck in a busy
-- state while detaching.
--
-- If an Amazon EBS volume is the root device of an instance, it can\'t be
-- detached while the instance is running. To detach the root volume, stop
-- the instance first.
--
-- When a volume with an AWS Marketplace product code is detached from an
-- instance, the product code is no longer associated with the instance.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-detaching-volume.html Detaching an Amazon EBS Volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVolume.html>
module Network.AWS.EC2.DetachVolume
    (
    -- * Request
      DetachVolume
    -- ** Request constructor
    , detachVolume
    -- ** Request lenses
    , dvrqInstanceId
    , dvrqForce
    , dvrqDevice
    , dvrqDryRun
    , dvrqVolumeId

    -- * Response
    , VolumeAttachment
    -- ** Response constructor
    , volumeAttachment
    -- ** Response lenses
    , dvrsInstanceId
    , dvrsDeleteOnTermination
    , dvrsState
    , dvrsDevice
    , dvrsVolumeId
    , dvrsAttachTime
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'detachVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrqInstanceId'
--
-- * 'dvrqForce'
--
-- * 'dvrqDevice'
--
-- * 'dvrqDryRun'
--
-- * 'dvrqVolumeId'
data DetachVolume = DetachVolume'
    { _dvrqInstanceId :: !(Maybe Text)
    , _dvrqForce      :: !(Maybe Bool)
    , _dvrqDevice     :: !(Maybe Text)
    , _dvrqDryRun     :: !(Maybe Bool)
    , _dvrqVolumeId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DetachVolume' smart constructor.
detachVolume :: Text -> DetachVolume
detachVolume pVolumeId =
    DetachVolume'
    { _dvrqInstanceId = Nothing
    , _dvrqForce = Nothing
    , _dvrqDevice = Nothing
    , _dvrqDryRun = Nothing
    , _dvrqVolumeId = pVolumeId
    }

-- | The ID of the instance.
dvrqInstanceId :: Lens' DetachVolume (Maybe Text)
dvrqInstanceId = lens _dvrqInstanceId (\ s a -> s{_dvrqInstanceId = a});

-- | Forces detachment if the previous detachment attempt did not occur
-- cleanly (for example, logging into an instance, unmounting the volume,
-- and detaching normally). This option can lead to data loss or a
-- corrupted file system. Use this option only as a last resort to detach a
-- volume from a failed instance. The instance won\'t have an opportunity
-- to flush file system caches or file system metadata. If you use this
-- option, you must perform file system check and repair procedures.
dvrqForce :: Lens' DetachVolume (Maybe Bool)
dvrqForce = lens _dvrqForce (\ s a -> s{_dvrqForce = a});

-- | The device name.
dvrqDevice :: Lens' DetachVolume (Maybe Text)
dvrqDevice = lens _dvrqDevice (\ s a -> s{_dvrqDevice = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvrqDryRun :: Lens' DetachVolume (Maybe Bool)
dvrqDryRun = lens _dvrqDryRun (\ s a -> s{_dvrqDryRun = a});

-- | The ID of the volume.
dvrqVolumeId :: Lens' DetachVolume Text
dvrqVolumeId = lens _dvrqVolumeId (\ s a -> s{_dvrqVolumeId = a});

instance AWSRequest DetachVolume where
        type Sv DetachVolume = EC2
        type Rs DetachVolume = VolumeAttachment
        request = post
        response = receiveXML (\ s h x -> parseXML x)

instance ToHeaders DetachVolume where
        toHeaders = const mempty

instance ToPath DetachVolume where
        toPath = const "/"

instance ToQuery DetachVolume where
        toQuery DetachVolume'{..}
          = mconcat
              ["Action" =: ("DetachVolume" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "InstanceId" =: _dvrqInstanceId,
               "Force" =: _dvrqForce, "Device" =: _dvrqDevice,
               "DryRun" =: _dvrqDryRun, "VolumeId" =: _dvrqVolumeId]
