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
-- Module      : Network.AWS.EC2.DetachVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an EBS volume from an instance. Make sure to unmount any file systems on the device within your operating system before detaching the volume. Failure to do so can result in the volume becoming stuck in the @busy@ state while detaching. If this happens, detachment can be delayed indefinitely until you unmount the volume, force detachment, reboot the instance, or all three. If an EBS volume is the root device of an instance, it can't be detached while the instance is running. To detach the root volume, stop the instance first.
--
--
-- When a volume with an AWS Marketplace product code is detached from an instance, the product code is no longer associated with the instance.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-detaching-volume.html Detaching an Amazon EBS Volume> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DetachVolume
    (
    -- * Creating a Request
      detachVolume
    , DetachVolume
    -- * Request Lenses
    , dvInstanceId
    , dvForce
    , dvDevice
    , dvDryRun
    , dvVolumeId

    -- * Destructuring the Response
    , volumeAttachment
    , VolumeAttachment
    -- * Response Lenses
    , volInstanceId
    , volDeleteOnTermination
    , volState
    , volDevice
    , volVolumeId
    , volAttachTime
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DetachVolume.
--
--
--
-- /See:/ 'detachVolume' smart constructor.
data DetachVolume = DetachVolume'
  { _dvInstanceId :: !(Maybe Text)
  , _dvForce      :: !(Maybe Bool)
  , _dvDevice     :: !(Maybe Text)
  , _dvDryRun     :: !(Maybe Bool)
  , _dvVolumeId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvInstanceId' - The ID of the instance.
--
-- * 'dvForce' - Forces detachment if the previous detachment attempt did not occur cleanly (for example, logging into an instance, unmounting the volume, and detaching normally). This option can lead to data loss or a corrupted file system. Use this option only as a last resort to detach a volume from a failed instance. The instance won't have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures.
--
-- * 'dvDevice' - The device name.
--
-- * 'dvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvVolumeId' - The ID of the volume.
detachVolume
    :: Text -- ^ 'dvVolumeId'
    -> DetachVolume
detachVolume pVolumeId_ =
  DetachVolume'
    { _dvInstanceId = Nothing
    , _dvForce = Nothing
    , _dvDevice = Nothing
    , _dvDryRun = Nothing
    , _dvVolumeId = pVolumeId_
    }


-- | The ID of the instance.
dvInstanceId :: Lens' DetachVolume (Maybe Text)
dvInstanceId = lens _dvInstanceId (\ s a -> s{_dvInstanceId = a})

-- | Forces detachment if the previous detachment attempt did not occur cleanly (for example, logging into an instance, unmounting the volume, and detaching normally). This option can lead to data loss or a corrupted file system. Use this option only as a last resort to detach a volume from a failed instance. The instance won't have an opportunity to flush file system caches or file system metadata. If you use this option, you must perform file system check and repair procedures.
dvForce :: Lens' DetachVolume (Maybe Bool)
dvForce = lens _dvForce (\ s a -> s{_dvForce = a})

-- | The device name.
dvDevice :: Lens' DetachVolume (Maybe Text)
dvDevice = lens _dvDevice (\ s a -> s{_dvDevice = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvDryRun :: Lens' DetachVolume (Maybe Bool)
dvDryRun = lens _dvDryRun (\ s a -> s{_dvDryRun = a})

-- | The ID of the volume.
dvVolumeId :: Lens' DetachVolume Text
dvVolumeId = lens _dvVolumeId (\ s a -> s{_dvVolumeId = a})

instance AWSRequest DetachVolume where
        type Rs DetachVolume = VolumeAttachment
        request = postQuery ec2
        response = receiveXML (\ s h x -> parseXML x)

instance Hashable DetachVolume where

instance NFData DetachVolume where

instance ToHeaders DetachVolume where
        toHeaders = const mempty

instance ToPath DetachVolume where
        toPath = const "/"

instance ToQuery DetachVolume where
        toQuery DetachVolume'{..}
          = mconcat
              ["Action" =: ("DetachVolume" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "InstanceId" =: _dvInstanceId, "Force" =: _dvForce,
               "Device" =: _dvDevice, "DryRun" =: _dvDryRun,
               "VolumeId" =: _dvVolumeId]
