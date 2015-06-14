{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.AttachVolume
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

-- | Attaches an EBS volume to a running or stopped instance and exposes it
-- to the instance with the specified device name.
--
-- Encrypted EBS volumes may only be attached to instances that support
-- Amazon EBS encryption. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For a list of supported device names, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html Attaching an EBS Volume to an Instance>.
-- Any device names that aren\'t reserved for instance store volumes can be
-- used for EBS volumes. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2 Instance Store>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If a volume has an AWS Marketplace product code:
--
-- -   The volume can be attached only to a stopped instance.
-- -   AWS Marketplace product codes are copied from the volume to the
--     instance.
-- -   You must be subscribed to the product.
-- -   The instance type and operating system of the instance must support
--     the product. For example, you can\'t detach a volume from a Windows
--     instance and attach it to a Linux instance.
--
-- For an overview of the AWS Marketplace, see
-- <https://aws.amazon.com/marketplace/help/200900000 Introducing AWS Marketplace>.
--
-- For more information about EBS volumes, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html Attaching Amazon EBS Volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVolume.html>
module Network.AWS.EC2.AttachVolume
    (
    -- * Request
      AttachVolume
    -- ** Request constructor
    , attachVolume
    -- ** Request lenses
    , avDryRun
    , avVolumeId
    , avInstanceId
    , avDevice

    -- * Response
    , VolumeAttachment
    -- ** Response constructor
    , volumeAttachment
    -- ** Response lenses
    , vInstanceId
    , vDeleteOnTermination
    , vState
    , vDevice
    , vVolumeId
    , vAttachTime
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'attachVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avDryRun'
--
-- * 'avVolumeId'
--
-- * 'avInstanceId'
--
-- * 'avDevice'
data AttachVolume = AttachVolume'{_avDryRun :: Maybe Bool, _avVolumeId :: Text, _avInstanceId :: Text, _avDevice :: Text} deriving (Eq, Read, Show)

-- | 'AttachVolume' smart constructor.
attachVolume :: Text -> Text -> Text -> AttachVolume
attachVolume pVolumeId pInstanceId pDevice = AttachVolume'{_avDryRun = Nothing, _avVolumeId = pVolumeId, _avInstanceId = pInstanceId, _avDevice = pDevice};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
avDryRun :: Lens' AttachVolume (Maybe Bool)
avDryRun = lens _avDryRun (\ s a -> s{_avDryRun = a});

-- | The ID of the EBS volume. The volume and instance must be within the
-- same Availability Zone.
avVolumeId :: Lens' AttachVolume Text
avVolumeId = lens _avVolumeId (\ s a -> s{_avVolumeId = a});

-- | The ID of the instance.
avInstanceId :: Lens' AttachVolume Text
avInstanceId = lens _avInstanceId (\ s a -> s{_avInstanceId = a});

-- | The device name to expose to the instance (for example, @\/dev\/sdh@ or
-- @xvdh@).
avDevice :: Lens' AttachVolume Text
avDevice = lens _avDevice (\ s a -> s{_avDevice = a});

instance AWSRequest AttachVolume where
        type Sv AttachVolume = EC2
        type Rs AttachVolume = VolumeAttachment
        request = post
        response = receiveXML (\ s h x -> parseXML x)

instance ToHeaders AttachVolume where
        toHeaders = const mempty

instance ToPath AttachVolume where
        toPath = const "/"

instance ToQuery AttachVolume where
        toQuery AttachVolume'{..}
          = mconcat
              ["Action" =: ("AttachVolume" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _avDryRun, "VolumeId" =: _avVolumeId,
               "InstanceId" =: _avInstanceId, "Device" =: _avDevice]
