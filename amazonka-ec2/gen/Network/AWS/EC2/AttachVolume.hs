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
-- Module      : Network.AWS.EC2.AttachVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an EBS volume to a running or stopped instance and exposes it
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
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVolume.html AWS API Reference> for AttachVolume.
module Network.AWS.EC2.AttachVolume
    (
    -- * Creating a Request
      attachVolume
    , AttachVolume
    -- * Request Lenses
    , avDryRun
    , avVolumeId
    , avInstanceId
    , avDevice

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

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'attachVolume' smart constructor.
data AttachVolume = AttachVolume'
    { _avDryRun     :: !(Maybe Bool)
    , _avVolumeId   :: !Text
    , _avInstanceId :: !Text
    , _avDevice     :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AttachVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avDryRun'
--
-- * 'avVolumeId'
--
-- * 'avInstanceId'
--
-- * 'avDevice'
attachVolume
    :: Text -- ^ 'avVolumeId'
    -> Text -- ^ 'avInstanceId'
    -> Text -- ^ 'avDevice'
    -> AttachVolume
attachVolume pVolumeId_ pInstanceId_ pDevice_ =
    AttachVolume'
    { _avDryRun = Nothing
    , _avVolumeId = pVolumeId_
    , _avInstanceId = pInstanceId_
    , _avDevice = pDevice_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
avDryRun :: Lens' AttachVolume (Maybe Bool)
avDryRun = lens _avDryRun (\ s a -> s{_avDryRun = a});

-- | The ID of the EBS volume. The volume and instance must be within the
-- same Availability Zone.
avVolumeId :: Lens' AttachVolume Text
avVolumeId = lens _avVolumeId (\ s a -> s{_avVolumeId = a});

-- | The ID of the instance.
avInstanceId :: Lens' AttachVolume Text
avInstanceId = lens _avInstanceId (\ s a -> s{_avInstanceId = a});

-- | The device name to expose to the instance (for example, '\/dev\/sdh' or
-- 'xvdh').
avDevice :: Lens' AttachVolume Text
avDevice = lens _avDevice (\ s a -> s{_avDevice = a});

instance AWSRequest AttachVolume where
        type Rs AttachVolume = VolumeAttachment
        request = postQuery eC2
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
