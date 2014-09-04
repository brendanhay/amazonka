{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AttachVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Amazon EBS volume to a running or stopped instance and exposes
-- it to the instance with the specified device name. Encrypted Amazon EBS
-- volumes may only be attached to instances that support Amazon EBS
-- encryption. For more information, see Amazon EBS Encryption in the Amazon
-- Elastic Compute Cloud User Guide. For a list of supported device names, see
-- Attaching an Amazon EBS Volume to an Instance. Any device names that aren't
-- reserved for instance store volumes can be used for Amazon EBS volumes. For
-- more information, see Amazon EC2 Instance Store in the Amazon Elastic
-- Compute Cloud User Guide. If a volume has an AWS Marketplace product code:
-- The volume can only be attached as the root device of a stopped instance.
-- You must be subscribed to the AWS Marketplace code that is on the volume.
-- The configuration (instance type, operating system) of the instance must
-- support that specific AWS Marketplace code. For example, you cannot take a
-- volume from a Windows instance and attach it to a Linux instance. AWS
-- Marketplace product codes are copied from the volume to the instance. For
-- an overview of the AWS Marketplace, see
-- https://aws.amazon.com/marketplace/help/200900000. For more information
-- about how to use the AWS Marketplace, see AWS Marketplace. For more
-- information about Amazon EBS volumes, see Attaching Amazon EBS Volumes in
-- the Amazon Elastic Compute Cloud User Guide. Example 1 This example request
-- attaches the volume with the ID vol-1a2b3c4d to the instance with the ID
-- i-1a2b3c4d and exposes it as /dev/sdh.
-- https://ec2.amazonaws.com/?Action=AttachVolume &amp;VolumeId=vol-1a2b3c4d
-- &amp;InstanceId=i-1a2b3c4d &amp;Device=/dev/sdh &amp;AUTHPARAMS
-- &lt;AttachVolumeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;instanceId&gt;i-1a2b3c4d&lt;/instanceId&gt;
-- &lt;device&gt;/dev/sdh&lt;/device&gt;
-- &lt;status&gt;attaching&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/attachTime&gt;
-- &lt;/AttachVolumeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.AttachVolume
    (
    -- * Request
      AttachVolume
    -- ** Request constructor
    , mkAttachVolumeRequest
    -- ** Request lenses
    , avrVolumeId
    , avrInstanceId
    , avrDevice

    -- * Response
    , AttachVolumeResponse
    -- ** Response lenses
    , vaVolumeId
    , vaInstanceId
    , vaDevice
    , vaState
    , vaAttachTime
    , vaDeleteOnTermination
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachVolume' request.
mkAttachVolumeRequest :: Text -- ^ 'avrVolumeId'
                      -> Text -- ^ 'avrInstanceId'
                      -> Text -- ^ 'avrDevice'
                      -> AttachVolume
mkAttachVolumeRequest p1 p2 p3 = AttachVolume
    { _avrVolumeId = p1
    , _avrInstanceId = p2
    , _avrDevice = p3
    }
{-# INLINE mkAttachVolumeRequest #-}

data AttachVolume = AttachVolume
    { _avrVolumeId :: Text
      -- ^ The ID of the Amazon EBS volume. The volume and instance must be
      -- within the same Availability Zone.
    , _avrInstanceId :: Text
      -- ^ The ID of the instance.
    , _avrDevice :: Text
      -- ^ The device name to expose to the instance (for example, /dev/sdh
      -- or xvdh).
    } deriving (Show, Generic)

-- | The ID of the Amazon EBS volume. The volume and instance must be within the
-- same Availability Zone.
avrVolumeId :: Lens' AttachVolume (Text)
avrVolumeId = lens _avrVolumeId (\s a -> s { _avrVolumeId = a })
{-# INLINE avrVolumeId #-}

-- | The ID of the instance.
avrInstanceId :: Lens' AttachVolume (Text)
avrInstanceId = lens _avrInstanceId (\s a -> s { _avrInstanceId = a })
{-# INLINE avrInstanceId #-}

-- | The device name to expose to the instance (for example, /dev/sdh or xvdh).
avrDevice :: Lens' AttachVolume (Text)
avrDevice = lens _avrDevice (\s a -> s { _avrDevice = a })
{-# INLINE avrDevice #-}

instance ToQuery AttachVolume where
    toQuery = genericQuery def

data AttachVolumeResponse = AttachVolumeResponse
    { _vaVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vaInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vaDevice :: Maybe Text
      -- ^ The device name.
    , _vaState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    , _vaAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _vaDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    } deriving (Show, Generic)

-- | The ID of the volume.
vaVolumeId :: Lens' AttachVolumeResponse (Maybe Text)
vaVolumeId = lens _vaVolumeId (\s a -> s { _vaVolumeId = a })
{-# INLINE vaVolumeId #-}

-- | The ID of the instance.
vaInstanceId :: Lens' AttachVolumeResponse (Maybe Text)
vaInstanceId = lens _vaInstanceId (\s a -> s { _vaInstanceId = a })
{-# INLINE vaInstanceId #-}

-- | The device name.
vaDevice :: Lens' AttachVolumeResponse (Maybe Text)
vaDevice = lens _vaDevice (\s a -> s { _vaDevice = a })
{-# INLINE vaDevice #-}

-- | The attachment state of the volume.
vaState :: Lens' AttachVolumeResponse (Maybe VolumeAttachmentState)
vaState = lens _vaState (\s a -> s { _vaState = a })
{-# INLINE vaState #-}

-- | The time stamp when the attachment initiated.
vaAttachTime :: Lens' AttachVolumeResponse (Maybe ISO8601)
vaAttachTime = lens _vaAttachTime (\s a -> s { _vaAttachTime = a })
{-# INLINE vaAttachTime #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
vaDeleteOnTermination :: Lens' AttachVolumeResponse (Maybe Bool)
vaDeleteOnTermination = lens _vaDeleteOnTermination (\s a -> s { _vaDeleteOnTermination = a })
{-# INLINE vaDeleteOnTermination #-}

instance FromXML AttachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachVolume where
    type Sv AttachVolume = EC2
    type Rs AttachVolume = AttachVolumeResponse

    request = post "AttachVolume"
    response _ = xmlResponse
