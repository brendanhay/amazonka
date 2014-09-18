{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.AttachVolume
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
module Network.AWS.EC2.AttachVolume
    (
    -- * Request
      AttachVolume
    -- ** Request constructor
    , attachVolume
    -- ** Request lenses
    , avVolumeId
    , avInstanceId
    , avDevice

    -- * Response
    , AttachVolumeResponse
    -- ** Response constructor
    , attachVolumeResponse
    -- ** Response lenses
    , avrVolumeId
    , avrInstanceId
    , avrDevice
    , avrState
    , avrAttachTime
    , avrDeleteOnTermination
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data AttachVolume = AttachVolume
    { _avVolumeId :: Text
    , _avInstanceId :: Text
    , _avDevice :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachVolume' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Text@
--
-- * @InstanceId ::@ @Text@
--
-- * @Device ::@ @Text@
--
attachVolume :: Text -- ^ 'avVolumeId'
               -> Text -- ^ 'avInstanceId'
               -> Text -- ^ 'avDevice'
               -> AttachVolume
attachVolume p1 p2 p3 = AttachVolume
    { _avVolumeId = p1
    , _avInstanceId = p2
    , _avDevice = p3
    }

-- | The ID of the Amazon EBS volume. The volume and instance must be within the
-- same Availability Zone.
avVolumeId :: Lens' AttachVolume Text
avVolumeId = lens _avVolumeId (\s a -> s { _avVolumeId = a })

-- | The ID of the instance.
avInstanceId :: Lens' AttachVolume Text
avInstanceId = lens _avInstanceId (\s a -> s { _avInstanceId = a })

-- | The device name to expose to the instance (for example, /dev/sdh or xvdh).
avDevice :: Lens' AttachVolume Text
avDevice = lens _avDevice (\s a -> s { _avDevice = a })

instance ToQuery AttachVolume where
    toQuery = genericQuery def

data AttachVolumeResponse = AttachVolumeResponse
    { _avrVolumeId :: Maybe Text
    , _avrInstanceId :: Maybe Text
    , _avrDevice :: Maybe Text
    , _avrState :: Maybe VolumeAttachmentState
    , _avrAttachTime :: Maybe ISO8601
    , _avrDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachVolumeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Device ::@ @Maybe Text@
--
-- * @State ::@ @Maybe VolumeAttachmentState@
--
-- * @AttachTime ::@ @Maybe ISO8601@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
attachVolumeResponse :: AttachVolumeResponse
attachVolumeResponse = AttachVolumeResponse
    { _avrVolumeId = Nothing
    , _avrInstanceId = Nothing
    , _avrDevice = Nothing
    , _avrState = Nothing
    , _avrAttachTime = Nothing
    , _avrDeleteOnTermination = Nothing
    }

-- | The ID of the volume.
avrVolumeId :: Lens' AttachVolumeResponse (Maybe Text)
avrVolumeId = lens _avrVolumeId (\s a -> s { _avrVolumeId = a })

-- | The ID of the instance.
avrInstanceId :: Lens' AttachVolumeResponse (Maybe Text)
avrInstanceId = lens _avrInstanceId (\s a -> s { _avrInstanceId = a })

-- | The device name.
avrDevice :: Lens' AttachVolumeResponse (Maybe Text)
avrDevice = lens _avrDevice (\s a -> s { _avrDevice = a })

-- | The attachment state of the volume.
avrState :: Lens' AttachVolumeResponse (Maybe VolumeAttachmentState)
avrState = lens _avrState (\s a -> s { _avrState = a })

-- | The time stamp when the attachment initiated.
avrAttachTime :: Lens' AttachVolumeResponse (Maybe ISO8601)
avrAttachTime = lens _avrAttachTime (\s a -> s { _avrAttachTime = a })

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
avrDeleteOnTermination :: Lens' AttachVolumeResponse (Maybe Bool)
avrDeleteOnTermination =
    lens _avrDeleteOnTermination (\s a -> s { _avrDeleteOnTermination = a })

instance FromXML AttachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachVolume where
    type Sv AttachVolume = EC2
    type Rs AttachVolume = AttachVolumeResponse

    request = post "AttachVolume"
    response _ = xmlResponse
