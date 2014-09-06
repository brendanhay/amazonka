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
    , mkAttachVolume
    -- ** Request lenses
    , avVolumeId
    , avInstanceId
    , avDevice

    -- * Response
    , AttachVolumeResponse
    -- ** Response lenses
    , avrsVolumeId
    , avrsInstanceId
    , avrsDevice
    , avrsState
    , avrsAttachTime
    , avrsDeleteOnTermination
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data AttachVolume = AttachVolume
    { _avVolumeId :: Text
    , _avInstanceId :: Text
    , _avDevice :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AttachVolume' request.
mkAttachVolume :: Text -- ^ 'avVolumeId'
               -> Text -- ^ 'avInstanceId'
               -> Text -- ^ 'avDevice'
               -> AttachVolume
mkAttachVolume p1 p2 p3 = AttachVolume
    { _avVolumeId = p1
    , _avInstanceId = p2
    , _avDevice = p3
    }
{-# INLINE mkAttachVolume #-}

-- | The ID of the Amazon EBS volume. The volume and instance must be within the
-- same Availability Zone.
avVolumeId :: Lens' AttachVolume Text
avVolumeId = lens _avVolumeId (\s a -> s { _avVolumeId = a })
{-# INLINE avVolumeId #-}

-- | The ID of the instance.
avInstanceId :: Lens' AttachVolume Text
avInstanceId = lens _avInstanceId (\s a -> s { _avInstanceId = a })
{-# INLINE avInstanceId #-}

-- | The device name to expose to the instance (for example, /dev/sdh or xvdh).
avDevice :: Lens' AttachVolume Text
avDevice = lens _avDevice (\s a -> s { _avDevice = a })
{-# INLINE avDevice #-}

instance ToQuery AttachVolume where
    toQuery = genericQuery def

-- | 
data AttachVolumeResponse = AttachVolumeResponse
    { _avrsVolumeId :: Maybe Text
    , _avrsInstanceId :: Maybe Text
    , _avrsDevice :: Maybe Text
    , _avrsState :: Maybe VolumeAttachmentState
    , _avrsAttachTime :: Maybe ISO8601
    , _avrsDeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | The ID of the volume.
avrsVolumeId :: Lens' AttachVolumeResponse (Maybe Text)
avrsVolumeId = lens _avrsVolumeId (\s a -> s { _avrsVolumeId = a })
{-# INLINE avrsVolumeId #-}

-- | The ID of the instance.
avrsInstanceId :: Lens' AttachVolumeResponse (Maybe Text)
avrsInstanceId = lens _avrsInstanceId (\s a -> s { _avrsInstanceId = a })
{-# INLINE avrsInstanceId #-}

-- | The device name.
avrsDevice :: Lens' AttachVolumeResponse (Maybe Text)
avrsDevice = lens _avrsDevice (\s a -> s { _avrsDevice = a })
{-# INLINE avrsDevice #-}

-- | The attachment state of the volume.
avrsState :: Lens' AttachVolumeResponse (Maybe VolumeAttachmentState)
avrsState = lens _avrsState (\s a -> s { _avrsState = a })
{-# INLINE avrsState #-}

-- | The time stamp when the attachment initiated.
avrsAttachTime :: Lens' AttachVolumeResponse (Maybe ISO8601)
avrsAttachTime = lens _avrsAttachTime (\s a -> s { _avrsAttachTime = a })
{-# INLINE avrsAttachTime #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
avrsDeleteOnTermination :: Lens' AttachVolumeResponse (Maybe Bool)
avrsDeleteOnTermination =
    lens _avrsDeleteOnTermination
         (\s a -> s { _avrsDeleteOnTermination = a })
{-# INLINE avrsDeleteOnTermination #-}

instance FromXML AttachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachVolume where
    type Sv AttachVolume = EC2
    type Rs AttachVolume = AttachVolumeResponse

    request = post "AttachVolume"
    response _ = xmlResponse
