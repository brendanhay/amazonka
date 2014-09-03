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
    , attachVolume
    -- ** Request lenses
    , avrVolumeId
    , avrInstanceId
    , avrDevice

    -- * Response
    , AttachVolumeResponse
    -- ** Response lenses
    , vaDeleteOnTermination
    , vaAttachTime
    , vaVolumeId
    , vaInstanceId
    , vaDevice
    , vaState
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AttachVolume' request.
attachVolume :: Text -- ^ 'avrVolumeId'
             -> Text -- ^ 'avrInstanceId'
             -> Text -- ^ 'avrDevice'
             -> AttachVolume
attachVolume p1 p2 p3 = AttachVolume
    { _avrVolumeId = p1
    , _avrInstanceId = p2
    , _avrDevice = p3
    }

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
avrVolumeId
    :: Functor f
    => (Text
    -> f (Text))
    -> AttachVolume
    -> f AttachVolume
avrVolumeId f x =
    (\y -> x { _avrVolumeId = y })
       <$> f (_avrVolumeId x)
{-# INLINE avrVolumeId #-}

-- | The ID of the instance.
avrInstanceId
    :: Functor f
    => (Text
    -> f (Text))
    -> AttachVolume
    -> f AttachVolume
avrInstanceId f x =
    (\y -> x { _avrInstanceId = y })
       <$> f (_avrInstanceId x)
{-# INLINE avrInstanceId #-}

-- | The device name to expose to the instance (for example, /dev/sdh or xvdh).
avrDevice
    :: Functor f
    => (Text
    -> f (Text))
    -> AttachVolume
    -> f AttachVolume
avrDevice f x =
    (\y -> x { _avrDevice = y })
       <$> f (_avrDevice x)
{-# INLINE avrDevice #-}

instance ToQuery AttachVolume where
    toQuery = genericQuery def

data AttachVolumeResponse = AttachVolumeResponse
    { _vaDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _vaAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _vaVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vaInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vaDevice :: Maybe Text
      -- ^ The device name.
    , _vaState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    } deriving (Show, Generic)

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
vaDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> AttachVolumeResponse
    -> f AttachVolumeResponse
vaDeleteOnTermination f x =
    (\y -> x { _vaDeleteOnTermination = y })
       <$> f (_vaDeleteOnTermination x)
{-# INLINE vaDeleteOnTermination #-}

-- | The time stamp when the attachment initiated.
vaAttachTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> AttachVolumeResponse
    -> f AttachVolumeResponse
vaAttachTime f x =
    (\y -> x { _vaAttachTime = y })
       <$> f (_vaAttachTime x)
{-# INLINE vaAttachTime #-}

-- | The ID of the volume.
vaVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttachVolumeResponse
    -> f AttachVolumeResponse
vaVolumeId f x =
    (\y -> x { _vaVolumeId = y })
       <$> f (_vaVolumeId x)
{-# INLINE vaVolumeId #-}

-- | The ID of the instance.
vaInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttachVolumeResponse
    -> f AttachVolumeResponse
vaInstanceId f x =
    (\y -> x { _vaInstanceId = y })
       <$> f (_vaInstanceId x)
{-# INLINE vaInstanceId #-}

-- | The device name.
vaDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AttachVolumeResponse
    -> f AttachVolumeResponse
vaDevice f x =
    (\y -> x { _vaDevice = y })
       <$> f (_vaDevice x)
{-# INLINE vaDevice #-}

-- | The attachment state of the volume.
vaState
    :: Functor f
    => (Maybe VolumeAttachmentState
    -> f (Maybe VolumeAttachmentState))
    -> AttachVolumeResponse
    -> f AttachVolumeResponse
vaState f x =
    (\y -> x { _vaState = y })
       <$> f (_vaState x)
{-# INLINE vaState #-}

instance FromXML AttachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AttachVolume where
    type Sv AttachVolume = EC2
    type Rs AttachVolume = AttachVolumeResponse

    request = post "AttachVolume"
    response _ = xmlResponse
