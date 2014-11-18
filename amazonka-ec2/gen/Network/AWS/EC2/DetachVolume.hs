{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DetachVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Detaches an Amazon EBS volume from an instance. Make sure to unmount any
-- file systems on the device within your operating system before detaching
-- the volume. Failure to do so results in the volume being stuck in a busy
-- state while detaching. If an Amazon EBS volume is the root device of an
-- instance, it can't be detached while the instance is running. To detach the
-- root volume, stop the instance first. If the root volume is detached from
-- an instance with an AWS Marketplace product code, then the AWS Marketplace
-- product codes from that volume are no longer associated with the instance.
-- For more information, see Detaching an Amazon EBS Volume in the Amazon
-- Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachVolume.html>
module Network.AWS.EC2.DetachVolume
    (
    -- * Request
      DetachVolume
    -- ** Request constructor
    , detachVolume
    -- ** Request lenses
    , dvDevice
    , dvDryRun
    , dvForce
    , dvInstanceId
    , dvVolumeId

    -- * Response
    , DetachVolumeResponse
    -- ** Response constructor
    , detachVolumeResponse
    -- ** Response lenses
    , dvrAttachTime
    , dvrDeleteOnTermination
    , dvrDevice
    , dvrInstanceId
    , dvrState
    , dvrVolumeId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DetachVolume = DetachVolume
    { _dvDevice     :: Maybe Text
    , _dvDryRun     :: Maybe Bool
    , _dvForce      :: Maybe Bool
    , _dvInstanceId :: Maybe Text
    , _dvVolumeId   :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DetachVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvDevice' @::@ 'Maybe' 'Text'
--
-- * 'dvDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvForce' @::@ 'Maybe' 'Bool'
--
-- * 'dvInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'dvVolumeId' @::@ 'Text'
--
detachVolume :: Text -- ^ 'dvVolumeId'
             -> DetachVolume
detachVolume p1 = DetachVolume
    { _dvVolumeId   = p1
    , _dvDryRun     = Nothing
    , _dvInstanceId = Nothing
    , _dvDevice     = Nothing
    , _dvForce      = Nothing
    }

-- | The device name.
dvDevice :: Lens' DetachVolume (Maybe Text)
dvDevice = lens _dvDevice (\s a -> s { _dvDevice = a })

dvDryRun :: Lens' DetachVolume (Maybe Bool)
dvDryRun = lens _dvDryRun (\s a -> s { _dvDryRun = a })

-- | Forces detachment if the previous detachment attempt did not occur
-- cleanly (for example, logging into an instance, unmounting the volume,
-- and detaching normally). This option can lead to data loss or a corrupted
-- file system. Use this option only as a last resort to detach a volume
-- from a failed instance. The instance won't have an opportunity to flush
-- file system caches or file system metadata. If you use this option, you
-- must perform file system check and repair procedures.
dvForce :: Lens' DetachVolume (Maybe Bool)
dvForce = lens _dvForce (\s a -> s { _dvForce = a })

-- | The ID of the instance.
dvInstanceId :: Lens' DetachVolume (Maybe Text)
dvInstanceId = lens _dvInstanceId (\s a -> s { _dvInstanceId = a })

-- | The ID of the volume.
dvVolumeId :: Lens' DetachVolume Text
dvVolumeId = lens _dvVolumeId (\s a -> s { _dvVolumeId = a })

data DetachVolumeResponse = DetachVolumeResponse
    { _dvrAttachTime          :: Maybe RFC822
    , _dvrDeleteOnTermination :: Maybe Bool
    , _dvrDevice              :: Maybe Text
    , _dvrInstanceId          :: Maybe Text
    , _dvrState               :: Maybe Text
    , _dvrVolumeId            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DetachVolumeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dvrDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'dvrDevice' @::@ 'Maybe' 'Text'
--
-- * 'dvrInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'dvrState' @::@ 'Maybe' 'Text'
--
-- * 'dvrVolumeId' @::@ 'Maybe' 'Text'
--
detachVolumeResponse :: DetachVolumeResponse
detachVolumeResponse = DetachVolumeResponse
    { _dvrVolumeId            = Nothing
    , _dvrInstanceId          = Nothing
    , _dvrDevice              = Nothing
    , _dvrState               = Nothing
    , _dvrAttachTime          = Nothing
    , _dvrDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
dvrAttachTime :: Lens' DetachVolumeResponse (Maybe UTCTime)
dvrAttachTime = lens _dvrAttachTime (\s a -> s { _dvrAttachTime = a })
    . mapping _Time

-- | Indicates whether the Amazon EBS volume is deleted on instance
-- termination.
dvrDeleteOnTermination :: Lens' DetachVolumeResponse (Maybe Bool)
dvrDeleteOnTermination =
    lens _dvrDeleteOnTermination (\s a -> s { _dvrDeleteOnTermination = a })

-- | The device name.
dvrDevice :: Lens' DetachVolumeResponse (Maybe Text)
dvrDevice = lens _dvrDevice (\s a -> s { _dvrDevice = a })

-- | The ID of the instance.
dvrInstanceId :: Lens' DetachVolumeResponse (Maybe Text)
dvrInstanceId = lens _dvrInstanceId (\s a -> s { _dvrInstanceId = a })

-- | The attachment state of the volume.
dvrState :: Lens' DetachVolumeResponse (Maybe Text)
dvrState = lens _dvrState (\s a -> s { _dvrState = a })

-- | The ID of the volume.
dvrVolumeId :: Lens' DetachVolumeResponse (Maybe Text)
dvrVolumeId = lens _dvrVolumeId (\s a -> s { _dvrVolumeId = a })

instance ToPath DetachVolume where
    toPath = const "/"

instance ToQuery DetachVolume

instance ToHeaders DetachVolume

instance AWSRequest DetachVolume where
    type Sv DetachVolume = EC2
    type Rs DetachVolume = DetachVolumeResponse

    request  = post "DetachVolume"
    response = xmlResponse

instance FromXML DetachVolumeResponse where
    parseXML c = DetachVolumeResponse
        <$> c .:? "attachTime"
        <*> c .:? "deleteOnTermination"
        <*> c .:? "device"
        <*> c .:? "instanceId"
        <*> c .:? "status"
        <*> c .:? "volumeId"
