{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DetachVolume
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
-- Elastic Compute Cloud User Guide. Example This example detaches volume
-- vol-1a2b3c4d. https://ec2.amazonaws.com/?Action=DetachVolume
-- &amp;VolumeId=vol-1a2b3c4d &amp;AUTHPARAMS &lt;DetachVolumeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;instanceId&gt;i-1a2b3c4d&lt;/instanceId&gt;
-- &lt;device&gt;/dev/sdh&lt;/device&gt;
-- &lt;status&gt;detaching&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/attachTime&gt;
-- &lt;/DetachVolumeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DetachVolume
    (
    -- * Request
      DetachVolume
    -- ** Request constructor
    , mkDetachVolume
    -- ** Request lenses
    , dv4VolumeId
    , dv4InstanceId
    , dv4Device
    , dv4Force

    -- * Response
    , DetachVolumeResponse
    -- ** Response lenses
    , dvrs1VolumeId
    , dvrs1InstanceId
    , dvrs1Device
    , dvrs1State
    , dvrs1AttachTime
    , dvrs1DeleteOnTermination
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DetachVolume = DetachVolume
    { _dv4VolumeId :: Text
    , _dv4InstanceId :: Maybe Text
    , _dv4Device :: Maybe Text
    , _dv4Force :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachVolume' request.
mkDetachVolume :: Text -- ^ 'dv4VolumeId'
               -> DetachVolume
mkDetachVolume p1 = DetachVolume
    { _dv4VolumeId = p1
    , _dv4InstanceId = Nothing
    , _dv4Device = Nothing
    , _dv4Force = Nothing
    }
{-# INLINE mkDetachVolume #-}

-- | The ID of the volume.
dv4VolumeId :: Lens' DetachVolume Text
dv4VolumeId = lens _dv4VolumeId (\s a -> s { _dv4VolumeId = a })
{-# INLINE dv4VolumeId #-}

-- | The ID of the instance.
dv4InstanceId :: Lens' DetachVolume (Maybe Text)
dv4InstanceId = lens _dv4InstanceId (\s a -> s { _dv4InstanceId = a })
{-# INLINE dv4InstanceId #-}

-- | The device name.
dv4Device :: Lens' DetachVolume (Maybe Text)
dv4Device = lens _dv4Device (\s a -> s { _dv4Device = a })
{-# INLINE dv4Device #-}

-- | Forces detachment if the previous detachment attempt did not occur cleanly
-- (for example, logging into an instance, unmounting the volume, and
-- detaching normally). This option can lead to data loss or a corrupted file
-- system. Use this option only as a last resort to detach a volume from a
-- failed instance. The instance won't have an opportunity to flush file
-- system caches or file system metadata. If you use this option, you must
-- perform file system check and repair procedures.
dv4Force :: Lens' DetachVolume (Maybe Bool)
dv4Force = lens _dv4Force (\s a -> s { _dv4Force = a })
{-# INLINE dv4Force #-}

instance ToQuery DetachVolume where
    toQuery = genericQuery def

-- | 
data DetachVolumeResponse = DetachVolumeResponse
    { _dvrs1VolumeId :: Maybe Text
    , _dvrs1InstanceId :: Maybe Text
    , _dvrs1Device :: Maybe Text
    , _dvrs1State :: Maybe VolumeAttachmentState
    , _dvrs1AttachTime :: Maybe ISO8601
    , _dvrs1DeleteOnTermination :: Maybe Bool
    } deriving (Show, Generic)

-- | The ID of the volume.
dvrs1VolumeId :: Lens' DetachVolumeResponse (Maybe Text)
dvrs1VolumeId = lens _dvrs1VolumeId (\s a -> s { _dvrs1VolumeId = a })
{-# INLINE dvrs1VolumeId #-}

-- | The ID of the instance.
dvrs1InstanceId :: Lens' DetachVolumeResponse (Maybe Text)
dvrs1InstanceId = lens _dvrs1InstanceId (\s a -> s { _dvrs1InstanceId = a })
{-# INLINE dvrs1InstanceId #-}

-- | The device name.
dvrs1Device :: Lens' DetachVolumeResponse (Maybe Text)
dvrs1Device = lens _dvrs1Device (\s a -> s { _dvrs1Device = a })
{-# INLINE dvrs1Device #-}

-- | The attachment state of the volume.
dvrs1State :: Lens' DetachVolumeResponse (Maybe VolumeAttachmentState)
dvrs1State = lens _dvrs1State (\s a -> s { _dvrs1State = a })
{-# INLINE dvrs1State #-}

-- | The time stamp when the attachment initiated.
dvrs1AttachTime :: Lens' DetachVolumeResponse (Maybe ISO8601)
dvrs1AttachTime = lens _dvrs1AttachTime (\s a -> s { _dvrs1AttachTime = a })
{-# INLINE dvrs1AttachTime #-}

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
dvrs1DeleteOnTermination :: Lens' DetachVolumeResponse (Maybe Bool)
dvrs1DeleteOnTermination =
    lens _dvrs1DeleteOnTermination
         (\s a -> s { _dvrs1DeleteOnTermination = a })
{-# INLINE dvrs1DeleteOnTermination #-}

instance FromXML DetachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachVolume where
    type Sv DetachVolume = EC2
    type Rs DetachVolume = DetachVolumeResponse

    request = post "DetachVolume"
    response _ = xmlResponse
