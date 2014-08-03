{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.EC2.V2014_06_15.DetachVolume where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DetachVolume' request.
detachVolume :: Text -- ^ '_dvrVolumeId'
             -> DetachVolume
detachVolume p1 = DetachVolume
    { _dvrVolumeId = p1
    , _dvrForce = Nothing
    , _dvrDryRun = Nothing
    , _dvrInstanceId = Nothing
    , _dvrDevice = Nothing
    }

data DetachVolume = DetachVolume
    { _dvrVolumeId :: Text
      -- ^ The ID of the volume.
    , _dvrForce :: Maybe Bool
      -- ^ Forces detachment if the previous detachment attempt did not
      -- occur cleanly (for example, logging into an instance, unmounting
      -- the volume, and detaching normally). This option can lead to data
      -- loss or a corrupted file system. Use this option only as a last
      -- resort to detach a volume from a failed instance. The instance
      -- won't have an opportunity to flush file system caches or file
      -- system metadata. If you use this option, you must perform file
      -- system check and repair procedures.
    , _dvrDryRun :: Maybe Bool
      -- ^ 
    , _dvrInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _dvrDevice :: Maybe Text
      -- ^ The device name.
    } deriving (Generic)

makeLenses ''DetachVolume

instance ToQuery DetachVolume where
    toQuery = genericToQuery def

data DetachVolumeResponse = DetachVolumeResponse
    { _vbDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _vbAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _vbInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vbDevice :: Maybe Text
      -- ^ The device name.
    , _vbVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vbState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    } deriving (Generic)

makeLenses ''DetachVolumeResponse

instance FromXML DetachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachVolume where
    type Sv DetachVolume = EC2
    type Rs DetachVolume = DetachVolumeResponse

    request = post "DetachVolume"
    response _ = xmlResponse
