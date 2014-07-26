{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DetachVolume
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
module Network.AWS.EC2.V2014_05_01.DetachVolume where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DetachVolume = DetachVolume
    { _dvrVolumeId :: Text
      -- ^ The ID of the volume.
    , _dvrForce :: Bool
      -- ^ Forces detachment if the previous detachment attempt did not
      -- occur cleanly (for example, logging into an instance, unmounting
      -- the volume, and detaching normally). This option can lead to data
      -- loss or a corrupted file system. Use this option only as a last
      -- resort to detach a volume from a failed instance. The instance
      -- won't have an opportunity to flush file system caches or file
      -- system metadata. If you use this option, you must perform file
      -- system check and repair procedures.
    , _dvrDryRun :: Bool
      -- ^ 
    , _dvrInstanceId :: Text
      -- ^ The ID of the instance.
    , _dvrDevice :: Text
      -- ^ The device name.
    } deriving (Generic)

instance ToQuery DetachVolume where
    toQuery = genericToQuery def

instance AWSRequest DetachVolume where
    type Sv DetachVolume = EC2
    type Rs DetachVolume = DetachVolumeResponse

    request = post "DetachVolume"

    response _ = xmlResponse

data DetachVolumeResponse = DetachVolumeResponse
    { _vaDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _vaAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _vaInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vaDevice :: Maybe Text
      -- ^ The device name.
    , _vaVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vaState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    } deriving (Generic)

instance FromXML DetachVolumeResponse where
    fromXMLOptions = xmlOptions
