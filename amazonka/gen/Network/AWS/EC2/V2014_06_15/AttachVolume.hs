{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.EC2.V2014_06_15.AttachVolume where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'AttachVolume' request.
attachVolume :: Text -- ^ '_avrInstanceId'
             -> Text -- ^ '_avrDevice'
             -> Text -- ^ '_avrVolumeId'
             -> AttachVolume
attachVolume p1 p2 p3 = AttachVolume
    { _avrInstanceId = p1
    , _avrDevice = p2
    , _avrVolumeId = p3
    , _avrDryRun = Nothing
    }

data AttachVolume = AttachVolume
    { _avrInstanceId :: Text
      -- ^ The ID of the instance.
    , _avrDevice :: Text
      -- ^ The device name to expose to the instance (for example, /dev/sdh
      -- or xvdh).
    , _avrVolumeId :: Text
      -- ^ The ID of the Amazon EBS volume. The volume and instance must be
      -- within the same Availability Zone.
    , _avrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery AttachVolume where
    toQuery = genericToQuery def

instance AWSRequest AttachVolume where
    type Sv AttachVolume = EC2
    type Rs AttachVolume = AttachVolumeResponse

    request = post "AttachVolume"
    response _ = xmlResponse

data AttachVolumeResponse = AttachVolumeResponse
    { _vvvvvvvvvvvvvvvvvvvvvvvtDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _vvvvvvvvvvvvvvvvvvvvvvvtAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _vvvvvvvvvvvvvvvvvvvvvvvtInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vvvvvvvvvvvvvvvvvvvvvvvtDevice :: Maybe Text
      -- ^ The device name.
    , _vvvvvvvvvvvvvvvvvvvvvvvtVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvtState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    } deriving (Generic)

instance FromXML AttachVolumeResponse where
    fromXMLOptions = xmlOptions
