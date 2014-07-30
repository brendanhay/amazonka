{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Amazon EBS volume that can be attached to an instance in the
-- same Availability Zone. The volume is created in the specified region. You
-- can create a new empty volume or restore a volume from an Amazon EBS
-- snapshot. Any AWS Marketplace product codes from the snapshot are
-- propagated to the volume. You can create encrypted volumes with the
-- Encrypted parameter. Encrypted volumes may only be attached to instances
-- that support Amazon EBS encryption. Volumes that are created from encrypted
-- snapshots are also automatically encrypted. For more information, see
-- Amazon EBS Encryption in the Amazon Elastic Compute Cloud User Guide. For
-- more information, see Creating or Restoring an Amazon EBS Volume in the
-- Amazon Elastic Compute Cloud User Guide. Example This example request
-- creates an 80 GiB encrypted volume in the Availability Zone us-east-1a.
-- https://ec2.amazonaws.com/?Action=CreateVolume &amp;Size=80
-- &amp;AvailabilityZone=us-east-1a &amp;Encrypted=1 &amp;AUTHPARAMS
-- &lt;CreateVolumeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt; &lt;size&gt;80&lt;/size&gt;
-- &lt;snapshotId/&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;status&gt;creating&lt;/status&gt;
-- &lt;createTime&gt;YYYY-MM-DDTHH:MM:SS.000Z&lt;/createTime&gt;
-- &lt;volumeType&gt;standard&lt;/volumeType&gt;
-- &lt;encrypted&gt;true&lt;/encrypted&gt; &lt;/CreateVolumeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateVolume where

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
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'CreateVolume' request.
createVolume :: Text -- ^ '_cvtAvailabilityZone'
             -> CreateVolume
createVolume p1 = CreateVolume
    { _cvtAvailabilityZone = p1
    , _cvtEncrypted = Nothing
    , _cvtDryRun = Nothing
    , _cvtSize = Nothing
    , _cvtIops = Nothing
    , _cvtSnapshotId = Nothing
    , _cvtVolumeType = Nothing
    }

data CreateVolume = CreateVolume
    { _cvtAvailabilityZone :: Text
      -- ^ The Availability Zone in which to create the volume. Use
      -- DescribeAvailabilityZones to list the Availability Zones that are
      -- currently available to you.
    , _cvtEncrypted :: Maybe Bool
      -- ^ Specifies whether the volume should be encrypted.
    , _cvtDryRun :: Maybe Bool
      -- ^ 
    , _cvtSize :: Maybe Integer
      -- ^ The size of the volume, in GiBs. Constraints: If the volume type
      -- is io1, the minimum size of the volume is 10 GiB. Default: If
      -- you're creating the volume from a snapshot and don't specify a
      -- volume size, the default is the snapshot size.
    , _cvtIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. This parameter is not used with Magnetic or General
      -- Purpose (SSD) volumes, but is required when the volume type is
      -- io1.
    , _cvtSnapshotId :: Maybe Text
      -- ^ The snapshot from which to create the volume.
    , _cvtVolumeType :: Maybe VolumeType
      -- ^ The volume type. This can be gp2 for General Purpose (SSD)
      -- volumes, io1 for Provisioned IOPS (SSD) volumes, or standard for
      -- Magnetic volumes. Default: standard.
    } deriving (Generic)

instance ToQuery CreateVolume where
    toQuery = genericToQuery def

instance AWSRequest CreateVolume where
    type Sv CreateVolume = EC2
    type Rs CreateVolume = CreateVolumeResponse

    request = post "CreateVolume"
    response _ = xmlResponse

data CreateVolumeResponse = CreateVolumeResponse
    { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhEncrypted :: Maybe Bool
      -- ^ Indicates whether the volume is encrypted.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhCreateTime :: Maybe ISO8601
      -- ^ The time stamp when volume creation was initiated.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhSize :: Maybe Integer
      -- ^ The size of the volume, in GiBs.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhTags :: [Tag]
      -- ^ Any tags assigned to the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhSnapshotId :: Maybe Text
      -- ^ The snapshot from which the volume was created, if applicable.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhAttachments :: [VolumeAttachment]
      -- ^ 
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhState :: Maybe VolumeState
      -- ^ The volume state.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvhVolumeType :: Maybe VolumeType
      -- ^ The volume type. This can be gp2 for General Purpose (SSD)
      -- volumes, io1 for Provisioned IOPS (SSD) volumes, or standard for
      -- Magnetic volumes.
    } deriving (Generic)

instance FromXML CreateVolumeResponse where
    fromXMLOptions = xmlOptions
