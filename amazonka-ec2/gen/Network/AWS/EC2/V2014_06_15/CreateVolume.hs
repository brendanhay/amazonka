{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.EC2.V2014_06_15.CreateVolume
    (
    -- * Request
      CreateVolume
    -- ** Request constructor
    , mkCreateVolume
    -- ** Request lenses
    , cvSize
    , cvSnapshotId
    , cvAvailabilityZone
    , cvVolumeType
    , cvIops
    , cvEncrypted

    -- * Response
    , CreateVolumeResponse
    -- ** Response lenses
    , cvrVolumeId
    , cvrSize
    , cvrSnapshotId
    , cvrAvailabilityZone
    , cvrState
    , cvrCreateTime
    , cvrAttachments
    , cvrTags
    , cvrVolumeType
    , cvrIops
    , cvrEncrypted
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data CreateVolume = CreateVolume
    { _cvSize :: Maybe Integer
    , _cvSnapshotId :: Maybe Text
    , _cvAvailabilityZone :: Text
    , _cvVolumeType :: Maybe VolumeType
    , _cvIops :: Maybe Integer
    , _cvEncrypted :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVolume' request.
mkCreateVolume :: Text -- ^ 'cvAvailabilityZone'
               -> CreateVolume
mkCreateVolume p3 = CreateVolume
    { _cvSize = Nothing
    , _cvSnapshotId = Nothing
    , _cvAvailabilityZone = p3
    , _cvVolumeType = Nothing
    , _cvIops = Nothing
    , _cvEncrypted = Nothing
    }

-- | The size of the volume, in GiBs. Constraints: If the volume type is io1,
-- the minimum size of the volume is 10 GiB. Default: If you're creating the
-- volume from a snapshot and don't specify a volume size, the default is the
-- snapshot size.
cvSize :: Lens' CreateVolume (Maybe Integer)
cvSize = lens _cvSize (\s a -> s { _cvSize = a })

-- | The snapshot from which to create the volume.
cvSnapshotId :: Lens' CreateVolume (Maybe Text)
cvSnapshotId = lens _cvSnapshotId (\s a -> s { _cvSnapshotId = a })

-- | The Availability Zone in which to create the volume. Use
-- DescribeAvailabilityZones to list the Availability Zones that are currently
-- available to you.
cvAvailabilityZone :: Lens' CreateVolume Text
cvAvailabilityZone =
    lens _cvAvailabilityZone (\s a -> s { _cvAvailabilityZone = a })

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes. Default:
-- standard.
cvVolumeType :: Lens' CreateVolume (Maybe VolumeType)
cvVolumeType = lens _cvVolumeType (\s a -> s { _cvVolumeType = a })

-- | Only valid for Provisioned IOPS (SSD) volumes. The number of I/O operations
-- per second (IOPS) to provision for the volume.
cvIops :: Lens' CreateVolume (Maybe Integer)
cvIops = lens _cvIops (\s a -> s { _cvIops = a })

-- | Specifies whether the volume should be encrypted.
cvEncrypted :: Lens' CreateVolume (Maybe Bool)
cvEncrypted = lens _cvEncrypted (\s a -> s { _cvEncrypted = a })

instance ToQuery CreateVolume where
    toQuery = genericQuery def

-- | 
data CreateVolumeResponse = CreateVolumeResponse
    { _cvrVolumeId :: Maybe Text
    , _cvrSize :: Maybe Integer
    , _cvrSnapshotId :: Maybe Text
    , _cvrAvailabilityZone :: Maybe Text
    , _cvrState :: Maybe VolumeState
    , _cvrCreateTime :: Maybe ISO8601
    , _cvrAttachments :: [VolumeAttachment]
    , _cvrTags :: [Tag]
    , _cvrVolumeType :: Maybe VolumeType
    , _cvrIops :: Maybe Integer
    , _cvrEncrypted :: Maybe Bool
    } deriving (Show, Generic)

-- | The ID of the volume.
cvrVolumeId :: Lens' CreateVolumeResponse (Maybe Text)
cvrVolumeId = lens _cvrVolumeId (\s a -> s { _cvrVolumeId = a })

-- | The size of the volume, in GiBs.
cvrSize :: Lens' CreateVolumeResponse (Maybe Integer)
cvrSize = lens _cvrSize (\s a -> s { _cvrSize = a })

-- | The snapshot from which the volume was created, if applicable.
cvrSnapshotId :: Lens' CreateVolumeResponse (Maybe Text)
cvrSnapshotId = lens _cvrSnapshotId (\s a -> s { _cvrSnapshotId = a })

-- | The Availability Zone for the volume.
cvrAvailabilityZone :: Lens' CreateVolumeResponse (Maybe Text)
cvrAvailabilityZone =
    lens _cvrAvailabilityZone (\s a -> s { _cvrAvailabilityZone = a })

-- | The volume state.
cvrState :: Lens' CreateVolumeResponse (Maybe VolumeState)
cvrState = lens _cvrState (\s a -> s { _cvrState = a })

-- | The time stamp when volume creation was initiated.
cvrCreateTime :: Lens' CreateVolumeResponse (Maybe ISO8601)
cvrCreateTime = lens _cvrCreateTime (\s a -> s { _cvrCreateTime = a })

-- | 
cvrAttachments :: Lens' CreateVolumeResponse [VolumeAttachment]
cvrAttachments = lens _cvrAttachments (\s a -> s { _cvrAttachments = a })

-- | Any tags assigned to the volume.
cvrTags :: Lens' CreateVolumeResponse [Tag]
cvrTags = lens _cvrTags (\s a -> s { _cvrTags = a })

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
cvrVolumeType :: Lens' CreateVolumeResponse (Maybe VolumeType)
cvrVolumeType = lens _cvrVolumeType (\s a -> s { _cvrVolumeType = a })

-- | The number of I/O operations per second (IOPS) that the volume supports.
-- For Provisioned IOPS (SSD) volumes, this represents the number of IOPS that
-- are provisioned for the volume. For General Purpose (SSD) volumes, this
-- represents the baseline performance of the volume and the rate at which the
-- volume accumulates I/O credits for bursting. For more information on
-- General Purpose (SSD) baseline performance, I/O credits, and bursting, see
-- Amazon EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
-- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD) volumes and 3
-- to 3072 for General Purpose (SSD) volumes. Condition: This parameter is
-- required for requests to create io1 volumes; it is not used in requests to
-- create standard or gp2 volumes.
cvrIops :: Lens' CreateVolumeResponse (Maybe Integer)
cvrIops = lens _cvrIops (\s a -> s { _cvrIops = a })

-- | Indicates whether the volume is encrypted.
cvrEncrypted :: Lens' CreateVolumeResponse (Maybe Bool)
cvrEncrypted = lens _cvrEncrypted (\s a -> s { _cvrEncrypted = a })

instance FromXML CreateVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateVolume where
    type Sv CreateVolume = EC2
    type Rs CreateVolume = CreateVolumeResponse

    request = post "CreateVolume"
    response _ = xmlResponse
