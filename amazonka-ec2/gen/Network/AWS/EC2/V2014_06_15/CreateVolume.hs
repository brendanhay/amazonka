{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
    -- ** Default constructor
    , createVolume
    -- ** Accessors and lenses
    , _cvrAvailabilityZone
    , cvrAvailabilityZone
    , _cvrEncrypted
    , cvrEncrypted
    , _cvrSize
    , cvrSize
    , _cvrIops
    , cvrIops
    , _cvrSnapshotId
    , cvrSnapshotId
    , _cvrVolumeType
    , cvrVolumeType

    -- * Response
    , CreateVolumeResponse
    -- ** Accessors and lenses
    , _vvvvvvvvvvvvxEncrypted
    , vvvvvvvvvvvvxEncrypted
    , _vvvvvvvvvvvvxCreateTime
    , vvvvvvvvvvvvxCreateTime
    , _vvvvvvvvvvvvxSize
    , vvvvvvvvvvvvxSize
    , _vvvvvvvvvvvvxIops
    , vvvvvvvvvvvvxIops
    , _vvvvvvvvvvvvxTags
    , vvvvvvvvvvvvxTags
    , _vvvvvvvvvvvvxVolumeId
    , vvvvvvvvvvvvxVolumeId
    , _vvvvvvvvvvvvxSnapshotId
    , vvvvvvvvvvvvxSnapshotId
    , _vvvvvvvvvvvvxAvailabilityZone
    , vvvvvvvvvvvvxAvailabilityZone
    , _vvvvvvvvvvvvxAttachments
    , vvvvvvvvvvvvxAttachments
    , _vvvvvvvvvvvvxState
    , vvvvvvvvvvvvxState
    , _vvvvvvvvvvvvxVolumeType
    , vvvvvvvvvvvvxVolumeType
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateVolume' request.
createVolume :: Text -- ^ 'cvrAvailabilityZone'
             -> CreateVolume
createVolume p1 = CreateVolume
    { _cvrAvailabilityZone = p1
    , _cvrEncrypted = Nothing
    , _cvrSize = Nothing
    , _cvrIops = Nothing
    , _cvrSnapshotId = Nothing
    , _cvrVolumeType = Nothing
    }

data CreateVolume = CreateVolume

makeSiglessLenses ''CreateVolume

instance ToQuery CreateVolume where
    toQuery = genericQuery def

data CreateVolumeResponse = CreateVolumeResponse
    { _vvvvvvvvvvvvxEncrypted :: Maybe Bool
      -- ^ Indicates whether the volume is encrypted.
    , _vvvvvvvvvvvvxCreateTime :: Maybe ISO8601
      -- ^ The time stamp when volume creation was initiated.
    , _vvvvvvvvvvvvxSize :: Maybe Integer
      -- ^ The size of the volume, in GiBs.
    , _vvvvvvvvvvvvxIops :: Maybe Integer
      -- ^ The number of I/O operations per second (IOPS) that the volume
      -- supports. For Provisioned IOPS (SSD) volumes, this represents the
      -- number of IOPS that are provisioned for the volume. For General
      -- Purpose (SSD) volumes, this represents the baseline performance
      -- of the volume and the rate at which the volume accumulates I/O
      -- credits for bursting. For more information on General Purpose
      -- (SSD) baseline performance, I/O credits, and bursting, see Amazon
      -- EBS Volume Types in the Amazon Elastic Compute Cloud User Guide.
      -- Constraint: Range is 100 to 4000 for Provisioned IOPS (SSD)
      -- volumes and 3 to 3072 for General Purpose (SSD) volumes.
      -- Condition: This parameter is required for requests to create io1
      -- volumes; it is not used in requests to create standard or gp2
      -- volumes.
    , _vvvvvvvvvvvvxTags :: [Tag]
      -- ^ Any tags assigned to the volume.
    , _vvvvvvvvvvvvxVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vvvvvvvvvvvvxSnapshotId :: Maybe Text
      -- ^ The snapshot from which the volume was created, if applicable.
    , _vvvvvvvvvvvvxAvailabilityZone :: Maybe Text
      -- ^ The Availability Zone for the volume.
    , _vvvvvvvvvvvvxAttachments :: [VolumeAttachment]
      -- ^ 
    , _vvvvvvvvvvvvxState :: Maybe VolumeState
      -- ^ The volume state.
    , _vvvvvvvvvvvvxVolumeType :: Maybe VolumeType
      -- ^ The volume type. This can be gp2 for General Purpose (SSD)
      -- volumes, io1 for Provisioned IOPS (SSD) volumes, or standard for
      -- Magnetic volumes.
    } deriving (Show, Generic)

makeSiglessLenses ''CreateVolumeResponse

instance FromXML CreateVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateVolume where
    type Sv CreateVolume = EC2
    type Rs CreateVolume = CreateVolumeResponse

    request = post "CreateVolume"
    response _ = xmlResponse

-- | The Availability Zone in which to create the volume. Use
-- DescribeAvailabilityZones to list the Availability Zones that are currently
-- available to you.
cvrAvailabilityZone :: Lens' CreateVolume (Text)

-- | Specifies whether the volume should be encrypted.
cvrEncrypted :: Lens' CreateVolume (Maybe Bool)

-- | The size of the volume, in GiBs. Constraints: If the volume type is io1,
-- the minimum size of the volume is 10 GiB. Default: If you're creating the
-- volume from a snapshot and don't specify a volume size, the default is the
-- snapshot size.
cvrSize :: Lens' CreateVolume (Maybe Integer)

-- | Only valid for Provisioned IOPS (SSD) volumes. The number of I/O operations
-- per second (IOPS) to provision for the volume.
cvrIops :: Lens' CreateVolume (Maybe Integer)

-- | The snapshot from which to create the volume.
cvrSnapshotId :: Lens' CreateVolume (Maybe Text)

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes. Default:
-- standard.
cvrVolumeType :: Lens' CreateVolume (Maybe VolumeType)

-- | Indicates whether the volume is encrypted.
vvvvvvvvvvvvxEncrypted :: Lens' CreateVolumeResponse (Maybe Bool)

-- | The time stamp when volume creation was initiated.
vvvvvvvvvvvvxCreateTime :: Lens' CreateVolumeResponse (Maybe ISO8601)

-- | The size of the volume, in GiBs.
vvvvvvvvvvvvxSize :: Lens' CreateVolumeResponse (Maybe Integer)

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
vvvvvvvvvvvvxIops :: Lens' CreateVolumeResponse (Maybe Integer)

-- | Any tags assigned to the volume.
vvvvvvvvvvvvxTags :: Lens' CreateVolumeResponse ([Tag])

-- | The ID of the volume.
vvvvvvvvvvvvxVolumeId :: Lens' CreateVolumeResponse (Maybe Text)

-- | The snapshot from which the volume was created, if applicable.
vvvvvvvvvvvvxSnapshotId :: Lens' CreateVolumeResponse (Maybe Text)

-- | The Availability Zone for the volume.
vvvvvvvvvvvvxAvailabilityZone :: Lens' CreateVolumeResponse (Maybe Text)

-- | 
vvvvvvvvvvvvxAttachments :: Lens' CreateVolumeResponse ([VolumeAttachment])

-- | The volume state.
vvvvvvvvvvvvxState :: Lens' CreateVolumeResponse (Maybe VolumeState)

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1 for
-- Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
vvvvvvvvvvvvxVolumeType :: Lens' CreateVolumeResponse (Maybe VolumeType)
