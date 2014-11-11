{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.CreateVolume
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
-- Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.CreateVolume
    (
    -- * Request
      CreateVolume
    -- ** Request constructor
    , createVolume
    -- ** Request lenses
    , cvAvailabilityZone
    , cvDryRun
    , cvEncrypted
    , cvIops
    , cvSize
    , cvSnapshotId
    , cvVolumeType

    -- * Response
    , Volume
    -- ** Response constructor
    , volume
    -- ** Response lenses
    , vAttachments
    , vAvailabilityZone
    , vCreateTime
    , vEncrypted
    , vIops
    , vSize
    , vSnapshotId
    , vState
    , vTags
    , vVolumeId
    , vVolumeType
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateVolume = CreateVolume
    { _cvAvailabilityZone :: Text
    , _cvDryRun           :: Maybe Bool
    , _cvEncrypted        :: Maybe Bool
    , _cvIops             :: Maybe Int
    , _cvSize             :: Maybe Int
    , _cvSnapshotId       :: Maybe Text
    , _cvVolumeType       :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvAvailabilityZone' @::@ 'Text'
--
-- * 'cvDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cvEncrypted' @::@ 'Maybe' 'Bool'
--
-- * 'cvIops' @::@ 'Maybe' 'Int'
--
-- * 'cvSize' @::@ 'Maybe' 'Int'
--
-- * 'cvSnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'cvVolumeType' @::@ 'Maybe' 'Text'
--
createVolume :: Text -- ^ 'cvAvailabilityZone'
             -> CreateVolume
createVolume p1 = CreateVolume
    { _cvAvailabilityZone = p1
    , _cvDryRun           = Nothing
    , _cvSize             = Nothing
    , _cvSnapshotId       = Nothing
    , _cvVolumeType       = Nothing
    , _cvIops             = Nothing
    , _cvEncrypted        = Nothing
    }

-- | The Availability Zone in which to create the volume. Use
-- DescribeAvailabilityZones to list the Availability Zones that are
-- currently available to you.
cvAvailabilityZone :: Lens' CreateVolume Text
cvAvailabilityZone =
    lens _cvAvailabilityZone (\s a -> s { _cvAvailabilityZone = a })

cvDryRun :: Lens' CreateVolume (Maybe Bool)
cvDryRun = lens _cvDryRun (\s a -> s { _cvDryRun = a })

-- | Specifies whether the volume should be encrypted.
cvEncrypted :: Lens' CreateVolume (Maybe Bool)
cvEncrypted = lens _cvEncrypted (\s a -> s { _cvEncrypted = a })

-- | Only valid for Provisioned IOPS (SSD) volumes. The number of I/O
-- operations per second (IOPS) to provision for the volume.
cvIops :: Lens' CreateVolume (Maybe Int)
cvIops = lens _cvIops (\s a -> s { _cvIops = a })

-- | The size of the volume, in GiBs. Constraints: If the volume type is io1,
-- the minimum size of the volume is 10 GiB. Default: If you're creating the
-- volume from a snapshot and don't specify a volume size, the default is
-- the snapshot size.
cvSize :: Lens' CreateVolume (Maybe Int)
cvSize = lens _cvSize (\s a -> s { _cvSize = a })

-- | The snapshot from which to create the volume.
cvSnapshotId :: Lens' CreateVolume (Maybe Text)
cvSnapshotId = lens _cvSnapshotId (\s a -> s { _cvSnapshotId = a })

-- | The volume type. This can be gp2 for General Purpose (SSD) volumes, io1
-- for Provisioned IOPS (SSD) volumes, or standard for Magnetic volumes.
-- Default: standard.
cvVolumeType :: Lens' CreateVolume (Maybe Text)
cvVolumeType = lens _cvVolumeType (\s a -> s { _cvVolumeType = a })
instance ToQuery CreateVolume

instance ToPath CreateVolume where
    toPath = const "/"

instance AWSRequest CreateVolume where
    type Sv CreateVolume = EC2
    type Rs CreateVolume = Volume

    request  = post "CreateVolume"
    response = xmlResponse $ const decodeCursor
