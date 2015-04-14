{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateVolume
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an Amazon EBS volume that can be attached to an instance in the same
-- Availability Zone. The volume is created in the regional endpoint that you
-- send the HTTP request to. For more information see <http://docs.aws.amazon.com/general/latest/gr/rande.html Regions and Endpoints>.
--
-- You can create a new empty volume or restore a volume from an Amazon EBS
-- snapshot. Any AWS Marketplace product codes from the snapshot are propagated
-- to the volume.
--
-- You can create encrypted volumes with the 'Encrypted' parameter. Encrypted
-- volumes may only be attached to instances that support Amazon EBS encryption.
-- Volumes that are created from encrypted snapshots are also automatically
-- encrypted. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /AmazonElastic Compute Cloud User Guide for Linux/.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-creating-volume.html Creating or Restoring an Amazon EBS Volume> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVolume.html>
module Network.AWS.EC2.CreateVolume
    (
    -- * Request
      CreateVolume
    -- ** Request constructor
    , createVolume
    -- ** Request lenses
    , cv1AvailabilityZone
    , cv1DryRun
    , cv1Encrypted
    , cv1Iops
    , cv1KmsKeyId
    , cv1Size
    , cv1SnapshotId
    , cv1VolumeType

    -- * Response
    , CreateVolumeResponse
    -- ** Response constructor
    , createVolumeResponse
    -- ** Response lenses
    , cvrAttachments
    , cvrAvailabilityZone
    , cvrCreateTime
    , cvrEncrypted
    , cvrIops
    , cvrKmsKeyId
    , cvrSize
    , cvrSnapshotId
    , cvrState
    , cvrTags
    , cvrVolumeId
    , cvrVolumeType
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateVolume = CreateVolume
    { _cv1AvailabilityZone :: Text
    , _cv1DryRun           :: Maybe Bool
    , _cv1Encrypted        :: Maybe Bool
    , _cv1Iops             :: Maybe Int
    , _cv1KmsKeyId         :: Maybe Text
    , _cv1Size             :: Maybe Int
    , _cv1SnapshotId       :: Maybe Text
    , _cv1VolumeType       :: Maybe VolumeType
    } deriving (Eq, Read, Show)

-- | 'CreateVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cv1AvailabilityZone' @::@ 'Text'
--
-- * 'cv1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cv1Encrypted' @::@ 'Maybe' 'Bool'
--
-- * 'cv1Iops' @::@ 'Maybe' 'Int'
--
-- * 'cv1KmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'cv1Size' @::@ 'Maybe' 'Int'
--
-- * 'cv1SnapshotId' @::@ 'Maybe' 'Text'
--
-- * 'cv1VolumeType' @::@ 'Maybe' 'VolumeType'
--
createVolume :: Text -- ^ 'cv1AvailabilityZone'
             -> CreateVolume
createVolume p1 = CreateVolume
    { _cv1AvailabilityZone = p1
    , _cv1DryRun           = Nothing
    , _cv1Size             = Nothing
    , _cv1SnapshotId       = Nothing
    , _cv1VolumeType       = Nothing
    , _cv1Iops             = Nothing
    , _cv1Encrypted        = Nothing
    , _cv1KmsKeyId         = Nothing
    }

-- | The Availability Zone in which to create the volume. Use 'DescribeAvailabilityZones' to list the Availability Zones that are currently available to you.
cv1AvailabilityZone :: Lens' CreateVolume Text
cv1AvailabilityZone =
    lens _cv1AvailabilityZone (\s a -> s { _cv1AvailabilityZone = a })

cv1DryRun :: Lens' CreateVolume (Maybe Bool)
cv1DryRun = lens _cv1DryRun (\s a -> s { _cv1DryRun = a })

-- | Specifies whether the volume should be encrypted. Encrypted Amazon EBS
-- volumes may only be attached to instances that support Amazon EBS encryption.
-- Volumes that are created from encrypted snapshots are automatically
-- encrypted. There is no way to create an encrypted volume from an unencrypted
-- snapshot or vice versa. If your AMI uses encrypted volumes, you can only
-- launch it on supported instance types. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBSEncryption> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
cv1Encrypted :: Lens' CreateVolume (Maybe Bool)
cv1Encrypted = lens _cv1Encrypted (\s a -> s { _cv1Encrypted = a })

-- | Only valid for Provisioned IOPS (SSD) volumes. The number of I/O operations
-- per second (IOPS) to provision for the volume, with a maximum ratio of 30
-- IOPS/GiB.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes
cv1Iops :: Lens' CreateVolume (Maybe Int)
cv1Iops = lens _cv1Iops (\s a -> s { _cv1Iops = a })

-- | The full ARN of the AWS Key Management Service (KMS) master key to use when
-- creating the encrypted volume. This parameter is only required if you want to
-- use a non-default master key; if this parameter is not specified, the default
-- master key is used. The ARN contains the 'arn:aws:kms' namespace, followed by
-- the region of the master key, the AWS account ID of the master key owner, the 'key' namespace, and then the master key ID. For example, arn:aws:kms:/us-east-1/:/012345678910/:key//abcd1234-a123-456a-a12b-a123b4cd56ef/.
cv1KmsKeyId :: Lens' CreateVolume (Maybe Text)
cv1KmsKeyId = lens _cv1KmsKeyId (\s a -> s { _cv1KmsKeyId = a })

-- | The size of the volume, in GiBs.
--
-- Constraints: '1-1024' for 'standard' volumes, '1-16384' for 'gp2' volumes, and '4-16384' for 'io1' volumes. If you specify a snapshot, the volume size must be equal to
-- or larger than the snapshot size.
--
-- Default: If you're creating the volume from a snapshot and don't specify a
-- volume size, the default is the snapshot size.
cv1Size :: Lens' CreateVolume (Maybe Int)
cv1Size = lens _cv1Size (\s a -> s { _cv1Size = a })

-- | The snapshot from which to create the volume.
cv1SnapshotId :: Lens' CreateVolume (Maybe Text)
cv1SnapshotId = lens _cv1SnapshotId (\s a -> s { _cv1SnapshotId = a })

-- | The volume type. This can be 'gp2' for General Purpose (SSD) volumes, 'io1' for
-- Provisioned IOPS (SSD) volumes, or 'standard' for Magnetic volumes.
--
-- Default: 'standard'
cv1VolumeType :: Lens' CreateVolume (Maybe VolumeType)
cv1VolumeType = lens _cv1VolumeType (\s a -> s { _cv1VolumeType = a })

data CreateVolumeResponse = CreateVolumeResponse
    { _cvrAttachments      :: List "item" VolumeAttachment
    , _cvrAvailabilityZone :: Text
    , _cvrCreateTime       :: ISO8601
    , _cvrEncrypted        :: Bool
    , _cvrIops             :: Maybe Int
    , _cvrKmsKeyId         :: Maybe Text
    , _cvrSize             :: Int
    , _cvrSnapshotId       :: Text
    , _cvrState            :: VolumeState
    , _cvrTags             :: List "item" Tag
    , _cvrVolumeId         :: Text
    , _cvrVolumeType       :: VolumeType
    } deriving (Eq, Read, Show)

-- | 'CreateVolumeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvrAttachments' @::@ ['VolumeAttachment']
--
-- * 'cvrAvailabilityZone' @::@ 'Text'
--
-- * 'cvrCreateTime' @::@ 'UTCTime'
--
-- * 'cvrEncrypted' @::@ 'Bool'
--
-- * 'cvrIops' @::@ 'Maybe' 'Int'
--
-- * 'cvrKmsKeyId' @::@ 'Maybe' 'Text'
--
-- * 'cvrSize' @::@ 'Int'
--
-- * 'cvrSnapshotId' @::@ 'Text'
--
-- * 'cvrState' @::@ 'VolumeState'
--
-- * 'cvrTags' @::@ ['Tag']
--
-- * 'cvrVolumeId' @::@ 'Text'
--
-- * 'cvrVolumeType' @::@ 'VolumeType'
--
createVolumeResponse :: Text -- ^ 'cvrVolumeId'
                     -> Int -- ^ 'cvrSize'
                     -> Text -- ^ 'cvrSnapshotId'
                     -> Text -- ^ 'cvrAvailabilityZone'
                     -> VolumeState -- ^ 'cvrState'
                     -> UTCTime -- ^ 'cvrCreateTime'
                     -> VolumeType -- ^ 'cvrVolumeType'
                     -> Bool -- ^ 'cvrEncrypted'
                     -> CreateVolumeResponse
createVolumeResponse p1 p2 p3 p4 p5 p6 p7 p8 = CreateVolumeResponse
    { _cvrVolumeId         = p1
    , _cvrSize             = p2
    , _cvrSnapshotId       = p3
    , _cvrAvailabilityZone = p4
    , _cvrState            = p5
    , _cvrCreateTime       = withIso _Time (const id) p6
    , _cvrVolumeType       = p7
    , _cvrEncrypted        = p8
    , _cvrAttachments      = mempty
    , _cvrTags             = mempty
    , _cvrIops             = Nothing
    , _cvrKmsKeyId         = Nothing
    }

cvrAttachments :: Lens' CreateVolumeResponse [VolumeAttachment]
cvrAttachments = lens _cvrAttachments (\s a -> s { _cvrAttachments = a }) . _List

-- | The Availability Zone for the volume.
cvrAvailabilityZone :: Lens' CreateVolumeResponse Text
cvrAvailabilityZone =
    lens _cvrAvailabilityZone (\s a -> s { _cvrAvailabilityZone = a })

-- | The time stamp when volume creation was initiated.
cvrCreateTime :: Lens' CreateVolumeResponse UTCTime
cvrCreateTime = lens _cvrCreateTime (\s a -> s { _cvrCreateTime = a }) . _Time

-- | Indicates whether the volume will be encrypted.
cvrEncrypted :: Lens' CreateVolumeResponse Bool
cvrEncrypted = lens _cvrEncrypted (\s a -> s { _cvrEncrypted = a })

-- | The number of I/O operations per second (IOPS) that the volume supports. For
-- Provisioned IOPS (SSD) volumes, this represents the number of IOPS that are
-- provisioned for the volume. For General Purpose (SSD) volumes, this
-- represents the baseline performance of the volume and the rate at which the
-- volume accumulates I/O credits for bursting. For more information on General
-- Purpose (SSD) baseline performance, I/O credits, and bursting, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html Amazon EBSVolume Types> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- Constraint: Range is 100 to 20000 for Provisioned IOPS (SSD) volumes and 3
-- to 10000 for General Purpose (SSD) volumes.
--
-- Condition: This parameter is required for requests to create 'io1' volumes; it
-- is not used in requests to create 'standard' or 'gp2' volumes.
cvrIops :: Lens' CreateVolumeResponse (Maybe Int)
cvrIops = lens _cvrIops (\s a -> s { _cvrIops = a })

-- | The full ARN of the AWS Key Management Service (KMS) master key that was used
-- to protect the volume encryption key for the volume.
cvrKmsKeyId :: Lens' CreateVolumeResponse (Maybe Text)
cvrKmsKeyId = lens _cvrKmsKeyId (\s a -> s { _cvrKmsKeyId = a })

-- | The size of the volume, in GiBs.
cvrSize :: Lens' CreateVolumeResponse Int
cvrSize = lens _cvrSize (\s a -> s { _cvrSize = a })

-- | The snapshot from which the volume was created, if applicable.
cvrSnapshotId :: Lens' CreateVolumeResponse Text
cvrSnapshotId = lens _cvrSnapshotId (\s a -> s { _cvrSnapshotId = a })

-- | The volume state.
cvrState :: Lens' CreateVolumeResponse VolumeState
cvrState = lens _cvrState (\s a -> s { _cvrState = a })

-- | Any tags assigned to the volume.
cvrTags :: Lens' CreateVolumeResponse [Tag]
cvrTags = lens _cvrTags (\s a -> s { _cvrTags = a }) . _List

-- | The ID of the volume.
cvrVolumeId :: Lens' CreateVolumeResponse Text
cvrVolumeId = lens _cvrVolumeId (\s a -> s { _cvrVolumeId = a })

-- | The volume type. This can be 'gp2' for General Purpose (SSD) volumes, 'io1' for
-- Provisioned IOPS (SSD) volumes, or 'standard' for Magnetic volumes.
cvrVolumeType :: Lens' CreateVolumeResponse VolumeType
cvrVolumeType = lens _cvrVolumeType (\s a -> s { _cvrVolumeType = a })

instance ToPath CreateVolume where
    toPath = const "/"

instance ToQuery CreateVolume where
    toQuery CreateVolume{..} = mconcat
        [ "AvailabilityZone" =? _cv1AvailabilityZone
        , "DryRun"           =? _cv1DryRun
        , "Encrypted"        =? _cv1Encrypted
        , "Iops"             =? _cv1Iops
        , "KmsKeyId"         =? _cv1KmsKeyId
        , "Size"             =? _cv1Size
        , "SnapshotId"       =? _cv1SnapshotId
        , "VolumeType"       =? _cv1VolumeType
        ]

instance ToHeaders CreateVolume

instance AWSRequest CreateVolume where
    type Sv CreateVolume = EC2
    type Rs CreateVolume = CreateVolumeResponse

    request  = post "CreateVolume"
    response = xmlResponse

instance FromXML CreateVolumeResponse where
    parseXML x = CreateVolumeResponse
        <$> x .@? "attachmentSet" .!@ mempty
        <*> x .@  "availabilityZone"
        <*> x .@  "createTime"
        <*> x .@  "encrypted"
        <*> x .@? "iops"
        <*> x .@? "kmsKeyId"
        <*> x .@  "size"
        <*> x .@  "snapshotId"
        <*> x .@  "status"
        <*> x .@? "tagSet" .!@ mempty
        <*> x .@  "volumeId"
        <*> x .@  "volumeType"
