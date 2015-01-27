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

-- Module      : Network.AWS.EC2.AttachVolume
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

-- | Attaches an Amazon EBS volume to a running or stopped instance and exposes it
-- to the instance with the specified device name.
--
-- Encrypted Amazon EBS volumes may only be attached to instances that support
-- Amazon EBS encryption. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- For a list of supported device names, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html Attaching an Amazon EBS Volume toan Instance>. Any device names that aren't reserved for instance store volumes
-- can be used for Amazon EBS volumes. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/InstanceStorage.html Amazon EC2Instance Store> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- If a volume has an AWS Marketplace product code:
--
-- The volume can be attached only to a stopped instance. AWS Marketplace
-- product codes are copied from the volume to the instance. You must be
-- subscribed to the product. The instance type and operating system of the
-- instance must support the product. For example, you can't detach a volume
-- from a Windows instance and attach it to a Linux instance.  For an overview
-- of the AWS Marketplace, see <https://aws.amazon.com/marketplace/help/200900000 Introducing AWS Marketplace>.
--
-- For more information about Amazon EBS volumes, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-attaching-volume.html Attaching Amazon EBSVolumes> in the /Amazon Elastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachVolume.html>
module Network.AWS.EC2.AttachVolume
    (
    -- * Request
      AttachVolume
    -- ** Request constructor
    , attachVolume
    -- ** Request lenses
    , avDevice
    , avDryRun
    , avInstanceId
    , avVolumeId

    -- * Response
    , AttachVolumeResponse
    -- ** Response constructor
    , attachVolumeResponse
    -- ** Response lenses
    , avrAttachTime
    , avrDeleteOnTermination
    , avrDevice
    , avrInstanceId
    , avrState
    , avrVolumeId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AttachVolume = AttachVolume
    { _avDevice     :: Text
    , _avDryRun     :: Maybe Bool
    , _avInstanceId :: Text
    , _avVolumeId   :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avDevice' @::@ 'Text'
--
-- * 'avDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'avInstanceId' @::@ 'Text'
--
-- * 'avVolumeId' @::@ 'Text'
--
attachVolume :: Text -- ^ 'avVolumeId'
             -> Text -- ^ 'avInstanceId'
             -> Text -- ^ 'avDevice'
             -> AttachVolume
attachVolume p1 p2 p3 = AttachVolume
    { _avVolumeId   = p1
    , _avInstanceId = p2
    , _avDevice     = p3
    , _avDryRun     = Nothing
    }

-- | The device name to expose to the instance (for example, '/dev/sdh' or 'xvdh').
avDevice :: Lens' AttachVolume Text
avDevice = lens _avDevice (\s a -> s { _avDevice = a })

avDryRun :: Lens' AttachVolume (Maybe Bool)
avDryRun = lens _avDryRun (\s a -> s { _avDryRun = a })

-- | The ID of the instance.
avInstanceId :: Lens' AttachVolume Text
avInstanceId = lens _avInstanceId (\s a -> s { _avInstanceId = a })

-- | The ID of the Amazon EBS volume. The volume and instance must be within the
-- same Availability Zone.
avVolumeId :: Lens' AttachVolume Text
avVolumeId = lens _avVolumeId (\s a -> s { _avVolumeId = a })

data AttachVolumeResponse = AttachVolumeResponse
    { _avrAttachTime          :: Maybe ISO8601
    , _avrDeleteOnTermination :: Maybe Bool
    , _avrDevice              :: Maybe Text
    , _avrInstanceId          :: Maybe Text
    , _avrState               :: Maybe VolumeAttachmentState
    , _avrVolumeId            :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'AttachVolumeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'avrAttachTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'avrDeleteOnTermination' @::@ 'Maybe' 'Bool'
--
-- * 'avrDevice' @::@ 'Maybe' 'Text'
--
-- * 'avrInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'avrState' @::@ 'Maybe' 'VolumeAttachmentState'
--
-- * 'avrVolumeId' @::@ 'Maybe' 'Text'
--
attachVolumeResponse :: AttachVolumeResponse
attachVolumeResponse = AttachVolumeResponse
    { _avrVolumeId            = Nothing
    , _avrInstanceId          = Nothing
    , _avrDevice              = Nothing
    , _avrState               = Nothing
    , _avrAttachTime          = Nothing
    , _avrDeleteOnTermination = Nothing
    }

-- | The time stamp when the attachment initiated.
avrAttachTime :: Lens' AttachVolumeResponse (Maybe UTCTime)
avrAttachTime = lens _avrAttachTime (\s a -> s { _avrAttachTime = a }) . mapping _Time

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
avrDeleteOnTermination :: Lens' AttachVolumeResponse (Maybe Bool)
avrDeleteOnTermination =
    lens _avrDeleteOnTermination (\s a -> s { _avrDeleteOnTermination = a })

-- | The device name.
avrDevice :: Lens' AttachVolumeResponse (Maybe Text)
avrDevice = lens _avrDevice (\s a -> s { _avrDevice = a })

-- | The ID of the instance.
avrInstanceId :: Lens' AttachVolumeResponse (Maybe Text)
avrInstanceId = lens _avrInstanceId (\s a -> s { _avrInstanceId = a })

-- | The attachment state of the volume.
avrState :: Lens' AttachVolumeResponse (Maybe VolumeAttachmentState)
avrState = lens _avrState (\s a -> s { _avrState = a })

-- | The ID of the volume.
avrVolumeId :: Lens' AttachVolumeResponse (Maybe Text)
avrVolumeId = lens _avrVolumeId (\s a -> s { _avrVolumeId = a })

instance ToPath AttachVolume where
    toPath = const "/"

instance ToQuery AttachVolume where
    toQuery AttachVolume{..} = mconcat
        [ "Device"     =? _avDevice
        , "DryRun"     =? _avDryRun
        , "InstanceId" =? _avInstanceId
        , "VolumeId"   =? _avVolumeId
        ]

instance ToHeaders AttachVolume

instance AWSRequest AttachVolume where
    type Sv AttachVolume = EC2
    type Rs AttachVolume = AttachVolumeResponse

    request  = post "AttachVolume"
    response = xmlResponse

instance FromXML AttachVolumeResponse where
    parseXML x = AttachVolumeResponse
        <$> x .@? "AttachTime"
        <*> x .@? "DeleteOnTermination"
        <*> x .@? "Device"
        <*> x .@? "InstanceId"
        <*> x .@? "Status"
        <*> x .@? "VolumeId"
