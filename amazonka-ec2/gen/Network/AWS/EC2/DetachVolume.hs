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

-- Module      : Network.AWS.EC2.DetachVolume
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
-- Elastic Compute Cloud User Guide.
module Network.AWS.EC2.DetachVolume
    (
    -- * Request
      DetachVolume
    -- ** Request constructor
    , detachVolume
    -- ** Request lenses
    , dv4Device
    , dv4DryRun
    , dv4Force
    , dv4InstanceId
    , dv4VolumeId

    -- * Response
    , VolumeAttachment
    -- ** Response constructor
    , volumeAttachment
    -- ** Response lenses
    , vaAttachTime
    , vaDeleteOnTermination
    , vaDevice
    , vaInstanceId
    , vaState
    , vaVolumeId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DetachVolume = DetachVolume
    { _dv4Device     :: Maybe Text
    , _dv4DryRun     :: Maybe Bool
    , _dv4Force      :: Maybe Bool
    , _dv4InstanceId :: Maybe Text
    , _dv4VolumeId   :: Text
    } (Eq, Ord, Show, Generic)

-- | 'DetachVolume' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv4Device' @::@ 'Maybe' 'Text'
--
-- * 'dv4DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dv4Force' @::@ 'Maybe' 'Bool'
--
-- * 'dv4InstanceId' @::@ 'Maybe' 'Text'
--
-- * 'dv4VolumeId' @::@ 'Text'
--
detachVolume :: Text -- ^ 'dv4VolumeId'
             -> DetachVolume
detachVolume p1 = DetachVolume
    { _dv4VolumeId   = p1
    , _dv4DryRun     = Nothing
    , _dv4InstanceId = Nothing
    , _dv4Device     = Nothing
    , _dv4Force      = Nothing
    }

-- | The device name.
dv4Device :: Lens' DetachVolume (Maybe Text)
dv4Device = lens _dv4Device (\s a -> s { _dv4Device = a })

dv4DryRun :: Lens' DetachVolume (Maybe Bool)
dv4DryRun = lens _dv4DryRun (\s a -> s { _dv4DryRun = a })

-- | Forces detachment if the previous detachment attempt did not occur
-- cleanly (for example, logging into an instance, unmounting the volume,
-- and detaching normally). This option can lead to data loss or a corrupted
-- file system. Use this option only as a last resort to detach a volume
-- from a failed instance. The instance won't have an opportunity to flush
-- file system caches or file system metadata. If you use this option, you
-- must perform file system check and repair procedures.
dv4Force :: Lens' DetachVolume (Maybe Bool)
dv4Force = lens _dv4Force (\s a -> s { _dv4Force = a })

-- | The ID of the instance.
dv4InstanceId :: Lens' DetachVolume (Maybe Text)
dv4InstanceId = lens _dv4InstanceId (\s a -> s { _dv4InstanceId = a })

-- | The ID of the volume.
dv4VolumeId :: Lens' DetachVolume Text
dv4VolumeId = lens _dv4VolumeId (\s a -> s { _dv4VolumeId = a })
instance ToQuery DetachVolume

instance ToPath DetachVolume where
    toPath = const "/"

instance AWSRequest DetachVolume where
    type Sv DetachVolume = EC2
    type Rs DetachVolume = VolumeAttachment

    request  = post "DetachVolume"
    response = xmlResponse $ const decodeCursor
