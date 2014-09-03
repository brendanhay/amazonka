{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
module Network.AWS.EC2.V2014_06_15.DetachVolume
    (
    -- * Request
      DetachVolume
    -- ** Request constructor
    , detachVolume
    -- ** Request lenses
    , dvxVolumeId
    , dvxForce
    , dvxInstanceId
    , dvxDevice

    -- * Response
    , DetachVolumeResponse
    -- ** Response lenses
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDevice
    , vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuState
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DetachVolume' request.
detachVolume :: Text -- ^ 'dvxVolumeId'
             -> DetachVolume
detachVolume p1 = DetachVolume
    { _dvxVolumeId = p1
    , _dvxForce = Nothing
    , _dvxInstanceId = Nothing
    , _dvxDevice = Nothing
    }

data DetachVolume = DetachVolume
    { _dvxVolumeId :: Text
      -- ^ The ID of the volume.
    , _dvxForce :: Maybe Bool
      -- ^ Forces detachment if the previous detachment attempt did not
      -- occur cleanly (for example, logging into an instance, unmounting
      -- the volume, and detaching normally). This option can lead to data
      -- loss or a corrupted file system. Use this option only as a last
      -- resort to detach a volume from a failed instance. The instance
      -- won't have an opportunity to flush file system caches or file
      -- system metadata. If you use this option, you must perform file
      -- system check and repair procedures.
    , _dvxInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _dvxDevice :: Maybe Text
      -- ^ The device name.
    } deriving (Show, Generic)

-- | The ID of the volume.
dvxVolumeId
    :: Functor f
    => (Text
    -> f (Text))
    -> DetachVolume
    -> f DetachVolume
dvxVolumeId f x =
    (\y -> x { _dvxVolumeId = y })
       <$> f (_dvxVolumeId x)
{-# INLINE dvxVolumeId #-}

-- | Forces detachment if the previous detachment attempt did not occur cleanly
-- (for example, logging into an instance, unmounting the volume, and
-- detaching normally). This option can lead to data loss or a corrupted file
-- system. Use this option only as a last resort to detach a volume from a
-- failed instance. The instance won't have an opportunity to flush file
-- system caches or file system metadata. If you use this option, you must
-- perform file system check and repair procedures.
dvxForce
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DetachVolume
    -> f DetachVolume
dvxForce f x =
    (\y -> x { _dvxForce = y })
       <$> f (_dvxForce x)
{-# INLINE dvxForce #-}

-- | The ID of the instance.
dvxInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DetachVolume
    -> f DetachVolume
dvxInstanceId f x =
    (\y -> x { _dvxInstanceId = y })
       <$> f (_dvxInstanceId x)
{-# INLINE dvxInstanceId #-}

-- | The device name.
dvxDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DetachVolume
    -> f DetachVolume
dvxDevice f x =
    (\y -> x { _dvxDevice = y })
       <$> f (_dvxDevice x)
{-# INLINE dvxDevice #-}

instance ToQuery DetachVolume where
    toQuery = genericQuery def

data DetachVolumeResponse = DetachVolumeResponse
    { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination :: Maybe Bool
      -- ^ Indicates whether the Amazon EBS volume is deleted on instance
      -- termination.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime :: Maybe ISO8601
      -- ^ The time stamp when the attachment initiated.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId :: Maybe Text
      -- ^ The ID of the volume.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId :: Maybe Text
      -- ^ The ID of the instance.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDevice :: Maybe Text
      -- ^ The device name.
    , _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuState :: Maybe VolumeAttachmentState
      -- ^ The attachment state of the volume.
    } deriving (Show, Generic)

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DetachVolumeResponse
    -> f DetachVolumeResponse
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDeleteOnTermination #-}

-- | The time stamp when the attachment initiated.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DetachVolumeResponse
    -> f DetachVolumeResponse
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuAttachTime #-}

-- | The ID of the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DetachVolumeResponse
    -> f DetachVolumeResponse
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuVolumeId #-}

-- | The ID of the instance.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DetachVolumeResponse
    -> f DetachVolumeResponse
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuInstanceId #-}

-- | The device name.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDevice
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DetachVolumeResponse
    -> f DetachVolumeResponse
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDevice f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDevice = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDevice x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuDevice #-}

-- | The attachment state of the volume.
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuState
    :: Functor f
    => (Maybe VolumeAttachmentState
    -> f (Maybe VolumeAttachmentState))
    -> DetachVolumeResponse
    -> f DetachVolumeResponse
vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuState f x =
    (\y -> x { _vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuState = y })
       <$> f (_vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuState x)
{-# INLINE vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvuState #-}

instance FromXML DetachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachVolume where
    type Sv DetachVolume = EC2
    type Rs DetachVolume = DetachVolumeResponse

    request = post "DetachVolume"
    response _ = xmlResponse
