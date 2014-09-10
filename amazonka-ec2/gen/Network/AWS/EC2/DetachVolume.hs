{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
module Network.AWS.EC2
    (
    -- * Request
      DetachVolume
    -- ** Request constructor
    , mkDetachVolume
    -- ** Request lenses
    , dv4VolumeId
    , dv4InstanceId
    , dv4Device
    , dv4Force

    -- * Response
    , DetachVolumeResponse
    -- ** Response constructor
    , mkDetachVolumeResponse
    -- ** Response lenses
    , dvr1VolumeId
    , dvr1InstanceId
    , dvr1Device
    , dvr1State
    , dvr1AttachTime
    , dvr1DeleteOnTermination
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DetachVolume = DetachVolume
    { _dv4VolumeId :: !Text
    , _dv4InstanceId :: !(Maybe Text)
    , _dv4Device :: !(Maybe Text)
    , _dv4Force :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachVolume' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Device ::@ @Maybe Text@
--
-- * @Force ::@ @Maybe Bool@
--
mkDetachVolume :: Text -- ^ 'dv4VolumeId'
               -> DetachVolume
mkDetachVolume p1 = DetachVolume
    { _dv4VolumeId = p1
    , _dv4InstanceId = Nothing
    , _dv4Device = Nothing
    , _dv4Force = Nothing
    }

-- | The ID of the volume.
dv4VolumeId :: Lens' DetachVolume Text
dv4VolumeId = lens _dv4VolumeId (\s a -> s { _dv4VolumeId = a })

-- | The ID of the instance.
dv4InstanceId :: Lens' DetachVolume (Maybe Text)
dv4InstanceId = lens _dv4InstanceId (\s a -> s { _dv4InstanceId = a })

-- | The device name.
dv4Device :: Lens' DetachVolume (Maybe Text)
dv4Device = lens _dv4Device (\s a -> s { _dv4Device = a })

-- | Forces detachment if the previous detachment attempt did not occur cleanly
-- (for example, logging into an instance, unmounting the volume, and
-- detaching normally). This option can lead to data loss or a corrupted file
-- system. Use this option only as a last resort to detach a volume from a
-- failed instance. The instance won't have an opportunity to flush file
-- system caches or file system metadata. If you use this option, you must
-- perform file system check and repair procedures.
dv4Force :: Lens' DetachVolume (Maybe Bool)
dv4Force = lens _dv4Force (\s a -> s { _dv4Force = a })

instance ToQuery DetachVolume where
    toQuery = genericQuery def

data DetachVolumeResponse = DetachVolumeResponse
    { _dvr1VolumeId :: !(Maybe Text)
    , _dvr1InstanceId :: !(Maybe Text)
    , _dvr1Device :: !(Maybe Text)
    , _dvr1State :: Maybe VolumeAttachmentState
    , _dvr1AttachTime :: !(Maybe ISO8601)
    , _dvr1DeleteOnTermination :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DetachVolumeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Maybe Text@
--
-- * @InstanceId ::@ @Maybe Text@
--
-- * @Device ::@ @Maybe Text@
--
-- * @State ::@ @Maybe VolumeAttachmentState@
--
-- * @AttachTime ::@ @Maybe ISO8601@
--
-- * @DeleteOnTermination ::@ @Maybe Bool@
--
mkDetachVolumeResponse :: DetachVolumeResponse
mkDetachVolumeResponse = DetachVolumeResponse
    { _dvr1VolumeId = Nothing
    , _dvr1InstanceId = Nothing
    , _dvr1Device = Nothing
    , _dvr1State = Nothing
    , _dvr1AttachTime = Nothing
    , _dvr1DeleteOnTermination = Nothing
    }

-- | The ID of the volume.
dvr1VolumeId :: Lens' DetachVolumeResponse (Maybe Text)
dvr1VolumeId = lens _dvr1VolumeId (\s a -> s { _dvr1VolumeId = a })

-- | The ID of the instance.
dvr1InstanceId :: Lens' DetachVolumeResponse (Maybe Text)
dvr1InstanceId = lens _dvr1InstanceId (\s a -> s { _dvr1InstanceId = a })

-- | The device name.
dvr1Device :: Lens' DetachVolumeResponse (Maybe Text)
dvr1Device = lens _dvr1Device (\s a -> s { _dvr1Device = a })

-- | The attachment state of the volume.
dvr1State :: Lens' DetachVolumeResponse (Maybe VolumeAttachmentState)
dvr1State = lens _dvr1State (\s a -> s { _dvr1State = a })

-- | The time stamp when the attachment initiated.
dvr1AttachTime :: Lens' DetachVolumeResponse (Maybe ISO8601)
dvr1AttachTime = lens _dvr1AttachTime (\s a -> s { _dvr1AttachTime = a })

-- | Indicates whether the Amazon EBS volume is deleted on instance termination.
dvr1DeleteOnTermination :: Lens' DetachVolumeResponse (Maybe Bool)
dvr1DeleteOnTermination =
    lens _dvr1DeleteOnTermination
         (\s a -> s { _dvr1DeleteOnTermination = a })

instance FromXML DetachVolumeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DetachVolume where
    type Sv DetachVolume = EC2
    type Rs DetachVolume = DetachVolumeResponse

    request = post "DetachVolume"
    response _ = xmlResponse
