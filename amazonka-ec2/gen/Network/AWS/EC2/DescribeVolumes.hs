{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified Amazon EBS volumes. For more information about
-- Amazon EBS volumes, see Amazon EBS Volumes in the Amazon Elastic Compute
-- Cloud User Guide. Example This example describes all volumes associated
-- with your account. https://ec2.amazonaws.com/?Action=DescribeVolumes
-- &amp;AUTHPARAMS &lt;DescribeVolumesResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-02-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;volumeSet&gt; &lt;item&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt; &lt;size&gt;80&lt;/size&gt;
-- &lt;snapshotId/&gt;
-- &lt;availabilityZone&gt;us-east-1a&lt;/availabilityZone&gt;
-- &lt;status&gt;in-use&lt;/status&gt;
-- &lt;createTime&gt;YYYY-MM-DDTHH:MM:SS.SSSZ&lt;/createTime&gt;
-- &lt;attachmentSet&gt; &lt;item&gt;
-- &lt;volumeId&gt;vol-1a2b3c4d&lt;/volumeId&gt;
-- &lt;instanceId&gt;i-1a2b3c4d&lt;/instanceId&gt;
-- &lt;device&gt;/dev/sdh&lt;/device&gt; &lt;status&gt;attached&lt;/status&gt;
-- &lt;attachTime&gt;YYYY-MM-DDTHH:MM:SS.SSSZ&lt;/attachTime&gt;
-- &lt;deleteOnTermination&gt;false&lt;/deleteOnTermination&gt; &lt;/item&gt;
-- &lt;/attachmentSet&gt; &lt;volumeType&gt;standard&lt;/volumeType&gt;
-- &lt;encrypted&gt;true&lt;/encrypted&gt; &lt;/item&gt; &lt;/volumeSet&gt;
-- &lt;/DescribeVolumesResponse&gt;.
module Network.AWS.EC2.DescribeVolumes
    (
    -- * Request
      DescribeVolumes
    -- ** Request constructor
    , describeVolumes
    -- ** Request lenses
    , dv2VolumeId
    , dv2Filter

    -- * Response
    , DescribeVolumesResponse
    -- ** Response constructor
    , describeVolumesResponse
    -- ** Response lenses
    , dvrItem
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeVolumes = DescribeVolumes
    { _dv2VolumeId :: [Text]
    , _dv2Filter :: [Filter]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVolumes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @[Text]@
--
-- * @Filter ::@ @[Filter]@
--
describeVolumes :: DescribeVolumes
describeVolumes = DescribeVolumes
    { _dv2VolumeId = mempty
    , _dv2Filter = mempty
    }

-- | One or more volume IDs.
dv2VolumeId :: Lens' DescribeVolumes [Text]
dv2VolumeId = lens _dv2VolumeId (\s a -> s { _dv2VolumeId = a })

-- | One or more filters. attachment.attach-time - The time stamp when the
-- attachment initiated. attachment.delete-on-termination - Whether the volume
-- is deleted on instance termination. attachment.device - The device name
-- that is exposed to the instance (for example, /dev/sda1).
-- attachment.instance-id - The ID of the instance the volume is attached to.
-- attachment.status - The attachment state (attaching | attached | detaching
-- | detached). availability-zone - The Availability Zone in which the volume
-- was created. create-time - The time stamp when the volume was created.
-- encrypted - The encryption status of the volume. size - The size of the
-- volume, in GiB. snapshot-id - The snapshot from which the volume was
-- created. status - The status of the volume (creating | available | in-use |
-- deleting | deleted | error). tag:key=value - The key/value combination of a
-- tag assigned to the resource. tag-key - The key of a tag assigned to the
-- resource. This filter is independent of the tag-value filter. For example,
-- if you use both the filter "tag-key=Purpose" and the filter "tag-value=X",
-- you get any resources assigned both the tag key Purpose (regardless of what
-- the tag's value is), and the tag value X (regardless of what the tag's key
-- is). If you want to list only resources where Purpose is X, see the
-- tag:key=value filter. tag-value - The value of a tag assigned to the
-- resource. This filter is independent of the tag-key filter. volume-id - The
-- volume ID. volume-type - The Amazon EBS volume type. This can be gp2 for
-- General Purpose (SSD) volumes, io1 for Provisioned IOPS (SSD) volumes, or
-- standard for Magnetic volumes.
dv2Filter :: Lens' DescribeVolumes [Filter]
dv2Filter = lens _dv2Filter (\s a -> s { _dv2Filter = a })

instance ToQuery DescribeVolumes where
    toQuery = genericQuery def

newtype DescribeVolumesResponse = DescribeVolumesResponse
    { _dvrItem :: [Volume]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVolumesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Item ::@ @[Volume]@
--
describeVolumesResponse :: DescribeVolumesResponse
describeVolumesResponse = DescribeVolumesResponse
    { _dvrItem = mempty
    }

-- | 
dvrItem :: Lens' DescribeVolumesResponse [Volume]
dvrItem = lens _dvrItem (\s a -> s { _dvrItem = a })

instance FromXML DescribeVolumesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVolumes where
    type Sv DescribeVolumes = EC2
    type Rs DescribeVolumes = DescribeVolumesResponse

    request = post "DescribeVolumes"
    response _ = xmlResponse
