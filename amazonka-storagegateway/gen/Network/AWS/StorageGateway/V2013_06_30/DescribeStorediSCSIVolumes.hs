{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DescribeStorediSCSIVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns description of the gateway volumes specified in the
-- request. The list of gateway volumes in the request must be from one
-- gateway. In the response Amazon Storage Gateway returns volume information
-- sorted by volume ARNs. Example Request The following example shows a
-- request that returns a description of a volume. POST / HTTP/1.1 Host:
-- storagegateway.us-east-1.amazonaws.com x-amz-Date: 20120425T120000Z
-- Authorization: CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG
-- Content-type: application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.DescribeStorediSCSIVolumes { "VolumeARNs":
-- ["arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB"]
-- } HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 507 {
-- "StorediSCSIVolumes": [ { "VolumeiSCSIAttributes": { "ChapEnabled": true,
-- "NetworkInterfaceId": "10.243.43.207", "NetworkInterfacePort": 3260,
-- "TargetARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/target/iqn.1997-05.com.amazon:myvolume"
-- }, "PreservedExistingData": false, "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/myg
-- ateway/volume/vol-1122AABB", "VolumeDiskId":
-- "pci-0000:03:00.0-scsi-0:0:0:0", "VolumeId": "vol-1122AABB",
-- "VolumeProgress": 23.7, "VolumeSizeInBytes": 1099511627776, "VolumeStatus":
-- "BOOTSTRAPPING" } ] }.
module Network.AWS.StorageGateway.V2013_06_30.DescribeStorediSCSIVolumes
    (
    -- * Request
      DescribeStorediSCSIVolumes
    -- ** Request constructor
    , mkDescribeStorediSCSIVolumes
    -- ** Request lenses
    , dsscsivVolumeARNs

    -- * Response
    , DescribeStorediSCSIVolumesResponse
    -- ** Response lenses
    , dsscsivrsStorediSCSIVolumes
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | A JSON Object containing a list of
-- DescribeStorediSCSIVolumesInput$VolumeARNs.
newtype DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumes
    { _dsscsivVolumeARNs :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeStorediSCSIVolumes' request.
mkDescribeStorediSCSIVolumes :: [Text] -- ^ 'dsscsivVolumeARNs'
                             -> DescribeStorediSCSIVolumes
mkDescribeStorediSCSIVolumes p1 = DescribeStorediSCSIVolumes
    { _dsscsivVolumeARNs = p1
    }

-- | An array of strings where each string represents the Amazon Resource Name
-- (ARN) of a stored volume. All of the specified stored volumes must from the
-- same gateway. Use ListVolumes to get volume ARNs for a gateway.
dsscsivVolumeARNs :: Lens' DescribeStorediSCSIVolumes [Text]
dsscsivVolumeARNs =
    lens _dsscsivVolumeARNs (\s a -> s { _dsscsivVolumeARNs = a })

instance ToPath DescribeStorediSCSIVolumes

instance ToQuery DescribeStorediSCSIVolumes

instance ToHeaders DescribeStorediSCSIVolumes

instance ToJSON DescribeStorediSCSIVolumes

newtype DescribeStorediSCSIVolumesResponse = DescribeStorediSCSIVolumesResponse
    { _dsscsivrsStorediSCSIVolumes :: [StorediSCSIVolumeInformation]
    } deriving (Show, Generic)

dsscsivrsStorediSCSIVolumes :: Lens' DescribeStorediSCSIVolumesResponse [StorediSCSIVolumeInformation]
dsscsivrsStorediSCSIVolumes =
    lens _dsscsivrsStorediSCSIVolumes
         (\s a -> s { _dsscsivrsStorediSCSIVolumes = a })

instance FromJSON DescribeStorediSCSIVolumesResponse

instance AWSRequest DescribeStorediSCSIVolumes where
    type Sv DescribeStorediSCSIVolumes = StorageGateway
    type Rs DescribeStorediSCSIVolumes = DescribeStorediSCSIVolumesResponse

    request = get
    response _ = jsonResponse
