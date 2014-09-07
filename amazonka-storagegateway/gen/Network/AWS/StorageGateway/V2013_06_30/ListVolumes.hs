{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.ListVolumes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation lists the iSCSI stored volumes of a gateway. Results are
-- sorted by volume ARN. The response includes only the volume ARNs. If you
-- want additional volume information, use the DescribeStorediSCSIVolumes API.
-- The operation supports pagination. By default, the operation returns a
-- maximum of up to 100 volumes. You can optionally specify the Limit field in
-- the body to limit the number of volumes in the response. If the number of
-- volumes returned in the response is truncated, the response includes a
-- Marker field. You can use this Marker value in your subsequent request to
-- retrieve the next set of volumes. Example Request The List iSCSI Volumes
-- request in this example does not specify a limit or marker field in the
-- response body. The response returns the volumes (up to the first 100) of
-- the gateway. POST / HTTP/1.1 Host: storagegateway.us-east-1.amazonaws.com
-- x-amz-Date: 20120425T120000Z Authorization:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Content-type:
-- application/x-amz-json-1.1 x-amz-target:
-- StorageGateway_20120630.ListVolumes { "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway" }
-- HTTP/1.1 200 OK x-amzn-RequestId:
-- CSOC7TJPLR0OOKIRLGOHVAICUFVV4KQNSO5AEMVJF66Q9ASUAAJG Date: Wed, 25 Apr 2012
-- 12:00:02 GMT Content-type: application/x-amz-json-1.1 Content-length: 346 {
-- "GatewayARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway",
-- "VolumeInfos": [ { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-1122AABB",
-- "VolumeType": "STORED" }, { "VolumeARN":
-- "arn:aws:storagegateway:us-east-1:111122223333:gateway/mygateway/volume/vol-3344CCDD",
-- "VolumeType": "STORED" }, ] }.
module Network.AWS.StorageGateway.V2013_06_30.ListVolumes
    (
    -- * Request
      ListVolumes
    -- ** Request constructor
    , mkListVolumes
    -- ** Request lenses
    , lvGatewayARN
    , lvMarker
    , lvLimit

    -- * Response
    , ListVolumesResponse
    -- ** Response lenses
    , lvrsGatewayARN
    , lvrsMarker
    , lvrsVolumeInfos
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | A JSON object that contains one or more of the following fields:
-- ListVolumesInput$Limit ListVolumesInput$Marker.
data ListVolumes = ListVolumes
    { _lvGatewayARN :: Text
    , _lvMarker :: Maybe Text
    , _lvLimit :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListVolumes' request.
mkListVolumes :: Text -- ^ 'lvGatewayARN'
              -> ListVolumes
mkListVolumes p1 = ListVolumes
    { _lvGatewayARN = p1
    , _lvMarker = Nothing
    , _lvLimit = Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
lvGatewayARN :: Lens' ListVolumes Text
lvGatewayARN = lens _lvGatewayARN (\s a -> s { _lvGatewayARN = a })

-- | A string that indicates the position at which to begin the returned list of
-- volumes. Obtain the marker from the response of a previous List iSCSI
-- Volumes request.
lvMarker :: Lens' ListVolumes (Maybe Text)
lvMarker = lens _lvMarker (\s a -> s { _lvMarker = a })

-- | Specifies that the list of volumes returned be limited to the specified
-- number of items.
lvLimit :: Lens' ListVolumes (Maybe Integer)
lvLimit = lens _lvLimit (\s a -> s { _lvLimit = a })

instance ToPath ListVolumes

instance ToQuery ListVolumes

instance ToHeaders ListVolumes

instance ToJSON ListVolumes

data ListVolumesResponse = ListVolumesResponse
    { _lvrsGatewayARN :: Maybe Text
    , _lvrsMarker :: Maybe Text
    , _lvrsVolumeInfos :: [VolumeInformation]
    } deriving (Show, Generic)

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and region.
lvrsGatewayARN :: Lens' ListVolumesResponse (Maybe Text)
lvrsGatewayARN = lens _lvrsGatewayARN (\s a -> s { _lvrsGatewayARN = a })

lvrsMarker :: Lens' ListVolumesResponse (Maybe Text)
lvrsMarker = lens _lvrsMarker (\s a -> s { _lvrsMarker = a })

lvrsVolumeInfos :: Lens' ListVolumesResponse [VolumeInformation]
lvrsVolumeInfos = lens _lvrsVolumeInfos (\s a -> s { _lvrsVolumeInfos = a })

instance FromJSON ListVolumesResponse

instance AWSRequest ListVolumes where
    type Sv ListVolumes = StorageGateway
    type Rs ListVolumes = ListVolumesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListVolumes where
    next rq rs = (\x -> rq & lvMarker ?~ x)
        <$> (rs ^. lvrsMarker)
