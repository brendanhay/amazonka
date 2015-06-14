{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.ListVolumes
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation lists the iSCSI stored volumes of a gateway. Results are
-- sorted by volume ARN. The response includes only the volume ARNs. If you
-- want additional volume information, use the DescribeStorediSCSIVolumes
-- API.
--
-- The operation supports pagination. By default, the operation returns a
-- maximum of up to 100 volumes. You can optionally specify the @Limit@
-- field in the body to limit the number of volumes in the response. If the
-- number of volumes returned in the response is truncated, the response
-- includes a Marker field. You can use this Marker value in your
-- subsequent request to retrieve the next set of volumes.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_ListVolumes.html>
module Network.AWS.StorageGateway.ListVolumes
    (
    -- * Request
      ListVolumes
    -- ** Request constructor
    , listVolumes
    -- ** Request lenses
    , lvGatewayARN
    , lvMarker
    , lvLimit

    -- * Response
    , ListVolumesResponse
    -- ** Response constructor
    , listVolumesResponse
    -- ** Response lenses
    , lvrVolumeInfos
    , lvrGatewayARN
    , lvrMarker
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'listVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvGatewayARN'
--
-- * 'lvMarker'
--
-- * 'lvLimit'
data ListVolumes = ListVolumes'{_lvGatewayARN :: Text, _lvMarker :: Text, _lvLimit :: Nat} deriving (Eq, Read, Show)

-- | 'ListVolumes' smart constructor.
listVolumes :: Text -> Text -> Natural -> ListVolumes
listVolumes pGatewayARN pMarker pLimit = ListVolumes'{_lvGatewayARN = pGatewayARN, _lvMarker = pMarker, _lvLimit = _Nat # pLimit};

-- | FIXME: Undocumented member.
lvGatewayARN :: Lens' ListVolumes Text
lvGatewayARN = lens _lvGatewayARN (\ s a -> s{_lvGatewayARN = a});

-- | A string that indicates the position at which to begin the returned list
-- of volumes. Obtain the marker from the response of a previous List iSCSI
-- Volumes request.
lvMarker :: Lens' ListVolumes Text
lvMarker = lens _lvMarker (\ s a -> s{_lvMarker = a});

-- | Specifies that the list of volumes returned be limited to the specified
-- number of items.
lvLimit :: Lens' ListVolumes Natural
lvLimit = lens _lvLimit (\ s a -> s{_lvLimit = a}) . _Nat;

instance AWSRequest ListVolumes where
        type Sv ListVolumes = StorageGateway
        type Rs ListVolumes = ListVolumesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumesResponse' <$>
                   x .?> "VolumeInfos" .!@ mempty <*> x .:> "GatewayARN"
                     <*> x .:> "Marker")

instance ToHeaders ListVolumes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.ListVolumes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListVolumes where
        toJSON ListVolumes'{..}
          = object
              ["GatewayARN" .= _lvGatewayARN,
               "Marker" .= _lvMarker, "Limit" .= _lvLimit]

instance ToPath ListVolumes where
        toPath = const "/"

instance ToQuery ListVolumes where
        toQuery = const mempty

-- | /See:/ 'listVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvrVolumeInfos'
--
-- * 'lvrGatewayARN'
--
-- * 'lvrMarker'
data ListVolumesResponse = ListVolumesResponse'{_lvrVolumeInfos :: [VolumeInfo], _lvrGatewayARN :: Text, _lvrMarker :: Text} deriving (Eq, Read, Show)

-- | 'ListVolumesResponse' smart constructor.
listVolumesResponse :: Text -> Text -> ListVolumesResponse
listVolumesResponse pGatewayARN pMarker = ListVolumesResponse'{_lvrVolumeInfos = mempty, _lvrGatewayARN = pGatewayARN, _lvrMarker = pMarker};

-- | FIXME: Undocumented member.
lvrVolumeInfos :: Lens' ListVolumesResponse [VolumeInfo]
lvrVolumeInfos = lens _lvrVolumeInfos (\ s a -> s{_lvrVolumeInfos = a});

-- | FIXME: Undocumented member.
lvrGatewayARN :: Lens' ListVolumesResponse Text
lvrGatewayARN = lens _lvrGatewayARN (\ s a -> s{_lvrGatewayARN = a});

-- | FIXME: Undocumented member.
lvrMarker :: Lens' ListVolumesResponse Text
lvrMarker = lens _lvrMarker (\ s a -> s{_lvrMarker = a});
