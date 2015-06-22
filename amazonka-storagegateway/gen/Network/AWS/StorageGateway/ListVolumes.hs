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
    , lvMarker
    , lvLimit
    , lvGatewayARN

    -- * Response
    , ListVolumesResponse
    -- ** Response constructor
    , listVolumesResponse
    -- ** Response lenses
    , lvrGatewayARN
    , lvrMarker
    , lvrVolumeInfos
    ) where

import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'listVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvMarker'
--
-- * 'lvLimit'
--
-- * 'lvGatewayARN'
data ListVolumes = ListVolumes'{_lvMarker :: Maybe Text, _lvLimit :: Maybe Nat, _lvGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'ListVolumes' smart constructor.
listVolumes :: Text -> ListVolumes
listVolumes pGatewayARN = ListVolumes'{_lvMarker = Nothing, _lvLimit = Nothing, _lvGatewayARN = pGatewayARN};

-- | A string that indicates the position at which to begin the returned list
-- of volumes. Obtain the marker from the response of a previous List iSCSI
-- Volumes request.
lvMarker :: Lens' ListVolumes (Maybe Text)
lvMarker = lens _lvMarker (\ s a -> s{_lvMarker = a});

-- | Specifies that the list of volumes returned be limited to the specified
-- number of items.
lvLimit :: Lens' ListVolumes (Maybe Natural)
lvLimit = lens _lvLimit (\ s a -> s{_lvLimit = a}) . mapping _Nat;

-- | FIXME: Undocumented member.
lvGatewayARN :: Lens' ListVolumes Text
lvGatewayARN = lens _lvGatewayARN (\ s a -> s{_lvGatewayARN = a});

instance AWSPager ListVolumes where
        page rq rs
          | stop (rs ^. lvrMarker) = Nothing
          | stop (rs ^. lvrVolumeInfos) = Nothing
          | otherwise = Just $ rq & lvMarker .~ rs ^. lvrMarker

instance AWSRequest ListVolumes where
        type Sv ListVolumes = StorageGateway
        type Rs ListVolumes = ListVolumesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumesResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "Marker") <*>
                     (x .?> "VolumeInfos" .!@ mempty))

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
              ["Marker" .= _lvMarker, "Limit" .= _lvLimit,
               "GatewayARN" .= _lvGatewayARN]

instance ToPath ListVolumes where
        toPath = const "/"

instance ToQuery ListVolumes where
        toQuery = const mempty

-- | /See:/ 'listVolumesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvrGatewayARN'
--
-- * 'lvrMarker'
--
-- * 'lvrVolumeInfos'
data ListVolumesResponse = ListVolumesResponse'{_lvrGatewayARN :: Maybe Text, _lvrMarker :: Maybe Text, _lvrVolumeInfos :: Maybe [VolumeInfo]} deriving (Eq, Read, Show)

-- | 'ListVolumesResponse' smart constructor.
listVolumesResponse :: ListVolumesResponse
listVolumesResponse = ListVolumesResponse'{_lvrGatewayARN = Nothing, _lvrMarker = Nothing, _lvrVolumeInfos = Nothing};

-- | FIXME: Undocumented member.
lvrGatewayARN :: Lens' ListVolumesResponse (Maybe Text)
lvrGatewayARN = lens _lvrGatewayARN (\ s a -> s{_lvrGatewayARN = a});

-- | FIXME: Undocumented member.
lvrMarker :: Lens' ListVolumesResponse (Maybe Text)
lvrMarker = lens _lvrMarker (\ s a -> s{_lvrMarker = a});

-- | FIXME: Undocumented member.
lvrVolumeInfos :: Lens' ListVolumesResponse [VolumeInfo]
lvrVolumeInfos = lens _lvrVolumeInfos (\ s a -> s{_lvrVolumeInfos = a}) . _Default;
