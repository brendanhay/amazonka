{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation lists the iSCSI stored volumes of a gateway. Results are
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
    , lvrsGatewayARN
    , lvrsMarker
    , lvrsVolumeInfos
    , lvrsStatus
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object that contains one or more of the following fields:
--
-- -   ListVolumesInput$Limit
-- -   ListVolumesInput$Marker
--
-- /See:/ 'listVolumes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lvMarker'
--
-- * 'lvLimit'
--
-- * 'lvGatewayARN'
data ListVolumes = ListVolumes'
    { _lvMarker     :: !(Maybe Text)
    , _lvLimit      :: !(Maybe Nat)
    , _lvGatewayARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVolumes' smart constructor.
listVolumes :: Text -> ListVolumes
listVolumes pGatewayARN_ =
    ListVolumes'
    { _lvMarker = Nothing
    , _lvLimit = Nothing
    , _lvGatewayARN = pGatewayARN_
    }

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
          | stop (rs ^. lvrsMarker) = Nothing
          | stop (rs ^. lvrsVolumeInfos) = Nothing
          | otherwise =
            Just $ rq & lvMarker .~ rs ^. lvrsMarker

instance AWSRequest ListVolumes where
        type Sv ListVolumes = StorageGateway
        type Rs ListVolumes = ListVolumesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumesResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "Marker") <*>
                     (x .?> "VolumeInfos" .!@ mempty)
                     <*> (pure (fromEnum s)))

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
-- * 'lvrsGatewayARN'
--
-- * 'lvrsMarker'
--
-- * 'lvrsVolumeInfos'
--
-- * 'lvrsStatus'
data ListVolumesResponse = ListVolumesResponse'
    { _lvrsGatewayARN  :: !(Maybe Text)
    , _lvrsMarker      :: !(Maybe Text)
    , _lvrsVolumeInfos :: !(Maybe [VolumeInfo])
    , _lvrsStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListVolumesResponse' smart constructor.
listVolumesResponse :: Int -> ListVolumesResponse
listVolumesResponse pStatus_ =
    ListVolumesResponse'
    { _lvrsGatewayARN = Nothing
    , _lvrsMarker = Nothing
    , _lvrsVolumeInfos = Nothing
    , _lvrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
lvrsGatewayARN :: Lens' ListVolumesResponse (Maybe Text)
lvrsGatewayARN = lens _lvrsGatewayARN (\ s a -> s{_lvrsGatewayARN = a});

-- | FIXME: Undocumented member.
lvrsMarker :: Lens' ListVolumesResponse (Maybe Text)
lvrsMarker = lens _lvrsMarker (\ s a -> s{_lvrsMarker = a});

-- | FIXME: Undocumented member.
lvrsVolumeInfos :: Lens' ListVolumesResponse [VolumeInfo]
lvrsVolumeInfos = lens _lvrsVolumeInfos (\ s a -> s{_lvrsVolumeInfos = a}) . _Default;

-- | FIXME: Undocumented member.
lvrsStatus :: Lens' ListVolumesResponse Int
lvrsStatus = lens _lvrsStatus (\ s a -> s{_lvrsStatus = a});
