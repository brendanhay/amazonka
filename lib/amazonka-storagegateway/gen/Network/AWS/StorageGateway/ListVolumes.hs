{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the iSCSI stored volumes of a gateway. Results are sorted by volume ARN. The response includes only the volume ARNs. If you want additional volume information, use the 'DescribeStorediSCSIVolumes' or the 'DescribeCachediSCSIVolumes' API.
--
--
-- The operation supports pagination. By default, the operation returns a maximum of up to 100 volumes. You can optionally specify the @Limit@ field in the body to limit the number of volumes in the response. If the number of volumes returned in the response is truncated, the response includes a Marker field. You can use this Marker value in your subsequent request to retrieve the next set of volumes. This operation is only supported in the cached volume and stored volume gateway types.
--
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListVolumes
    (
    -- * Creating a Request
      listVolumes
    , ListVolumes
    -- * Request Lenses
    , lvGatewayARN
    , lvMarker
    , lvLimit

    -- * Destructuring the Response
    , listVolumesResponse
    , ListVolumesResponse
    -- * Response Lenses
    , lvrsGatewayARN
    , lvrsMarker
    , lvrsVolumeInfos
    , lvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object that contains one or more of the following fields:
--
--
--     * 'ListVolumesInput$Limit'
--
--     * 'ListVolumesInput$Marker'
--
--
--
--
-- /See:/ 'listVolumes' smart constructor.
data ListVolumes = ListVolumes'
  { _lvGatewayARN :: !(Maybe Text)
  , _lvMarker     :: !(Maybe Text)
  , _lvLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVolumes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvGatewayARN' - Undocumented member.
--
-- * 'lvMarker' - A string that indicates the position at which to begin the returned list of volumes. Obtain the marker from the response of a previous List iSCSI Volumes request.
--
-- * 'lvLimit' - Specifies that the list of volumes returned be limited to the specified number of items.
listVolumes
    :: ListVolumes
listVolumes =
  ListVolumes'
    {_lvGatewayARN = Nothing, _lvMarker = Nothing, _lvLimit = Nothing}


-- | Undocumented member.
lvGatewayARN :: Lens' ListVolumes (Maybe Text)
lvGatewayARN = lens _lvGatewayARN (\ s a -> s{_lvGatewayARN = a})

-- | A string that indicates the position at which to begin the returned list of volumes. Obtain the marker from the response of a previous List iSCSI Volumes request.
lvMarker :: Lens' ListVolumes (Maybe Text)
lvMarker = lens _lvMarker (\ s a -> s{_lvMarker = a})

-- | Specifies that the list of volumes returned be limited to the specified number of items.
lvLimit :: Lens' ListVolumes (Maybe Natural)
lvLimit = lens _lvLimit (\ s a -> s{_lvLimit = a}) . mapping _Nat

instance AWSPager ListVolumes where
        page rq rs
          | stop (rs ^. lvrsMarker) = Nothing
          | stop (rs ^. lvrsVolumeInfos) = Nothing
          | otherwise =
            Just $ rq & lvMarker .~ rs ^. lvrsMarker

instance AWSRequest ListVolumes where
        type Rs ListVolumes = ListVolumesResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 ListVolumesResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "Marker") <*>
                     (x .?> "VolumeInfos" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListVolumes where

instance NFData ListVolumes where

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
              (catMaybes
                 [("GatewayARN" .=) <$> _lvGatewayARN,
                  ("Marker" .=) <$> _lvMarker,
                  ("Limit" .=) <$> _lvLimit])

instance ToPath ListVolumes where
        toPath = const "/"

instance ToQuery ListVolumes where
        toQuery = const mempty

-- | /See:/ 'listVolumesResponse' smart constructor.
data ListVolumesResponse = ListVolumesResponse'
  { _lvrsGatewayARN     :: !(Maybe Text)
  , _lvrsMarker         :: !(Maybe Text)
  , _lvrsVolumeInfos    :: !(Maybe [VolumeInfo])
  , _lvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListVolumesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lvrsGatewayARN' - Undocumented member.
--
-- * 'lvrsMarker' - Undocumented member.
--
-- * 'lvrsVolumeInfos' - Undocumented member.
--
-- * 'lvrsResponseStatus' - -- | The response status code.
listVolumesResponse
    :: Int -- ^ 'lvrsResponseStatus'
    -> ListVolumesResponse
listVolumesResponse pResponseStatus_ =
  ListVolumesResponse'
    { _lvrsGatewayARN = Nothing
    , _lvrsMarker = Nothing
    , _lvrsVolumeInfos = Nothing
    , _lvrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lvrsGatewayARN :: Lens' ListVolumesResponse (Maybe Text)
lvrsGatewayARN = lens _lvrsGatewayARN (\ s a -> s{_lvrsGatewayARN = a})

-- | Undocumented member.
lvrsMarker :: Lens' ListVolumesResponse (Maybe Text)
lvrsMarker = lens _lvrsMarker (\ s a -> s{_lvrsMarker = a})

-- | Undocumented member.
lvrsVolumeInfos :: Lens' ListVolumesResponse [VolumeInfo]
lvrsVolumeInfos = lens _lvrsVolumeInfos (\ s a -> s{_lvrsVolumeInfos = a}) . _Default . _Coerce

-- | -- | The response status code.
lvrsResponseStatus :: Lens' ListVolumesResponse Int
lvrsResponseStatus = lens _lvrsResponseStatus (\ s a -> s{_lvrsResponseStatus = a})

instance NFData ListVolumesResponse where
