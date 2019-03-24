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
-- Module      : Network.AWS.Redshift.DescribeClusterTracks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the available maintenance tracks.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterTracks
    (
    -- * Creating a Request
      describeClusterTracks
    , DescribeClusterTracks
    -- * Request Lenses
    , dctMaintenanceTrackName
    , dctMarker
    , dctMaxRecords

    -- * Destructuring the Response
    , describeClusterTracksResponse
    , DescribeClusterTracksResponse
    -- * Response Lenses
    , dctrsMaintenanceTracks
    , dctrsMarker
    , dctrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeClusterTracks' smart constructor.
data DescribeClusterTracks = DescribeClusterTracks'
  { _dctMaintenanceTrackName :: !(Maybe Text)
  , _dctMarker               :: !(Maybe Text)
  , _dctMaxRecords           :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterTracks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dctMaintenanceTrackName' - The name of the maintenance track.
--
-- * 'dctMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeClusterTracks@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dctMaxRecords' - An integer value for the maximum number of maintenance tracks to return.
describeClusterTracks
    :: DescribeClusterTracks
describeClusterTracks =
  DescribeClusterTracks'
    { _dctMaintenanceTrackName = Nothing
    , _dctMarker = Nothing
    , _dctMaxRecords = Nothing
    }


-- | The name of the maintenance track.
dctMaintenanceTrackName :: Lens' DescribeClusterTracks (Maybe Text)
dctMaintenanceTrackName = lens _dctMaintenanceTrackName (\ s a -> s{_dctMaintenanceTrackName = a})

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a @DescribeClusterTracks@ request exceed the value specified in @MaxRecords@ , Amazon Redshift returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dctMarker :: Lens' DescribeClusterTracks (Maybe Text)
dctMarker = lens _dctMarker (\ s a -> s{_dctMarker = a})

-- | An integer value for the maximum number of maintenance tracks to return.
dctMaxRecords :: Lens' DescribeClusterTracks (Maybe Int)
dctMaxRecords = lens _dctMaxRecords (\ s a -> s{_dctMaxRecords = a})

instance AWSPager DescribeClusterTracks where
        page rq rs
          | stop (rs ^. dctrsMarker) = Nothing
          | stop (rs ^. dctrsMaintenanceTracks) = Nothing
          | otherwise =
            Just $ rq & dctMarker .~ rs ^. dctrsMarker

instance AWSRequest DescribeClusterTracks where
        type Rs DescribeClusterTracks =
             DescribeClusterTracksResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DescribeClusterTracksResult"
              (\ s h x ->
                 DescribeClusterTracksResponse' <$>
                   (x .@? "MaintenanceTracks" .!@ mempty >>=
                      may (parseXMLList "MaintenanceTrack"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusterTracks where

instance NFData DescribeClusterTracks where

instance ToHeaders DescribeClusterTracks where
        toHeaders = const mempty

instance ToPath DescribeClusterTracks where
        toPath = const "/"

instance ToQuery DescribeClusterTracks where
        toQuery DescribeClusterTracks'{..}
          = mconcat
              ["Action" =: ("DescribeClusterTracks" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "MaintenanceTrackName" =: _dctMaintenanceTrackName,
               "Marker" =: _dctMarker,
               "MaxRecords" =: _dctMaxRecords]

-- | /See:/ 'describeClusterTracksResponse' smart constructor.
data DescribeClusterTracksResponse = DescribeClusterTracksResponse'
  { _dctrsMaintenanceTracks :: !(Maybe [MaintenanceTrack])
  , _dctrsMarker            :: !(Maybe Text)
  , _dctrsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterTracksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dctrsMaintenanceTracks' - A list of maintenance tracks output by the @DescribeClusterTracks@ operation.
--
-- * 'dctrsMarker' - The starting point to return a set of response tracklist records. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dctrsResponseStatus' - -- | The response status code.
describeClusterTracksResponse
    :: Int -- ^ 'dctrsResponseStatus'
    -> DescribeClusterTracksResponse
describeClusterTracksResponse pResponseStatus_ =
  DescribeClusterTracksResponse'
    { _dctrsMaintenanceTracks = Nothing
    , _dctrsMarker = Nothing
    , _dctrsResponseStatus = pResponseStatus_
    }


-- | A list of maintenance tracks output by the @DescribeClusterTracks@ operation.
dctrsMaintenanceTracks :: Lens' DescribeClusterTracksResponse [MaintenanceTrack]
dctrsMaintenanceTracks = lens _dctrsMaintenanceTracks (\ s a -> s{_dctrsMaintenanceTracks = a}) . _Default . _Coerce

-- | The starting point to return a set of response tracklist records. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dctrsMarker :: Lens' DescribeClusterTracksResponse (Maybe Text)
dctrsMarker = lens _dctrsMarker (\ s a -> s{_dctrsMarker = a})

-- | -- | The response status code.
dctrsResponseStatus :: Lens' DescribeClusterTracksResponse Int
dctrsResponseStatus = lens _dctrsResponseStatus (\ s a -> s{_dctrsResponseStatus = a})

instance NFData DescribeClusterTracksResponse where
