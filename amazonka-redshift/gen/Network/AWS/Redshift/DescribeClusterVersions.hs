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
-- Module      : Network.AWS.Redshift.DescribeClusterVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptions of the available Amazon Redshift cluster versions. You can call this operation even before creating any clusters to learn more about the Amazon Redshift versions. For more information about managing clusters, go to <http://docs.aws.amazon.com/redshift/latest/mgmt/working-with-clusters.html Amazon Redshift Clusters> in the /Amazon Redshift Cluster Management Guide/ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Redshift.DescribeClusterVersions
    (
    -- * Creating a Request
      describeClusterVersions
    , DescribeClusterVersions
    -- * Request Lenses
    , dcvClusterParameterGroupFamily
    , dcvMarker
    , dcvMaxRecords
    , dcvClusterVersion

    -- * Destructuring the Response
    , describeClusterVersionsResponse
    , DescribeClusterVersionsResponse
    -- * Response Lenses
    , dcvrsClusterVersions
    , dcvrsMarker
    , dcvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeClusterVersions' smart constructor.
data DescribeClusterVersions = DescribeClusterVersions'
  { _dcvClusterParameterGroupFamily :: !(Maybe Text)
  , _dcvMarker                      :: !(Maybe Text)
  , _dcvMaxRecords                  :: !(Maybe Int)
  , _dcvClusterVersion              :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvClusterParameterGroupFamily' - The name of a specific cluster parameter group family to return details for. Constraints:     * Must be 1 to 255 alphanumeric characters     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
--
-- * 'dcvMarker' - An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
--
-- * 'dcvMaxRecords' - The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
--
-- * 'dcvClusterVersion' - The specific cluster version to return. Example: @1.0@
describeClusterVersions
    :: DescribeClusterVersions
describeClusterVersions =
  DescribeClusterVersions'
    { _dcvClusterParameterGroupFamily = Nothing
    , _dcvMarker = Nothing
    , _dcvMaxRecords = Nothing
    , _dcvClusterVersion = Nothing
    }


-- | The name of a specific cluster parameter group family to return details for. Constraints:     * Must be 1 to 255 alphanumeric characters     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
dcvClusterParameterGroupFamily :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterParameterGroupFamily = lens _dcvClusterParameterGroupFamily (\ s a -> s{_dcvClusterParameterGroupFamily = a})

-- | An optional parameter that specifies the starting point to return a set of response records. When the results of a 'DescribeClusterVersions' request exceed the value specified in @MaxRecords@ , AWS returns a value in the @Marker@ field of the response. You can retrieve the next set of response records by providing the returned marker value in the @Marker@ parameter and retrying the request.
dcvMarker :: Lens' DescribeClusterVersions (Maybe Text)
dcvMarker = lens _dcvMarker (\ s a -> s{_dcvMarker = a})

-- | The maximum number of response records to return in each call. If the number of remaining response records exceeds the specified @MaxRecords@ value, a value is returned in a @marker@ field of the response. You can retrieve the next set of records by retrying the command with the returned marker value.  Default: @100@  Constraints: minimum 20, maximum 100.
dcvMaxRecords :: Lens' DescribeClusterVersions (Maybe Int)
dcvMaxRecords = lens _dcvMaxRecords (\ s a -> s{_dcvMaxRecords = a})

-- | The specific cluster version to return. Example: @1.0@
dcvClusterVersion :: Lens' DescribeClusterVersions (Maybe Text)
dcvClusterVersion = lens _dcvClusterVersion (\ s a -> s{_dcvClusterVersion = a})

instance AWSPager DescribeClusterVersions where
        page rq rs
          | stop (rs ^. dcvrsMarker) = Nothing
          | stop (rs ^. dcvrsClusterVersions) = Nothing
          | otherwise =
            Just $ rq & dcvMarker .~ rs ^. dcvrsMarker

instance AWSRequest DescribeClusterVersions where
        type Rs DescribeClusterVersions =
             DescribeClusterVersionsResponse
        request = postQuery redshift
        response
          = receiveXMLWrapper "DescribeClusterVersionsResult"
              (\ s h x ->
                 DescribeClusterVersionsResponse' <$>
                   (x .@? "ClusterVersions" .!@ mempty >>=
                      may (parseXMLList "ClusterVersion"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeClusterVersions where

instance NFData DescribeClusterVersions where

instance ToHeaders DescribeClusterVersions where
        toHeaders = const mempty

instance ToPath DescribeClusterVersions where
        toPath = const "/"

instance ToQuery DescribeClusterVersions where
        toQuery DescribeClusterVersions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeClusterVersions" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "ClusterParameterGroupFamily" =:
                 _dcvClusterParameterGroupFamily,
               "Marker" =: _dcvMarker,
               "MaxRecords" =: _dcvMaxRecords,
               "ClusterVersion" =: _dcvClusterVersion]

-- | Contains the output from the 'DescribeClusterVersions' action.
--
--
--
-- /See:/ 'describeClusterVersionsResponse' smart constructor.
data DescribeClusterVersionsResponse = DescribeClusterVersionsResponse'
  { _dcvrsClusterVersions :: !(Maybe [ClusterVersion])
  , _dcvrsMarker          :: !(Maybe Text)
  , _dcvrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeClusterVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcvrsClusterVersions' - A list of @Version@ elements.
--
-- * 'dcvrsMarker' - A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
--
-- * 'dcvrsResponseStatus' - -- | The response status code.
describeClusterVersionsResponse
    :: Int -- ^ 'dcvrsResponseStatus'
    -> DescribeClusterVersionsResponse
describeClusterVersionsResponse pResponseStatus_ =
  DescribeClusterVersionsResponse'
    { _dcvrsClusterVersions = Nothing
    , _dcvrsMarker = Nothing
    , _dcvrsResponseStatus = pResponseStatus_
    }


-- | A list of @Version@ elements.
dcvrsClusterVersions :: Lens' DescribeClusterVersionsResponse [ClusterVersion]
dcvrsClusterVersions = lens _dcvrsClusterVersions (\ s a -> s{_dcvrsClusterVersions = a}) . _Default . _Coerce

-- | A value that indicates the starting point for the next set of response records in a subsequent request. If a value is returned in a response, you can retrieve the next set of records by providing this returned marker value in the @Marker@ parameter and retrying the command. If the @Marker@ field is empty, all response records have been retrieved for the request.
dcvrsMarker :: Lens' DescribeClusterVersionsResponse (Maybe Text)
dcvrsMarker = lens _dcvrsMarker (\ s a -> s{_dcvrsMarker = a})

-- | -- | The response status code.
dcvrsResponseStatus :: Lens' DescribeClusterVersionsResponse Int
dcvrsResponseStatus = lens _dcvrsResponseStatus (\ s a -> s{_dcvrsResponseStatus = a})

instance NFData DescribeClusterVersionsResponse where
