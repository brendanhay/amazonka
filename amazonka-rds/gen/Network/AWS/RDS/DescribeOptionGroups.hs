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
-- Module      : Network.AWS.RDS.DescribeOptionGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available option groups.
--
--
--
-- This operation returns paginated results.
module Network.AWS.RDS.DescribeOptionGroups
    (
    -- * Creating a Request
      describeOptionGroups
    , DescribeOptionGroups
    -- * Request Lenses
    , dogFilters
    , dogEngineName
    , dogMajorEngineVersion
    , dogMarker
    , dogMaxRecords
    , dogOptionGroupName

    -- * Destructuring the Response
    , describeOptionGroupsResponse
    , DescribeOptionGroupsResponse
    -- * Response Lenses
    , dogrsMarker
    , dogrsOptionGroupsList
    , dogrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'describeOptionGroups' smart constructor.
data DescribeOptionGroups = DescribeOptionGroups'
  { _dogFilters            :: !(Maybe [Filter])
  , _dogEngineName         :: !(Maybe Text)
  , _dogMajorEngineVersion :: !(Maybe Text)
  , _dogMarker             :: !(Maybe Text)
  , _dogMaxRecords         :: !(Maybe Int)
  , _dogOptionGroupName    :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOptionGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dogFilters' - This parameter is not currently supported.
--
-- * 'dogEngineName' - Filters the list of option groups to only include groups associated with a specific database engine.
--
-- * 'dogMajorEngineVersion' - Filters the list of option groups to only include groups associated with a specific database engine version. If specified, then EngineName must also be specified.
--
-- * 'dogMarker' - An optional pagination token provided by a previous DescribeOptionGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dogMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
--
-- * 'dogOptionGroupName' - The name of the option group to describe. Cannot be supplied together with EngineName or MajorEngineVersion.
describeOptionGroups
    :: DescribeOptionGroups
describeOptionGroups =
  DescribeOptionGroups'
    { _dogFilters = Nothing
    , _dogEngineName = Nothing
    , _dogMajorEngineVersion = Nothing
    , _dogMarker = Nothing
    , _dogMaxRecords = Nothing
    , _dogOptionGroupName = Nothing
    }


-- | This parameter is not currently supported.
dogFilters :: Lens' DescribeOptionGroups [Filter]
dogFilters = lens _dogFilters (\ s a -> s{_dogFilters = a}) . _Default . _Coerce

-- | Filters the list of option groups to only include groups associated with a specific database engine.
dogEngineName :: Lens' DescribeOptionGroups (Maybe Text)
dogEngineName = lens _dogEngineName (\ s a -> s{_dogEngineName = a})

-- | Filters the list of option groups to only include groups associated with a specific database engine version. If specified, then EngineName must also be specified.
dogMajorEngineVersion :: Lens' DescribeOptionGroups (Maybe Text)
dogMajorEngineVersion = lens _dogMajorEngineVersion (\ s a -> s{_dogMajorEngineVersion = a})

-- | An optional pagination token provided by a previous DescribeOptionGroups request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dogMarker :: Lens' DescribeOptionGroups (Maybe Text)
dogMarker = lens _dogMarker (\ s a -> s{_dogMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a pagination token called a marker is included in the response so that the remaining results can be retrieved.  Default: 100 Constraints: Minimum 20, maximum 100.
dogMaxRecords :: Lens' DescribeOptionGroups (Maybe Int)
dogMaxRecords = lens _dogMaxRecords (\ s a -> s{_dogMaxRecords = a})

-- | The name of the option group to describe. Cannot be supplied together with EngineName or MajorEngineVersion.
dogOptionGroupName :: Lens' DescribeOptionGroups (Maybe Text)
dogOptionGroupName = lens _dogOptionGroupName (\ s a -> s{_dogOptionGroupName = a})

instance AWSPager DescribeOptionGroups where
        page rq rs
          | stop (rs ^. dogrsMarker) = Nothing
          | stop (rs ^. dogrsOptionGroupsList) = Nothing
          | otherwise =
            Just $ rq & dogMarker .~ rs ^. dogrsMarker

instance AWSRequest DescribeOptionGroups where
        type Rs DescribeOptionGroups =
             DescribeOptionGroupsResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DescribeOptionGroupsResult"
              (\ s h x ->
                 DescribeOptionGroupsResponse' <$>
                   (x .@? "Marker") <*>
                     (x .@? "OptionGroupsList" .!@ mempty >>=
                        may (parseXMLList "OptionGroup"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeOptionGroups where

instance NFData DescribeOptionGroups where

instance ToHeaders DescribeOptionGroups where
        toHeaders = const mempty

instance ToPath DescribeOptionGroups where
        toPath = const "/"

instance ToQuery DescribeOptionGroups where
        toQuery DescribeOptionGroups'{..}
          = mconcat
              ["Action" =: ("DescribeOptionGroups" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _dogFilters),
               "EngineName" =: _dogEngineName,
               "MajorEngineVersion" =: _dogMajorEngineVersion,
               "Marker" =: _dogMarker,
               "MaxRecords" =: _dogMaxRecords,
               "OptionGroupName" =: _dogOptionGroupName]

-- | List of option groups.
--
--
--
-- /See:/ 'describeOptionGroupsResponse' smart constructor.
data DescribeOptionGroupsResponse = DescribeOptionGroupsResponse'
  { _dogrsMarker           :: !(Maybe Text)
  , _dogrsOptionGroupsList :: !(Maybe [OptionGroup])
  , _dogrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeOptionGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dogrsMarker' - An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dogrsOptionGroupsList' - List of option groups.
--
-- * 'dogrsResponseStatus' - -- | The response status code.
describeOptionGroupsResponse
    :: Int -- ^ 'dogrsResponseStatus'
    -> DescribeOptionGroupsResponse
describeOptionGroupsResponse pResponseStatus_ =
  DescribeOptionGroupsResponse'
    { _dogrsMarker = Nothing
    , _dogrsOptionGroupsList = Nothing
    , _dogrsResponseStatus = pResponseStatus_
    }


-- | An optional pagination token provided by a previous request. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dogrsMarker :: Lens' DescribeOptionGroupsResponse (Maybe Text)
dogrsMarker = lens _dogrsMarker (\ s a -> s{_dogrsMarker = a})

-- | List of option groups.
dogrsOptionGroupsList :: Lens' DescribeOptionGroupsResponse [OptionGroup]
dogrsOptionGroupsList = lens _dogrsOptionGroupsList (\ s a -> s{_dogrsOptionGroupsList = a}) . _Default . _Coerce

-- | -- | The response status code.
dogrsResponseStatus :: Lens' DescribeOptionGroupsResponse Int
dogrsResponseStatus = lens _dogrsResponseStatus (\ s a -> s{_dogrsResponseStatus = a})

instance NFData DescribeOptionGroupsResponse where
