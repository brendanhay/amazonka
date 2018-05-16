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
-- Module      : Network.AWS.Discovery.DescribeExportTasks
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve status of one or more export tasks. You can retrieve the status of up to 100 export tasks.
--
--
module Network.AWS.Discovery.DescribeExportTasks
    (
    -- * Creating a Request
      describeExportTasks
    , DescribeExportTasks
    -- * Request Lenses
    , detFilters
    , detNextToken
    , detExportIds
    , detMaxResults

    -- * Destructuring the Response
    , describeExportTasksResponse
    , DescribeExportTasksResponse
    -- * Response Lenses
    , detrsNextToken
    , detrsExportsInfo
    , detrsResponseStatus
    ) where

import Network.AWS.Discovery.Types
import Network.AWS.Discovery.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { _detFilters    :: !(Maybe [ExportFilter])
  , _detNextToken  :: !(Maybe Text)
  , _detExportIds  :: !(Maybe [Text])
  , _detMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detFilters' - One or more filters.     * @AgentId@ - ID of the agent whose collected data will be exported
--
-- * 'detNextToken' - The @nextToken@ value returned from a previous paginated @DescribeExportTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
--
-- * 'detExportIds' - One or more unique identifiers used to query the status of an export request.
--
-- * 'detMaxResults' - The maximum number of volume results returned by @DescribeExportTasks@ in paginated output. When this parameter is used, @DescribeExportTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element.
describeExportTasks
    :: DescribeExportTasks
describeExportTasks =
  DescribeExportTasks'
    { _detFilters = Nothing
    , _detNextToken = Nothing
    , _detExportIds = Nothing
    , _detMaxResults = Nothing
    }


-- | One or more filters.     * @AgentId@ - ID of the agent whose collected data will be exported
detFilters :: Lens' DescribeExportTasks [ExportFilter]
detFilters = lens _detFilters (\ s a -> s{_detFilters = a}) . _Default . _Coerce

-- | The @nextToken@ value returned from a previous paginated @DescribeExportTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
detNextToken :: Lens' DescribeExportTasks (Maybe Text)
detNextToken = lens _detNextToken (\ s a -> s{_detNextToken = a})

-- | One or more unique identifiers used to query the status of an export request.
detExportIds :: Lens' DescribeExportTasks [Text]
detExportIds = lens _detExportIds (\ s a -> s{_detExportIds = a}) . _Default . _Coerce

-- | The maximum number of volume results returned by @DescribeExportTasks@ in paginated output. When this parameter is used, @DescribeExportTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element.
detMaxResults :: Lens' DescribeExportTasks (Maybe Int)
detMaxResults = lens _detMaxResults (\ s a -> s{_detMaxResults = a})

instance AWSRequest DescribeExportTasks where
        type Rs DescribeExportTasks =
             DescribeExportTasksResponse
        request = postJSON discovery
        response
          = receiveJSON
              (\ s h x ->
                 DescribeExportTasksResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "exportsInfo" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeExportTasks where

instance NFData DescribeExportTasks where

instance ToHeaders DescribeExportTasks where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSPoseidonService_V2015_11_01.DescribeExportTasks"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeExportTasks where
        toJSON DescribeExportTasks'{..}
          = object
              (catMaybes
                 [("filters" .=) <$> _detFilters,
                  ("nextToken" .=) <$> _detNextToken,
                  ("exportIds" .=) <$> _detExportIds,
                  ("maxResults" .=) <$> _detMaxResults])

instance ToPath DescribeExportTasks where
        toPath = const "/"

instance ToQuery DescribeExportTasks where
        toQuery = const mempty

-- | /See:/ 'describeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { _detrsNextToken      :: !(Maybe Text)
  , _detrsExportsInfo    :: !(Maybe [ExportInfo])
  , _detrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'detrsNextToken' - The @nextToken@ value to include in a future @DescribeExportTasks@ request. When the results of a @DescribeExportTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
--
-- * 'detrsExportsInfo' - Contains one or more sets of export request details. When the status of a request is @SUCCEEDED@ , the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
--
-- * 'detrsResponseStatus' - -- | The response status code.
describeExportTasksResponse
    :: Int -- ^ 'detrsResponseStatus'
    -> DescribeExportTasksResponse
describeExportTasksResponse pResponseStatus_ =
  DescribeExportTasksResponse'
    { _detrsNextToken = Nothing
    , _detrsExportsInfo = Nothing
    , _detrsResponseStatus = pResponseStatus_
    }


-- | The @nextToken@ value to include in a future @DescribeExportTasks@ request. When the results of a @DescribeExportTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
detrsNextToken :: Lens' DescribeExportTasksResponse (Maybe Text)
detrsNextToken = lens _detrsNextToken (\ s a -> s{_detrsNextToken = a})

-- | Contains one or more sets of export request details. When the status of a request is @SUCCEEDED@ , the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
detrsExportsInfo :: Lens' DescribeExportTasksResponse [ExportInfo]
detrsExportsInfo = lens _detrsExportsInfo (\ s a -> s{_detrsExportsInfo = a}) . _Default . _Coerce

-- | -- | The response status code.
detrsResponseStatus :: Lens' DescribeExportTasksResponse Int
detrsResponseStatus = lens _detrsResponseStatus (\ s a -> s{_detrsResponseStatus = a})

instance NFData DescribeExportTasksResponse where
