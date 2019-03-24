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
-- Module      : Network.AWS.CloudWatchLogs.GetQueryResults
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results from the specified query. If the query is in progress, partial results of that current execution are returned. Only the fields requested in the query are returned.
--
--
-- @GetQueryResults@ does not start a query execution. To run a query, use .
--
module Network.AWS.CloudWatchLogs.GetQueryResults
    (
    -- * Creating a Request
      getQueryResults
    , GetQueryResults
    -- * Request Lenses
    , gqrQueryId

    -- * Destructuring the Response
    , getQueryResultsResponse
    , GetQueryResultsResponse
    -- * Response Lenses
    , gqrrsStatus
    , gqrrsResults
    , gqrrsStatistics
    , gqrrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getQueryResults' smart constructor.
newtype GetQueryResults = GetQueryResults'
  { _gqrQueryId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryResults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqrQueryId' - The ID number of the query.
getQueryResults
    :: Text -- ^ 'gqrQueryId'
    -> GetQueryResults
getQueryResults pQueryId_ = GetQueryResults' {_gqrQueryId = pQueryId_}


-- | The ID number of the query.
gqrQueryId :: Lens' GetQueryResults Text
gqrQueryId = lens _gqrQueryId (\ s a -> s{_gqrQueryId = a})

instance AWSRequest GetQueryResults where
        type Rs GetQueryResults = GetQueryResultsResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 GetQueryResultsResponse' <$>
                   (x .?> "status") <*> (x .?> "results" .!@ mempty) <*>
                     (x .?> "statistics")
                     <*> (pure (fromEnum s)))

instance Hashable GetQueryResults where

instance NFData GetQueryResults where

instance ToHeaders GetQueryResults where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.GetQueryResults" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetQueryResults where
        toJSON GetQueryResults'{..}
          = object
              (catMaybes [Just ("queryId" .= _gqrQueryId)])

instance ToPath GetQueryResults where
        toPath = const "/"

instance ToQuery GetQueryResults where
        toQuery = const mempty

-- | /See:/ 'getQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { _gqrrsStatus         :: !(Maybe QueryStatus)
  , _gqrrsResults        :: !(Maybe [[ResultField]])
  , _gqrrsStatistics     :: !(Maybe QueryStatistics)
  , _gqrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryResultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqrrsStatus' - The status of the most recent running of the query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
--
-- * 'gqrrsResults' - The log events that matched the query criteria during the most recent time it ran. The @results@ value is an array of arrays. Each log event is one object in the top-level array. Each of these log event objects is an array of @field@ /@value@ pairs.
--
-- * 'gqrrsStatistics' - Includes the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned.
--
-- * 'gqrrsResponseStatus' - -- | The response status code.
getQueryResultsResponse
    :: Int -- ^ 'gqrrsResponseStatus'
    -> GetQueryResultsResponse
getQueryResultsResponse pResponseStatus_ =
  GetQueryResultsResponse'
    { _gqrrsStatus = Nothing
    , _gqrrsResults = Nothing
    , _gqrrsStatistics = Nothing
    , _gqrrsResponseStatus = pResponseStatus_
    }


-- | The status of the most recent running of the query. Possible values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , @Scheduled@ , and @Unknown@ .
gqrrsStatus :: Lens' GetQueryResultsResponse (Maybe QueryStatus)
gqrrsStatus = lens _gqrrsStatus (\ s a -> s{_gqrrsStatus = a})

-- | The log events that matched the query criteria during the most recent time it ran. The @results@ value is an array of arrays. Each log event is one object in the top-level array. Each of these log event objects is an array of @field@ /@value@ pairs.
gqrrsResults :: Lens' GetQueryResultsResponse [[ResultField]]
gqrrsResults = lens _gqrrsResults (\ s a -> s{_gqrrsResults = a}) . _Default . _Coerce

-- | Includes the number of log events scanned by the query, the number of log events that matched the query criteria, and the total number of bytes in the log events that were scanned.
gqrrsStatistics :: Lens' GetQueryResultsResponse (Maybe QueryStatistics)
gqrrsStatistics = lens _gqrrsStatistics (\ s a -> s{_gqrrsStatistics = a})

-- | -- | The response status code.
gqrrsResponseStatus :: Lens' GetQueryResultsResponse Int
gqrrsResponseStatus = lens _gqrrsResponseStatus (\ s a -> s{_gqrrsResponseStatus = a})

instance NFData GetQueryResultsResponse where
