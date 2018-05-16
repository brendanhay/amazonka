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
-- Module      : Network.AWS.Athena.GetQueryResults
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results of a single query execution specified by @QueryExecutionId@ . This request does not execute the query but returns results. Use 'StartQueryExecution' to run a query.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Athena.GetQueryResults
    (
    -- * Creating a Request
      getQueryResults
    , GetQueryResults
    -- * Request Lenses
    , gqrNextToken
    , gqrMaxResults
    , gqrQueryExecutionId

    -- * Destructuring the Response
    , getQueryResultsResponse
    , GetQueryResultsResponse
    -- * Response Lenses
    , gqrrsNextToken
    , gqrrsResultSet
    , gqrrsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getQueryResults' smart constructor.
data GetQueryResults = GetQueryResults'
  { _gqrNextToken        :: !(Maybe Text)
  , _gqrMaxResults       :: !(Maybe Nat)
  , _gqrQueryExecutionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryResults' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqrNextToken' - The token that specifies where to start pagination if a previous request was truncated.
--
-- * 'gqrMaxResults' - The maximum number of results (rows) to return in this request.
--
-- * 'gqrQueryExecutionId' - The unique ID of the query execution.
getQueryResults
    :: Text -- ^ 'gqrQueryExecutionId'
    -> GetQueryResults
getQueryResults pQueryExecutionId_ =
  GetQueryResults'
    { _gqrNextToken = Nothing
    , _gqrMaxResults = Nothing
    , _gqrQueryExecutionId = pQueryExecutionId_
    }


-- | The token that specifies where to start pagination if a previous request was truncated.
gqrNextToken :: Lens' GetQueryResults (Maybe Text)
gqrNextToken = lens _gqrNextToken (\ s a -> s{_gqrNextToken = a})

-- | The maximum number of results (rows) to return in this request.
gqrMaxResults :: Lens' GetQueryResults (Maybe Natural)
gqrMaxResults = lens _gqrMaxResults (\ s a -> s{_gqrMaxResults = a}) . mapping _Nat

-- | The unique ID of the query execution.
gqrQueryExecutionId :: Lens' GetQueryResults Text
gqrQueryExecutionId = lens _gqrQueryExecutionId (\ s a -> s{_gqrQueryExecutionId = a})

instance AWSPager GetQueryResults where
        page rq rs
          | stop (rs ^. gqrrsNextToken) = Nothing
          | stop (rs ^? gqrrsResultSet . _Just . rsRows) =
            Nothing
          | otherwise =
            Just $ rq & gqrNextToken .~ rs ^. gqrrsNextToken

instance AWSRequest GetQueryResults where
        type Rs GetQueryResults = GetQueryResultsResponse
        request = postJSON athena
        response
          = receiveJSON
              (\ s h x ->
                 GetQueryResultsResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "ResultSet") <*>
                     (pure (fromEnum s)))

instance Hashable GetQueryResults where

instance NFData GetQueryResults where

instance ToHeaders GetQueryResults where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.GetQueryResults" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetQueryResults where
        toJSON GetQueryResults'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _gqrNextToken,
                  ("MaxResults" .=) <$> _gqrMaxResults,
                  Just ("QueryExecutionId" .= _gqrQueryExecutionId)])

instance ToPath GetQueryResults where
        toPath = const "/"

instance ToQuery GetQueryResults where
        toQuery = const mempty

-- | /See:/ 'getQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { _gqrrsNextToken      :: !(Maybe Text)
  , _gqrrsResultSet      :: !(Maybe ResultSet)
  , _gqrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetQueryResultsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gqrrsNextToken' - A token to be used by the next request if this request is truncated.
--
-- * 'gqrrsResultSet' - The results of the query execution.
--
-- * 'gqrrsResponseStatus' - -- | The response status code.
getQueryResultsResponse
    :: Int -- ^ 'gqrrsResponseStatus'
    -> GetQueryResultsResponse
getQueryResultsResponse pResponseStatus_ =
  GetQueryResultsResponse'
    { _gqrrsNextToken = Nothing
    , _gqrrsResultSet = Nothing
    , _gqrrsResponseStatus = pResponseStatus_
    }


-- | A token to be used by the next request if this request is truncated.
gqrrsNextToken :: Lens' GetQueryResultsResponse (Maybe Text)
gqrrsNextToken = lens _gqrrsNextToken (\ s a -> s{_gqrrsNextToken = a})

-- | The results of the query execution.
gqrrsResultSet :: Lens' GetQueryResultsResponse (Maybe ResultSet)
gqrrsResultSet = lens _gqrrsResultSet (\ s a -> s{_gqrrsResultSet = a})

-- | -- | The response status code.
gqrrsResponseStatus :: Lens' GetQueryResultsResponse Int
gqrrsResponseStatus = lens _gqrrsResponseStatus (\ s a -> s{_gqrrsResponseStatus = a})

instance NFData GetQueryResultsResponse where
