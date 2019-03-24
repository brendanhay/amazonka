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
-- Module      : Network.AWS.CloudWatchLogs.DescribeQueries
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of CloudWatch Logs Insights queries that are scheduled, executing, or have been executed recently in this account. You can request all queries, or limit it to queries of a specific log group or queries with a certain status.
--
--
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeQueries
    (
    -- * Creating a Request
      describeQueries
    , DescribeQueries
    -- * Request Lenses
    , dqStatus
    , dqLogGroupName
    , dqNextToken
    , dqMaxResults

    -- * Destructuring the Response
    , describeQueriesResponse
    , DescribeQueriesResponse
    -- * Response Lenses
    , dqrsQueries
    , dqrsNextToken
    , dqrsResponseStatus
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeQueries' smart constructor.
data DescribeQueries = DescribeQueries'
  { _dqStatus       :: !(Maybe QueryStatus)
  , _dqLogGroupName :: !(Maybe Text)
  , _dqNextToken    :: !(Maybe Text)
  , _dqMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeQueries' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqStatus' - Limits the returned queries to only those that have the specified status. Valid values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , and @Scheduled@ .
--
-- * 'dqLogGroupName' - Limits the returned queries to only those for the specified log group.
--
-- * 'dqNextToken' - Undocumented member.
--
-- * 'dqMaxResults' - Limits the number of returned queries to the specified number.
describeQueries
    :: DescribeQueries
describeQueries =
  DescribeQueries'
    { _dqStatus = Nothing
    , _dqLogGroupName = Nothing
    , _dqNextToken = Nothing
    , _dqMaxResults = Nothing
    }


-- | Limits the returned queries to only those that have the specified status. Valid values are @Cancelled@ , @Complete@ , @Failed@ , @Running@ , and @Scheduled@ .
dqStatus :: Lens' DescribeQueries (Maybe QueryStatus)
dqStatus = lens _dqStatus (\ s a -> s{_dqStatus = a})

-- | Limits the returned queries to only those for the specified log group.
dqLogGroupName :: Lens' DescribeQueries (Maybe Text)
dqLogGroupName = lens _dqLogGroupName (\ s a -> s{_dqLogGroupName = a})

-- | Undocumented member.
dqNextToken :: Lens' DescribeQueries (Maybe Text)
dqNextToken = lens _dqNextToken (\ s a -> s{_dqNextToken = a})

-- | Limits the number of returned queries to the specified number.
dqMaxResults :: Lens' DescribeQueries (Maybe Natural)
dqMaxResults = lens _dqMaxResults (\ s a -> s{_dqMaxResults = a}) . mapping _Nat

instance AWSPager DescribeQueries where
        page rq rs
          | stop (rs ^. dqrsNextToken) = Nothing
          | stop (rs ^. dqrsQueries) = Nothing
          | otherwise =
            Just $ rq & dqNextToken .~ rs ^. dqrsNextToken

instance AWSRequest DescribeQueries where
        type Rs DescribeQueries = DescribeQueriesResponse
        request = postJSON cloudWatchLogs
        response
          = receiveJSON
              (\ s h x ->
                 DescribeQueriesResponse' <$>
                   (x .?> "queries" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeQueries where

instance NFData DescribeQueries where

instance ToHeaders DescribeQueries where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DescribeQueries" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeQueries where
        toJSON DescribeQueries'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _dqStatus,
                  ("logGroupName" .=) <$> _dqLogGroupName,
                  ("nextToken" .=) <$> _dqNextToken,
                  ("maxResults" .=) <$> _dqMaxResults])

instance ToPath DescribeQueries where
        toPath = const "/"

instance ToQuery DescribeQueries where
        toQuery = const mempty

-- | /See:/ 'describeQueriesResponse' smart constructor.
data DescribeQueriesResponse = DescribeQueriesResponse'
  { _dqrsQueries        :: !(Maybe [QueryInfo])
  , _dqrsNextToken      :: !(Maybe Text)
  , _dqrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeQueriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dqrsQueries' - The list of queries that match the request.
--
-- * 'dqrsNextToken' - Undocumented member.
--
-- * 'dqrsResponseStatus' - -- | The response status code.
describeQueriesResponse
    :: Int -- ^ 'dqrsResponseStatus'
    -> DescribeQueriesResponse
describeQueriesResponse pResponseStatus_ =
  DescribeQueriesResponse'
    { _dqrsQueries = Nothing
    , _dqrsNextToken = Nothing
    , _dqrsResponseStatus = pResponseStatus_
    }


-- | The list of queries that match the request.
dqrsQueries :: Lens' DescribeQueriesResponse [QueryInfo]
dqrsQueries = lens _dqrsQueries (\ s a -> s{_dqrsQueries = a}) . _Default . _Coerce

-- | Undocumented member.
dqrsNextToken :: Lens' DescribeQueriesResponse (Maybe Text)
dqrsNextToken = lens _dqrsNextToken (\ s a -> s{_dqrsNextToken = a})

-- | -- | The response status code.
dqrsResponseStatus :: Lens' DescribeQueriesResponse Int
dqrsResponseStatus = lens _dqrsResponseStatus (\ s a -> s{_dqrsResponseStatus = a})

instance NFData DescribeQueriesResponse where
