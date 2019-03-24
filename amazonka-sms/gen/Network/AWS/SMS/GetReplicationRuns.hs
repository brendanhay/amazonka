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
-- Module      : Network.AWS.SMS.GetReplicationRuns
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the replication runs for the specified replication job.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetReplicationRuns
    (
    -- * Creating a Request
      getReplicationRuns
    , GetReplicationRuns
    -- * Request Lenses
    , grrNextToken
    , grrMaxResults
    , grrReplicationJobId

    -- * Destructuring the Response
    , getReplicationRunsResponse
    , GetReplicationRunsResponse
    -- * Response Lenses
    , grrrsReplicationJob
    , grrrsNextToken
    , grrrsReplicationRunList
    , grrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types
import Network.AWS.SMS.Types.Product

-- | /See:/ 'getReplicationRuns' smart constructor.
data GetReplicationRuns = GetReplicationRuns'
  { _grrNextToken        :: !(Maybe Text)
  , _grrMaxResults       :: !(Maybe Int)
  , _grrReplicationJobId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReplicationRuns' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrNextToken' - The token for the next set of results.
--
-- * 'grrMaxResults' - The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- * 'grrReplicationJobId' - The identifier of the replication job.
getReplicationRuns
    :: Text -- ^ 'grrReplicationJobId'
    -> GetReplicationRuns
getReplicationRuns pReplicationJobId_ =
  GetReplicationRuns'
    { _grrNextToken = Nothing
    , _grrMaxResults = Nothing
    , _grrReplicationJobId = pReplicationJobId_
    }


-- | The token for the next set of results.
grrNextToken :: Lens' GetReplicationRuns (Maybe Text)
grrNextToken = lens _grrNextToken (\ s a -> s{_grrNextToken = a})

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
grrMaxResults :: Lens' GetReplicationRuns (Maybe Int)
grrMaxResults = lens _grrMaxResults (\ s a -> s{_grrMaxResults = a})

-- | The identifier of the replication job.
grrReplicationJobId :: Lens' GetReplicationRuns Text
grrReplicationJobId = lens _grrReplicationJobId (\ s a -> s{_grrReplicationJobId = a})

instance AWSPager GetReplicationRuns where
        page rq rs
          | stop (rs ^. grrrsNextToken) = Nothing
          | stop (rs ^. grrrsReplicationRunList) = Nothing
          | otherwise =
            Just $ rq & grrNextToken .~ rs ^. grrrsNextToken

instance AWSRequest GetReplicationRuns where
        type Rs GetReplicationRuns =
             GetReplicationRunsResponse
        request = postJSON sms
        response
          = receiveJSON
              (\ s h x ->
                 GetReplicationRunsResponse' <$>
                   (x .?> "replicationJob") <*> (x .?> "nextToken") <*>
                     (x .?> "replicationRunList" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetReplicationRuns where

instance NFData GetReplicationRuns where

instance ToHeaders GetReplicationRuns where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSServerMigrationService_V2016_10_24.GetReplicationRuns"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetReplicationRuns where
        toJSON GetReplicationRuns'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _grrNextToken,
                  ("maxResults" .=) <$> _grrMaxResults,
                  Just ("replicationJobId" .= _grrReplicationJobId)])

instance ToPath GetReplicationRuns where
        toPath = const "/"

instance ToQuery GetReplicationRuns where
        toQuery = const mempty

-- | /See:/ 'getReplicationRunsResponse' smart constructor.
data GetReplicationRunsResponse = GetReplicationRunsResponse'
  { _grrrsReplicationJob     :: !(Maybe ReplicationJob)
  , _grrrsNextToken          :: !(Maybe Text)
  , _grrrsReplicationRunList :: !(Maybe [ReplicationRun])
  , _grrrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetReplicationRunsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrrsReplicationJob' - Information about the replication job.
--
-- * 'grrrsNextToken' - The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- * 'grrrsReplicationRunList' - Information about the replication runs.
--
-- * 'grrrsResponseStatus' - -- | The response status code.
getReplicationRunsResponse
    :: Int -- ^ 'grrrsResponseStatus'
    -> GetReplicationRunsResponse
getReplicationRunsResponse pResponseStatus_ =
  GetReplicationRunsResponse'
    { _grrrsReplicationJob = Nothing
    , _grrrsNextToken = Nothing
    , _grrrsReplicationRunList = Nothing
    , _grrrsResponseStatus = pResponseStatus_
    }


-- | Information about the replication job.
grrrsReplicationJob :: Lens' GetReplicationRunsResponse (Maybe ReplicationJob)
grrrsReplicationJob = lens _grrrsReplicationJob (\ s a -> s{_grrrsReplicationJob = a})

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
grrrsNextToken :: Lens' GetReplicationRunsResponse (Maybe Text)
grrrsNextToken = lens _grrrsNextToken (\ s a -> s{_grrrsNextToken = a})

-- | Information about the replication runs.
grrrsReplicationRunList :: Lens' GetReplicationRunsResponse [ReplicationRun]
grrrsReplicationRunList = lens _grrrsReplicationRunList (\ s a -> s{_grrrsReplicationRunList = a}) . _Default . _Coerce

-- | -- | The response status code.
grrrsResponseStatus :: Lens' GetReplicationRunsResponse Int
grrrsResponseStatus = lens _grrrsResponseStatus (\ s a -> s{_grrrsResponseStatus = a})

instance NFData GetReplicationRunsResponse where
