{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetReplicationJobs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified replication job or all of your replication jobs.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetReplicationJobs
  ( -- * Creating a Request
    getReplicationJobs,
    GetReplicationJobs,

    -- * Request Lenses
    grjReplicationJobId,
    grjNextToken,
    grjMaxResults,

    -- * Destructuring the Response
    getReplicationJobsResponse,
    GetReplicationJobsResponse,

    -- * Response Lenses
    grjrsReplicationJobList,
    grjrsNextToken,
    grjrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SMS.Types

-- | /See:/ 'getReplicationJobs' smart constructor.
data GetReplicationJobs = GetReplicationJobs'
  { _grjReplicationJobId ::
      !(Maybe Text),
    _grjNextToken :: !(Maybe Text),
    _grjMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetReplicationJobs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grjReplicationJobId' - The ID of the replication job.
--
-- * 'grjNextToken' - The token for the next set of results.
--
-- * 'grjMaxResults' - The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
getReplicationJobs ::
  GetReplicationJobs
getReplicationJobs =
  GetReplicationJobs'
    { _grjReplicationJobId = Nothing,
      _grjNextToken = Nothing,
      _grjMaxResults = Nothing
    }

-- | The ID of the replication job.
grjReplicationJobId :: Lens' GetReplicationJobs (Maybe Text)
grjReplicationJobId = lens _grjReplicationJobId (\s a -> s {_grjReplicationJobId = a})

-- | The token for the next set of results.
grjNextToken :: Lens' GetReplicationJobs (Maybe Text)
grjNextToken = lens _grjNextToken (\s a -> s {_grjNextToken = a})

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
grjMaxResults :: Lens' GetReplicationJobs (Maybe Int)
grjMaxResults = lens _grjMaxResults (\s a -> s {_grjMaxResults = a})

instance AWSPager GetReplicationJobs where
  page rq rs
    | stop (rs ^. grjrsNextToken) = Nothing
    | stop (rs ^. grjrsReplicationJobList) = Nothing
    | otherwise = Just $ rq & grjNextToken .~ rs ^. grjrsNextToken

instance AWSRequest GetReplicationJobs where
  type Rs GetReplicationJobs = GetReplicationJobsResponse
  request = postJSON sms
  response =
    receiveJSON
      ( \s h x ->
          GetReplicationJobsResponse'
            <$> (x .?> "replicationJobList" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetReplicationJobs

instance NFData GetReplicationJobs

instance ToHeaders GetReplicationJobs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSServerMigrationService_V2016_10_24.GetReplicationJobs" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetReplicationJobs where
  toJSON GetReplicationJobs' {..} =
    object
      ( catMaybes
          [ ("replicationJobId" .=) <$> _grjReplicationJobId,
            ("nextToken" .=) <$> _grjNextToken,
            ("maxResults" .=) <$> _grjMaxResults
          ]
      )

instance ToPath GetReplicationJobs where
  toPath = const "/"

instance ToQuery GetReplicationJobs where
  toQuery = const mempty

-- | /See:/ 'getReplicationJobsResponse' smart constructor.
data GetReplicationJobsResponse = GetReplicationJobsResponse'
  { _grjrsReplicationJobList ::
      !(Maybe [ReplicationJob]),
    _grjrsNextToken :: !(Maybe Text),
    _grjrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetReplicationJobsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grjrsReplicationJobList' - Information about the replication jobs.
--
-- * 'grjrsNextToken' - The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- * 'grjrsResponseStatus' - -- | The response status code.
getReplicationJobsResponse ::
  -- | 'grjrsResponseStatus'
  Int ->
  GetReplicationJobsResponse
getReplicationJobsResponse pResponseStatus_ =
  GetReplicationJobsResponse'
    { _grjrsReplicationJobList = Nothing,
      _grjrsNextToken = Nothing,
      _grjrsResponseStatus = pResponseStatus_
    }

-- | Information about the replication jobs.
grjrsReplicationJobList :: Lens' GetReplicationJobsResponse [ReplicationJob]
grjrsReplicationJobList = lens _grjrsReplicationJobList (\s a -> s {_grjrsReplicationJobList = a}) . _Default . _Coerce

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
grjrsNextToken :: Lens' GetReplicationJobsResponse (Maybe Text)
grjrsNextToken = lens _grjrsNextToken (\s a -> s {_grjrsNextToken = a})

-- | -- | The response status code.
grjrsResponseStatus :: Lens' GetReplicationJobsResponse Int
grjrsResponseStatus = lens _grjrsResponseStatus (\s a -> s {_grjrsResponseStatus = a})

instance NFData GetReplicationJobsResponse
