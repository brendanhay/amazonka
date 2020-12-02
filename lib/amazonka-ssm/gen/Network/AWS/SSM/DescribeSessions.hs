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
-- Module      : Network.AWS.SSM.DescribeSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of all active sessions (both connected and disconnected) or terminated sessions from the past 30 days.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeSessions
  ( -- * Creating a Request
    describeSessions,
    DescribeSessions,

    -- * Request Lenses
    dsFilters,
    dsNextToken,
    dsMaxResults,
    dsState,

    -- * Destructuring the Response
    describeSessionsResponse,
    DescribeSessionsResponse,

    -- * Response Lenses
    dsrsNextToken,
    dsrsSessions,
    dsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'describeSessions' smart constructor.
data DescribeSessions = DescribeSessions'
  { _dsFilters ::
      !(Maybe (List1 SessionFilter)),
    _dsNextToken :: !(Maybe Text),
    _dsMaxResults :: !(Maybe Nat),
    _dsState :: !SessionState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsFilters' - One or more filters to limit the type of sessions returned by the request.
--
-- * 'dsNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dsMaxResults' - The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- * 'dsState' - The session status to retrieve a list of sessions for. For example, "Active".
describeSessions ::
  -- | 'dsState'
  SessionState ->
  DescribeSessions
describeSessions pState_ =
  DescribeSessions'
    { _dsFilters = Nothing,
      _dsNextToken = Nothing,
      _dsMaxResults = Nothing,
      _dsState = pState_
    }

-- | One or more filters to limit the type of sessions returned by the request.
dsFilters :: Lens' DescribeSessions (Maybe (NonEmpty SessionFilter))
dsFilters = lens _dsFilters (\s a -> s {_dsFilters = a}) . mapping _List1

-- | The token for the next set of items to return. (You received this token from a previous call.)
dsNextToken :: Lens' DescribeSessions (Maybe Text)
dsNextToken = lens _dsNextToken (\s a -> s {_dsNextToken = a})

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
dsMaxResults :: Lens' DescribeSessions (Maybe Natural)
dsMaxResults = lens _dsMaxResults (\s a -> s {_dsMaxResults = a}) . mapping _Nat

-- | The session status to retrieve a list of sessions for. For example, "Active".
dsState :: Lens' DescribeSessions SessionState
dsState = lens _dsState (\s a -> s {_dsState = a})

instance AWSPager DescribeSessions where
  page rq rs
    | stop (rs ^. dsrsNextToken) = Nothing
    | stop (rs ^. dsrsSessions) = Nothing
    | otherwise = Just $ rq & dsNextToken .~ rs ^. dsrsNextToken

instance AWSRequest DescribeSessions where
  type Rs DescribeSessions = DescribeSessionsResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          DescribeSessionsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Sessions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeSessions

instance NFData DescribeSessions

instance ToHeaders DescribeSessions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AmazonSSM.DescribeSessions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeSessions where
  toJSON DescribeSessions' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _dsFilters,
            ("NextToken" .=) <$> _dsNextToken,
            ("MaxResults" .=) <$> _dsMaxResults,
            Just ("State" .= _dsState)
          ]
      )

instance ToPath DescribeSessions where
  toPath = const "/"

instance ToQuery DescribeSessions where
  toQuery = const mempty

-- | /See:/ 'describeSessionsResponse' smart constructor.
data DescribeSessionsResponse = DescribeSessionsResponse'
  { _dsrsNextToken ::
      !(Maybe Text),
    _dsrsSessions :: !(Maybe [Session]),
    _dsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsrsNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dsrsSessions' - A list of sessions meeting the request parameters.
--
-- * 'dsrsResponseStatus' - -- | The response status code.
describeSessionsResponse ::
  -- | 'dsrsResponseStatus'
  Int ->
  DescribeSessionsResponse
describeSessionsResponse pResponseStatus_ =
  DescribeSessionsResponse'
    { _dsrsNextToken = Nothing,
      _dsrsSessions = Nothing,
      _dsrsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of items to return. (You received this token from a previous call.)
dsrsNextToken :: Lens' DescribeSessionsResponse (Maybe Text)
dsrsNextToken = lens _dsrsNextToken (\s a -> s {_dsrsNextToken = a})

-- | A list of sessions meeting the request parameters.
dsrsSessions :: Lens' DescribeSessionsResponse [Session]
dsrsSessions = lens _dsrsSessions (\s a -> s {_dsrsSessions = a}) . _Default . _Coerce

-- | -- | The response status code.
dsrsResponseStatus :: Lens' DescribeSessionsResponse Int
dsrsResponseStatus = lens _dsrsResponseStatus (\s a -> s {_dsrsResponseStatus = a})

instance NFData DescribeSessionsResponse
