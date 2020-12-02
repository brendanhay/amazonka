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
-- Module      : Network.AWS.DeviceFarm.ListTestGridSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of sessions for a 'TestGridProject' .
module Network.AWS.DeviceFarm.ListTestGridSessions
  ( -- * Creating a Request
    listTestGridSessions,
    ListTestGridSessions,

    -- * Request Lenses
    ltgsStatus,
    ltgsMaxResult,
    ltgsCreationTimeAfter,
    ltgsEndTimeBefore,
    ltgsEndTimeAfter,
    ltgsNextToken,
    ltgsCreationTimeBefore,
    ltgsProjectARN,

    -- * Destructuring the Response
    listTestGridSessionsResponse,
    ListTestGridSessionsResponse,

    -- * Response Lenses
    ltgsrsNextToken,
    ltgsrsTestGridSessions,
    ltgsrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTestGridSessions' smart constructor.
data ListTestGridSessions = ListTestGridSessions'
  { _ltgsStatus ::
      !(Maybe TestGridSessionStatus),
    _ltgsMaxResult :: !(Maybe Nat),
    _ltgsCreationTimeAfter :: !(Maybe POSIX),
    _ltgsEndTimeBefore :: !(Maybe POSIX),
    _ltgsEndTimeAfter :: !(Maybe POSIX),
    _ltgsNextToken :: !(Maybe Text),
    _ltgsCreationTimeBefore :: !(Maybe POSIX),
    _ltgsProjectARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTestGridSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgsStatus' - Return only sessions in this state.
--
-- * 'ltgsMaxResult' - Return only this many results at a time.
--
-- * 'ltgsCreationTimeAfter' - Return only sessions created after this time.
--
-- * 'ltgsEndTimeBefore' - Return only sessions that ended before this time.
--
-- * 'ltgsEndTimeAfter' - Return only sessions that ended after this time.
--
-- * 'ltgsNextToken' - Pagination token.
--
-- * 'ltgsCreationTimeBefore' - Return only sessions created before this time.
--
-- * 'ltgsProjectARN' - ARN of a 'TestGridProject' .
listTestGridSessions ::
  -- | 'ltgsProjectARN'
  Text ->
  ListTestGridSessions
listTestGridSessions pProjectARN_ =
  ListTestGridSessions'
    { _ltgsStatus = Nothing,
      _ltgsMaxResult = Nothing,
      _ltgsCreationTimeAfter = Nothing,
      _ltgsEndTimeBefore = Nothing,
      _ltgsEndTimeAfter = Nothing,
      _ltgsNextToken = Nothing,
      _ltgsCreationTimeBefore = Nothing,
      _ltgsProjectARN = pProjectARN_
    }

-- | Return only sessions in this state.
ltgsStatus :: Lens' ListTestGridSessions (Maybe TestGridSessionStatus)
ltgsStatus = lens _ltgsStatus (\s a -> s {_ltgsStatus = a})

-- | Return only this many results at a time.
ltgsMaxResult :: Lens' ListTestGridSessions (Maybe Natural)
ltgsMaxResult = lens _ltgsMaxResult (\s a -> s {_ltgsMaxResult = a}) . mapping _Nat

-- | Return only sessions created after this time.
ltgsCreationTimeAfter :: Lens' ListTestGridSessions (Maybe UTCTime)
ltgsCreationTimeAfter = lens _ltgsCreationTimeAfter (\s a -> s {_ltgsCreationTimeAfter = a}) . mapping _Time

-- | Return only sessions that ended before this time.
ltgsEndTimeBefore :: Lens' ListTestGridSessions (Maybe UTCTime)
ltgsEndTimeBefore = lens _ltgsEndTimeBefore (\s a -> s {_ltgsEndTimeBefore = a}) . mapping _Time

-- | Return only sessions that ended after this time.
ltgsEndTimeAfter :: Lens' ListTestGridSessions (Maybe UTCTime)
ltgsEndTimeAfter = lens _ltgsEndTimeAfter (\s a -> s {_ltgsEndTimeAfter = a}) . mapping _Time

-- | Pagination token.
ltgsNextToken :: Lens' ListTestGridSessions (Maybe Text)
ltgsNextToken = lens _ltgsNextToken (\s a -> s {_ltgsNextToken = a})

-- | Return only sessions created before this time.
ltgsCreationTimeBefore :: Lens' ListTestGridSessions (Maybe UTCTime)
ltgsCreationTimeBefore = lens _ltgsCreationTimeBefore (\s a -> s {_ltgsCreationTimeBefore = a}) . mapping _Time

-- | ARN of a 'TestGridProject' .
ltgsProjectARN :: Lens' ListTestGridSessions Text
ltgsProjectARN = lens _ltgsProjectARN (\s a -> s {_ltgsProjectARN = a})

instance AWSRequest ListTestGridSessions where
  type Rs ListTestGridSessions = ListTestGridSessionsResponse
  request = postJSON deviceFarm
  response =
    receiveJSON
      ( \s h x ->
          ListTestGridSessionsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "testGridSessions" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListTestGridSessions

instance NFData ListTestGridSessions

instance ToHeaders ListTestGridSessions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("DeviceFarm_20150623.ListTestGridSessions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListTestGridSessions where
  toJSON ListTestGridSessions' {..} =
    object
      ( catMaybes
          [ ("status" .=) <$> _ltgsStatus,
            ("maxResult" .=) <$> _ltgsMaxResult,
            ("creationTimeAfter" .=) <$> _ltgsCreationTimeAfter,
            ("endTimeBefore" .=) <$> _ltgsEndTimeBefore,
            ("endTimeAfter" .=) <$> _ltgsEndTimeAfter,
            ("nextToken" .=) <$> _ltgsNextToken,
            ("creationTimeBefore" .=) <$> _ltgsCreationTimeBefore,
            Just ("projectArn" .= _ltgsProjectARN)
          ]
      )

instance ToPath ListTestGridSessions where
  toPath = const "/"

instance ToQuery ListTestGridSessions where
  toQuery = const mempty

-- | /See:/ 'listTestGridSessionsResponse' smart constructor.
data ListTestGridSessionsResponse = ListTestGridSessionsResponse'
  { _ltgsrsNextToken ::
      !(Maybe Text),
    _ltgsrsTestGridSessions ::
      !(Maybe [TestGridSession]),
    _ltgsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListTestGridSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltgsrsNextToken' - Pagination token.
--
-- * 'ltgsrsTestGridSessions' - The sessions that match the criteria in a 'ListTestGridSessionsRequest' .
--
-- * 'ltgsrsResponseStatus' - -- | The response status code.
listTestGridSessionsResponse ::
  -- | 'ltgsrsResponseStatus'
  Int ->
  ListTestGridSessionsResponse
listTestGridSessionsResponse pResponseStatus_ =
  ListTestGridSessionsResponse'
    { _ltgsrsNextToken = Nothing,
      _ltgsrsTestGridSessions = Nothing,
      _ltgsrsResponseStatus = pResponseStatus_
    }

-- | Pagination token.
ltgsrsNextToken :: Lens' ListTestGridSessionsResponse (Maybe Text)
ltgsrsNextToken = lens _ltgsrsNextToken (\s a -> s {_ltgsrsNextToken = a})

-- | The sessions that match the criteria in a 'ListTestGridSessionsRequest' .
ltgsrsTestGridSessions :: Lens' ListTestGridSessionsResponse [TestGridSession]
ltgsrsTestGridSessions = lens _ltgsrsTestGridSessions (\s a -> s {_ltgsrsTestGridSessions = a}) . _Default . _Coerce

-- | -- | The response status code.
ltgsrsResponseStatus :: Lens' ListTestGridSessionsResponse Int
ltgsrsResponseStatus = lens _ltgsrsResponseStatus (\s a -> s {_ltgsrsResponseStatus = a})

instance NFData ListTestGridSessionsResponse
