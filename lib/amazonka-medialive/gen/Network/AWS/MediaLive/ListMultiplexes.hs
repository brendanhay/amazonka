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
-- Module      : Network.AWS.MediaLive.ListMultiplexes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a list of the existing multiplexes.
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.ListMultiplexes
  ( -- * Creating a Request
    listMultiplexes,
    ListMultiplexes,

    -- * Request Lenses
    lmNextToken,
    lmMaxResults,

    -- * Destructuring the Response
    listMultiplexesResponse,
    ListMultiplexesResponse,

    -- * Response Lenses
    lmrsNextToken,
    lmrsMultiplexes,
    lmrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaLive.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Placeholder documentation for ListMultiplexesRequest
--
-- /See:/ 'listMultiplexes' smart constructor.
data ListMultiplexes = ListMultiplexes'
  { _lmNextToken ::
      !(Maybe Text),
    _lmMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMultiplexes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmNextToken' - The token to retrieve the next page of results.
--
-- * 'lmMaxResults' - The maximum number of items to return.
listMultiplexes ::
  ListMultiplexes
listMultiplexes =
  ListMultiplexes' {_lmNextToken = Nothing, _lmMaxResults = Nothing}

-- | The token to retrieve the next page of results.
lmNextToken :: Lens' ListMultiplexes (Maybe Text)
lmNextToken = lens _lmNextToken (\s a -> s {_lmNextToken = a})

-- | The maximum number of items to return.
lmMaxResults :: Lens' ListMultiplexes (Maybe Natural)
lmMaxResults = lens _lmMaxResults (\s a -> s {_lmMaxResults = a}) . mapping _Nat

instance AWSPager ListMultiplexes where
  page rq rs
    | stop (rs ^. lmrsNextToken) = Nothing
    | stop (rs ^. lmrsMultiplexes) = Nothing
    | otherwise = Just $ rq & lmNextToken .~ rs ^. lmrsNextToken

instance AWSRequest ListMultiplexes where
  type Rs ListMultiplexes = ListMultiplexesResponse
  request = get mediaLive
  response =
    receiveJSON
      ( \s h x ->
          ListMultiplexesResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "multiplexes" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListMultiplexes

instance NFData ListMultiplexes

instance ToHeaders ListMultiplexes where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListMultiplexes where
  toPath = const "/prod/multiplexes"

instance ToQuery ListMultiplexes where
  toQuery ListMultiplexes' {..} =
    mconcat
      ["nextToken" =: _lmNextToken, "maxResults" =: _lmMaxResults]

-- | Placeholder documentation for ListMultiplexesResponse
--
-- /See:/ 'listMultiplexesResponse' smart constructor.
data ListMultiplexesResponse = ListMultiplexesResponse'
  { _lmrsNextToken ::
      !(Maybe Text),
    _lmrsMultiplexes ::
      !(Maybe [MultiplexSummary]),
    _lmrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMultiplexesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmrsNextToken' - Token for the next ListMultiplexes request.
--
-- * 'lmrsMultiplexes' - List of multiplexes.
--
-- * 'lmrsResponseStatus' - -- | The response status code.
listMultiplexesResponse ::
  -- | 'lmrsResponseStatus'
  Int ->
  ListMultiplexesResponse
listMultiplexesResponse pResponseStatus_ =
  ListMultiplexesResponse'
    { _lmrsNextToken = Nothing,
      _lmrsMultiplexes = Nothing,
      _lmrsResponseStatus = pResponseStatus_
    }

-- | Token for the next ListMultiplexes request.
lmrsNextToken :: Lens' ListMultiplexesResponse (Maybe Text)
lmrsNextToken = lens _lmrsNextToken (\s a -> s {_lmrsNextToken = a})

-- | List of multiplexes.
lmrsMultiplexes :: Lens' ListMultiplexesResponse [MultiplexSummary]
lmrsMultiplexes = lens _lmrsMultiplexes (\s a -> s {_lmrsMultiplexes = a}) . _Default . _Coerce

-- | -- | The response status code.
lmrsResponseStatus :: Lens' ListMultiplexesResponse Int
lmrsResponseStatus = lens _lmrsResponseStatus (\s a -> s {_lmrsResponseStatus = a})

instance NFData ListMultiplexesResponse
