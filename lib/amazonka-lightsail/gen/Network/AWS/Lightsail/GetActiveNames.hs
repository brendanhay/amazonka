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
-- Module      : Network.AWS.Lightsail.GetActiveNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the names of all active (not deleted) resources.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetActiveNames
  ( -- * Creating a Request
    getActiveNames,
    GetActiveNames,

    -- * Request Lenses
    ganPageToken,

    -- * Destructuring the Response
    getActiveNamesResponse,
    GetActiveNamesResponse,

    -- * Response Lenses
    ganrsNextPageToken,
    ganrsActiveNames,
    ganrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getActiveNames' smart constructor.
newtype GetActiveNames = GetActiveNames'
  { _ganPageToken ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetActiveNames' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ganPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getActiveNames ::
  GetActiveNames
getActiveNames = GetActiveNames' {_ganPageToken = Nothing}

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetActiveNames@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
ganPageToken :: Lens' GetActiveNames (Maybe Text)
ganPageToken = lens _ganPageToken (\s a -> s {_ganPageToken = a})

instance AWSPager GetActiveNames where
  page rq rs
    | stop (rs ^. ganrsNextPageToken) = Nothing
    | stop (rs ^. ganrsActiveNames) = Nothing
    | otherwise = Just $ rq & ganPageToken .~ rs ^. ganrsNextPageToken

instance AWSRequest GetActiveNames where
  type Rs GetActiveNames = GetActiveNamesResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetActiveNamesResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "activeNames" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetActiveNames

instance NFData GetActiveNames

instance ToHeaders GetActiveNames where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetActiveNames" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetActiveNames where
  toJSON GetActiveNames' {..} =
    object (catMaybes [("pageToken" .=) <$> _ganPageToken])

instance ToPath GetActiveNames where
  toPath = const "/"

instance ToQuery GetActiveNames where
  toQuery = const mempty

-- | /See:/ 'getActiveNamesResponse' smart constructor.
data GetActiveNamesResponse = GetActiveNamesResponse'
  { _ganrsNextPageToken ::
      !(Maybe Text),
    _ganrsActiveNames :: !(Maybe [Text]),
    _ganrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetActiveNamesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ganrsNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'ganrsActiveNames' - The list of active names returned by the get active names request.
--
-- * 'ganrsResponseStatus' - -- | The response status code.
getActiveNamesResponse ::
  -- | 'ganrsResponseStatus'
  Int ->
  GetActiveNamesResponse
getActiveNamesResponse pResponseStatus_ =
  GetActiveNamesResponse'
    { _ganrsNextPageToken = Nothing,
      _ganrsActiveNames = Nothing,
      _ganrsResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetActiveNames@ request and specify the next page token using the @pageToken@ parameter.
ganrsNextPageToken :: Lens' GetActiveNamesResponse (Maybe Text)
ganrsNextPageToken = lens _ganrsNextPageToken (\s a -> s {_ganrsNextPageToken = a})

-- | The list of active names returned by the get active names request.
ganrsActiveNames :: Lens' GetActiveNamesResponse [Text]
ganrsActiveNames = lens _ganrsActiveNames (\s a -> s {_ganrsActiveNames = a}) . _Default . _Coerce

-- | -- | The response status code.
ganrsResponseStatus :: Lens' GetActiveNamesResponse Int
ganrsResponseStatus = lens _ganrsResponseStatus (\s a -> s {_ganrsResponseStatus = a})

instance NFData GetActiveNamesResponse
