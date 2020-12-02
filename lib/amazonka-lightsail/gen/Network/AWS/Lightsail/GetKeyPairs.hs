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
-- Module      : Network.AWS.Lightsail.GetKeyPairs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all key pairs in the user's account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetKeyPairs
  ( -- * Creating a Request
    getKeyPairs,
    GetKeyPairs,

    -- * Request Lenses
    gkpPageToken,

    -- * Destructuring the Response
    getKeyPairsResponse,
    GetKeyPairsResponse,

    -- * Response Lenses
    gkpsrsNextPageToken,
    gkpsrsKeyPairs,
    gkpsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getKeyPairs' smart constructor.
newtype GetKeyPairs = GetKeyPairs' {_gkpPageToken :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyPairs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkpPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetKeyPairs@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getKeyPairs ::
  GetKeyPairs
getKeyPairs = GetKeyPairs' {_gkpPageToken = Nothing}

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetKeyPairs@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
gkpPageToken :: Lens' GetKeyPairs (Maybe Text)
gkpPageToken = lens _gkpPageToken (\s a -> s {_gkpPageToken = a})

instance AWSPager GetKeyPairs where
  page rq rs
    | stop (rs ^. gkpsrsNextPageToken) = Nothing
    | stop (rs ^. gkpsrsKeyPairs) = Nothing
    | otherwise = Just $ rq & gkpPageToken .~ rs ^. gkpsrsNextPageToken

instance AWSRequest GetKeyPairs where
  type Rs GetKeyPairs = GetKeyPairsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetKeyPairsResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "keyPairs" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetKeyPairs

instance NFData GetKeyPairs

instance ToHeaders GetKeyPairs where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetKeyPairs" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetKeyPairs where
  toJSON GetKeyPairs' {..} =
    object (catMaybes [("pageToken" .=) <$> _gkpPageToken])

instance ToPath GetKeyPairs where
  toPath = const "/"

instance ToQuery GetKeyPairs where
  toQuery = const mempty

-- | /See:/ 'getKeyPairsResponse' smart constructor.
data GetKeyPairsResponse = GetKeyPairsResponse'
  { _gkpsrsNextPageToken ::
      !(Maybe Text),
    _gkpsrsKeyPairs :: !(Maybe [KeyPair]),
    _gkpsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetKeyPairsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gkpsrsNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetKeyPairs@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'gkpsrsKeyPairs' - An array of key-value pairs containing information about the key pairs.
--
-- * 'gkpsrsResponseStatus' - -- | The response status code.
getKeyPairsResponse ::
  -- | 'gkpsrsResponseStatus'
  Int ->
  GetKeyPairsResponse
getKeyPairsResponse pResponseStatus_ =
  GetKeyPairsResponse'
    { _gkpsrsNextPageToken = Nothing,
      _gkpsrsKeyPairs = Nothing,
      _gkpsrsResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetKeyPairs@ request and specify the next page token using the @pageToken@ parameter.
gkpsrsNextPageToken :: Lens' GetKeyPairsResponse (Maybe Text)
gkpsrsNextPageToken = lens _gkpsrsNextPageToken (\s a -> s {_gkpsrsNextPageToken = a})

-- | An array of key-value pairs containing information about the key pairs.
gkpsrsKeyPairs :: Lens' GetKeyPairsResponse [KeyPair]
gkpsrsKeyPairs = lens _gkpsrsKeyPairs (\s a -> s {_gkpsrsKeyPairs = a}) . _Default . _Coerce

-- | -- | The response status code.
gkpsrsResponseStatus :: Lens' GetKeyPairsResponse Int
gkpsrsResponseStatus = lens _gkpsrsResponseStatus (\s a -> s {_gkpsrsResponseStatus = a})

instance NFData GetKeyPairsResponse
