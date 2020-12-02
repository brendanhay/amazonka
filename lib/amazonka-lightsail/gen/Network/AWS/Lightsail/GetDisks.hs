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
-- Module      : Network.AWS.Lightsail.GetDisks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disks in your AWS account and region.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDisks
  ( -- * Creating a Request
    getDisks,
    GetDisks,

    -- * Request Lenses
    getPageToken,

    -- * Destructuring the Response
    getDisksResponse,
    GetDisksResponse,

    -- * Response Lenses
    getersNextPageToken,
    getersDisks,
    getersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDisks' smart constructor.
newtype GetDisks = GetDisks' {_getPageToken :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDisks' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDisks@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getDisks ::
  GetDisks
getDisks = GetDisks' {_getPageToken = Nothing}

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDisks@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getPageToken :: Lens' GetDisks (Maybe Text)
getPageToken = lens _getPageToken (\s a -> s {_getPageToken = a})

instance AWSPager GetDisks where
  page rq rs
    | stop (rs ^. getersNextPageToken) = Nothing
    | stop (rs ^. getersDisks) = Nothing
    | otherwise = Just $ rq & getPageToken .~ rs ^. getersNextPageToken

instance AWSRequest GetDisks where
  type Rs GetDisks = GetDisksResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetDisksResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "disks" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetDisks

instance NFData GetDisks

instance ToHeaders GetDisks where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Lightsail_20161128.GetDisks" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDisks where
  toJSON GetDisks' {..} =
    object (catMaybes [("pageToken" .=) <$> _getPageToken])

instance ToPath GetDisks where
  toPath = const "/"

instance ToQuery GetDisks where
  toQuery = const mempty

-- | /See:/ 'getDisksResponse' smart constructor.
data GetDisksResponse = GetDisksResponse'
  { _getersNextPageToken ::
      !(Maybe Text),
    _getersDisks :: !(Maybe [Disk]),
    _getersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDisksResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getersNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'getersDisks' - An array of objects containing information about all block storage disks.
--
-- * 'getersResponseStatus' - -- | The response status code.
getDisksResponse ::
  -- | 'getersResponseStatus'
  Int ->
  GetDisksResponse
getDisksResponse pResponseStatus_ =
  GetDisksResponse'
    { _getersNextPageToken = Nothing,
      _getersDisks = Nothing,
      _getersResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDisks@ request and specify the next page token using the @pageToken@ parameter.
getersNextPageToken :: Lens' GetDisksResponse (Maybe Text)
getersNextPageToken = lens _getersNextPageToken (\s a -> s {_getersNextPageToken = a})

-- | An array of objects containing information about all block storage disks.
getersDisks :: Lens' GetDisksResponse [Disk]
getersDisks = lens _getersDisks (\s a -> s {_getersDisks = a}) . _Default . _Coerce

-- | -- | The response status code.
getersResponseStatus :: Lens' GetDisksResponse Int
getersResponseStatus = lens _getersResponseStatus (\s a -> s {_getersResponseStatus = a})

instance NFData GetDisksResponse
