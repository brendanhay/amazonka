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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of bundles that are available in Amazon Lightsail. A bundle describes the performance specifications for a database.
--
--
-- You can use a bundle ID to create a new database with explicit performance specifications.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetRelationalDatabaseBundles
  ( -- * Creating a Request
    getRelationalDatabaseBundles,
    GetRelationalDatabaseBundles,

    -- * Request Lenses
    grdbsPageToken,

    -- * Destructuring the Response
    getRelationalDatabaseBundlesResponse,
    GetRelationalDatabaseBundlesResponse,

    -- * Response Lenses
    grdbrsNextPageToken,
    grdbrsBundles,
    grdbrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRelationalDatabaseBundles' smart constructor.
newtype GetRelationalDatabaseBundles = GetRelationalDatabaseBundles'
  { _grdbsPageToken ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRelationalDatabaseBundles' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdbsPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetRelationalDatabaseBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getRelationalDatabaseBundles ::
  GetRelationalDatabaseBundles
getRelationalDatabaseBundles =
  GetRelationalDatabaseBundles' {_grdbsPageToken = Nothing}

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetRelationalDatabaseBundles@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
grdbsPageToken :: Lens' GetRelationalDatabaseBundles (Maybe Text)
grdbsPageToken = lens _grdbsPageToken (\s a -> s {_grdbsPageToken = a})

instance AWSPager GetRelationalDatabaseBundles where
  page rq rs
    | stop (rs ^. grdbrsNextPageToken) = Nothing
    | stop (rs ^. grdbrsBundles) = Nothing
    | otherwise =
      Just $ rq & grdbsPageToken .~ rs ^. grdbrsNextPageToken

instance AWSRequest GetRelationalDatabaseBundles where
  type
    Rs GetRelationalDatabaseBundles =
      GetRelationalDatabaseBundlesResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetRelationalDatabaseBundlesResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "bundles" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetRelationalDatabaseBundles

instance NFData GetRelationalDatabaseBundles

instance ToHeaders GetRelationalDatabaseBundles where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetRelationalDatabaseBundles" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetRelationalDatabaseBundles where
  toJSON GetRelationalDatabaseBundles' {..} =
    object (catMaybes [("pageToken" .=) <$> _grdbsPageToken])

instance ToPath GetRelationalDatabaseBundles where
  toPath = const "/"

instance ToQuery GetRelationalDatabaseBundles where
  toQuery = const mempty

-- | /See:/ 'getRelationalDatabaseBundlesResponse' smart constructor.
data GetRelationalDatabaseBundlesResponse = GetRelationalDatabaseBundlesResponse'
  { _grdbrsNextPageToken ::
      !(Maybe Text),
    _grdbrsBundles ::
      !( Maybe
           [RelationalDatabaseBundle]
       ),
    _grdbrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetRelationalDatabaseBundlesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grdbrsNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetRelationalDatabaseBundles@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'grdbrsBundles' - An object describing the result of your get relational database bundles request.
--
-- * 'grdbrsResponseStatus' - -- | The response status code.
getRelationalDatabaseBundlesResponse ::
  -- | 'grdbrsResponseStatus'
  Int ->
  GetRelationalDatabaseBundlesResponse
getRelationalDatabaseBundlesResponse pResponseStatus_ =
  GetRelationalDatabaseBundlesResponse'
    { _grdbrsNextPageToken =
        Nothing,
      _grdbrsBundles = Nothing,
      _grdbrsResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetRelationalDatabaseBundles@ request and specify the next page token using the @pageToken@ parameter.
grdbrsNextPageToken :: Lens' GetRelationalDatabaseBundlesResponse (Maybe Text)
grdbrsNextPageToken = lens _grdbrsNextPageToken (\s a -> s {_grdbrsNextPageToken = a})

-- | An object describing the result of your get relational database bundles request.
grdbrsBundles :: Lens' GetRelationalDatabaseBundlesResponse [RelationalDatabaseBundle]
grdbrsBundles = lens _grdbrsBundles (\s a -> s {_grdbrsBundles = a}) . _Default . _Coerce

-- | -- | The response status code.
grdbrsResponseStatus :: Lens' GetRelationalDatabaseBundlesResponse Int
grdbrsResponseStatus = lens _grdbrsResponseStatus (\s a -> s {_grdbrsResponseStatus = a})

instance NFData GetRelationalDatabaseBundlesResponse
