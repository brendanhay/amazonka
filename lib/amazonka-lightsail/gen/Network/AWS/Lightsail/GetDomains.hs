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
-- Module      : Network.AWS.Lightsail.GetDomains
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all domains in the user's account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDomains
  ( -- * Creating a Request
    getDomains,
    GetDomains,

    -- * Request Lenses
    gPageToken,

    -- * Destructuring the Response
    getDomainsResponse,
    GetDomainsResponse,

    -- * Response Lenses
    getdomainsersNextPageToken,
    getdomainsersDomains,
    getdomainsersResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getDomains' smart constructor.
newtype GetDomains = GetDomains' {_gPageToken :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDomains@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getDomains ::
  GetDomains
getDomains = GetDomains' {_gPageToken = Nothing}

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetDomains@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
gPageToken :: Lens' GetDomains (Maybe Text)
gPageToken = lens _gPageToken (\s a -> s {_gPageToken = a})

instance AWSPager GetDomains where
  page rq rs
    | stop (rs ^. getdomainsersNextPageToken) = Nothing
    | stop (rs ^. getdomainsersDomains) = Nothing
    | otherwise =
      Just $ rq & gPageToken .~ rs ^. getdomainsersNextPageToken

instance AWSRequest GetDomains where
  type Rs GetDomains = GetDomainsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetDomainsResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "domains" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetDomains

instance NFData GetDomains

instance ToHeaders GetDomains where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("Lightsail_20161128.GetDomains" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetDomains where
  toJSON GetDomains' {..} =
    object (catMaybes [("pageToken" .=) <$> _gPageToken])

instance ToPath GetDomains where
  toPath = const "/"

instance ToQuery GetDomains where
  toQuery = const mempty

-- | /See:/ 'getDomainsResponse' smart constructor.
data GetDomainsResponse = GetDomainsResponse'
  { _getdomainsersNextPageToken ::
      !(Maybe Text),
    _getdomainsersDomains :: !(Maybe [Domain]),
    _getdomainsersResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getdomainsersNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDomains@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'getdomainsersDomains' - An array of key-value pairs containing information about each of the domain entries in the user's account.
--
-- * 'getdomainsersResponseStatus' - -- | The response status code.
getDomainsResponse ::
  -- | 'getdomainsersResponseStatus'
  Int ->
  GetDomainsResponse
getDomainsResponse pResponseStatus_ =
  GetDomainsResponse'
    { _getdomainsersNextPageToken = Nothing,
      _getdomainsersDomains = Nothing,
      _getdomainsersResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetDomains@ request and specify the next page token using the @pageToken@ parameter.
getdomainsersNextPageToken :: Lens' GetDomainsResponse (Maybe Text)
getdomainsersNextPageToken = lens _getdomainsersNextPageToken (\s a -> s {_getdomainsersNextPageToken = a})

-- | An array of key-value pairs containing information about each of the domain entries in the user's account.
getdomainsersDomains :: Lens' GetDomainsResponse [Domain]
getdomainsersDomains = lens _getdomainsersDomains (\s a -> s {_getdomainsersDomains = a}) . _Default . _Coerce

-- | -- | The response status code.
getdomainsersResponseStatus :: Lens' GetDomainsResponse Int
getdomainsersResponseStatus = lens _getdomainsersResponseStatus (\s a -> s {_getdomainsersResponseStatus = a})

instance NFData GetDomainsResponse
