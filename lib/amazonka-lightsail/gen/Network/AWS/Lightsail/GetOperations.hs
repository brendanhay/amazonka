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
-- Module      : Network.AWS.Lightsail.GetOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all operations.
--
--
-- Results are returned from oldest to newest, up to a maximum of 200. Results can be paged by making each subsequent call to @GetOperations@ use the maximum (last) @statusChangedAt@ value from the previous request.
--
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetOperations
  ( -- * Creating a Request
    getOperations,
    GetOperations,

    -- * Request Lenses
    goPageToken,

    -- * Destructuring the Response
    getOperationsResponse,
    GetOperationsResponse,

    -- * Response Lenses
    gosrsNextPageToken,
    gosrsOperations,
    gosrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getOperations' smart constructor.
newtype GetOperations = GetOperations' {_goPageToken :: Maybe Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOperations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goPageToken' - The token to advance to the next page of results from your request. To get a page token, perform an initial @GetOperations@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
getOperations ::
  GetOperations
getOperations = GetOperations' {_goPageToken = Nothing}

-- | The token to advance to the next page of results from your request. To get a page token, perform an initial @GetOperations@ request. If your results are paginated, the response will return a next page token that you can specify as the page token in a subsequent request.
goPageToken :: Lens' GetOperations (Maybe Text)
goPageToken = lens _goPageToken (\s a -> s {_goPageToken = a})

instance AWSPager GetOperations where
  page rq rs
    | stop (rs ^. gosrsNextPageToken) = Nothing
    | stop (rs ^. gosrsOperations) = Nothing
    | otherwise = Just $ rq & goPageToken .~ rs ^. gosrsNextPageToken

instance AWSRequest GetOperations where
  type Rs GetOperations = GetOperationsResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetOperationsResponse'
            <$> (x .?> "nextPageToken")
            <*> (x .?> "operations" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetOperations

instance NFData GetOperations

instance ToHeaders GetOperations where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetOperations" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetOperations where
  toJSON GetOperations' {..} =
    object (catMaybes [("pageToken" .=) <$> _goPageToken])

instance ToPath GetOperations where
  toPath = const "/"

instance ToQuery GetOperations where
  toQuery = const mempty

-- | /See:/ 'getOperationsResponse' smart constructor.
data GetOperationsResponse = GetOperationsResponse'
  { _gosrsNextPageToken ::
      !(Maybe Text),
    _gosrsOperations :: !(Maybe [Operation]),
    _gosrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetOperationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gosrsNextPageToken' - The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetOperations@ request and specify the next page token using the @pageToken@ parameter.
--
-- * 'gosrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'gosrsResponseStatus' - -- | The response status code.
getOperationsResponse ::
  -- | 'gosrsResponseStatus'
  Int ->
  GetOperationsResponse
getOperationsResponse pResponseStatus_ =
  GetOperationsResponse'
    { _gosrsNextPageToken = Nothing,
      _gosrsOperations = Nothing,
      _gosrsResponseStatus = pResponseStatus_
    }

-- | The token to advance to the next page of results from your request. A next page token is not returned if there are no more results to display. To get the next page of results, perform another @GetOperations@ request and specify the next page token using the @pageToken@ parameter.
gosrsNextPageToken :: Lens' GetOperationsResponse (Maybe Text)
gosrsNextPageToken = lens _gosrsNextPageToken (\s a -> s {_gosrsNextPageToken = a})

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
gosrsOperations :: Lens' GetOperationsResponse [Operation]
gosrsOperations = lens _gosrsOperations (\s a -> s {_gosrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
gosrsResponseStatus :: Lens' GetOperationsResponse Int
gosrsResponseStatus = lens _gosrsResponseStatus (\s a -> s {_gosrsResponseStatus = a})

instance NFData GetOperationsResponse
