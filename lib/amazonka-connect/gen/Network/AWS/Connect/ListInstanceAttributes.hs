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
-- Module      : Network.AWS.Connect.ListInstanceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute types for the given instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstanceAttributes
  ( -- * Creating a Request
    listInstanceAttributes,
    ListInstanceAttributes,

    -- * Request Lenses
    liaNextToken,
    liaMaxResults,
    liaInstanceId,

    -- * Destructuring the Response
    listInstanceAttributesResponse,
    ListInstanceAttributesResponse,

    -- * Response Lenses
    liarsNextToken,
    liarsAttributes,
    liarsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listInstanceAttributes' smart constructor.
data ListInstanceAttributes = ListInstanceAttributes'
  { _liaNextToken ::
      !(Maybe Text),
    _liaMaxResults :: !(Maybe Nat),
    _liaInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListInstanceAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liaNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'liaMaxResults' - The maximimum number of results to return per page.
--
-- * 'liaInstanceId' - The identifier of the Amazon Connect instance.
listInstanceAttributes ::
  -- | 'liaInstanceId'
  Text ->
  ListInstanceAttributes
listInstanceAttributes pInstanceId_ =
  ListInstanceAttributes'
    { _liaNextToken = Nothing,
      _liaMaxResults = Nothing,
      _liaInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
liaNextToken :: Lens' ListInstanceAttributes (Maybe Text)
liaNextToken = lens _liaNextToken (\s a -> s {_liaNextToken = a})

-- | The maximimum number of results to return per page.
liaMaxResults :: Lens' ListInstanceAttributes (Maybe Natural)
liaMaxResults = lens _liaMaxResults (\s a -> s {_liaMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
liaInstanceId :: Lens' ListInstanceAttributes Text
liaInstanceId = lens _liaInstanceId (\s a -> s {_liaInstanceId = a})

instance AWSPager ListInstanceAttributes where
  page rq rs
    | stop (rs ^. liarsNextToken) = Nothing
    | stop (rs ^. liarsAttributes) = Nothing
    | otherwise = Just $ rq & liaNextToken .~ rs ^. liarsNextToken

instance AWSRequest ListInstanceAttributes where
  type Rs ListInstanceAttributes = ListInstanceAttributesResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListInstanceAttributesResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Attributes" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListInstanceAttributes

instance NFData ListInstanceAttributes

instance ToHeaders ListInstanceAttributes where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListInstanceAttributes where
  toPath ListInstanceAttributes' {..} =
    mconcat ["/instance/", toBS _liaInstanceId, "/attributes"]

instance ToQuery ListInstanceAttributes where
  toQuery ListInstanceAttributes' {..} =
    mconcat
      ["nextToken" =: _liaNextToken, "maxResults" =: _liaMaxResults]

-- | /See:/ 'listInstanceAttributesResponse' smart constructor.
data ListInstanceAttributesResponse = ListInstanceAttributesResponse'
  { _liarsNextToken ::
      !(Maybe Text),
    _liarsAttributes ::
      !(Maybe [Attribute]),
    _liarsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListInstanceAttributesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liarsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'liarsAttributes' - The attribute types.
--
-- * 'liarsResponseStatus' - -- | The response status code.
listInstanceAttributesResponse ::
  -- | 'liarsResponseStatus'
  Int ->
  ListInstanceAttributesResponse
listInstanceAttributesResponse pResponseStatus_ =
  ListInstanceAttributesResponse'
    { _liarsNextToken = Nothing,
      _liarsAttributes = Nothing,
      _liarsResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
liarsNextToken :: Lens' ListInstanceAttributesResponse (Maybe Text)
liarsNextToken = lens _liarsNextToken (\s a -> s {_liarsNextToken = a})

-- | The attribute types.
liarsAttributes :: Lens' ListInstanceAttributesResponse [Attribute]
liarsAttributes = lens _liarsAttributes (\s a -> s {_liarsAttributes = a}) . _Default . _Coerce

-- | -- | The response status code.
liarsResponseStatus :: Lens' ListInstanceAttributesResponse Int
liarsResponseStatus = lens _liarsResponseStatus (\s a -> s {_liarsResponseStatus = a})

instance NFData ListInstanceAttributesResponse
