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
-- Module      : Network.AWS.Connect.ListApprovedOrigins
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all approved origins associated with the instance.
--
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListApprovedOrigins
  ( -- * Creating a Request
    listApprovedOrigins,
    ListApprovedOrigins,

    -- * Request Lenses
    laoNextToken,
    laoMaxResults,
    laoInstanceId,

    -- * Destructuring the Response
    listApprovedOriginsResponse,
    ListApprovedOriginsResponse,

    -- * Response Lenses
    laorsNextToken,
    laorsOrigins,
    laorsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listApprovedOrigins' smart constructor.
data ListApprovedOrigins = ListApprovedOrigins'
  { _laoNextToken ::
      !(Maybe Text),
    _laoMaxResults :: !(Maybe Nat),
    _laoInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApprovedOrigins' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laoNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'laoMaxResults' - The maximimum number of results to return per page.
--
-- * 'laoInstanceId' - The identifier of the Amazon Connect instance.
listApprovedOrigins ::
  -- | 'laoInstanceId'
  Text ->
  ListApprovedOrigins
listApprovedOrigins pInstanceId_ =
  ListApprovedOrigins'
    { _laoNextToken = Nothing,
      _laoMaxResults = Nothing,
      _laoInstanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
laoNextToken :: Lens' ListApprovedOrigins (Maybe Text)
laoNextToken = lens _laoNextToken (\s a -> s {_laoNextToken = a})

-- | The maximimum number of results to return per page.
laoMaxResults :: Lens' ListApprovedOrigins (Maybe Natural)
laoMaxResults = lens _laoMaxResults (\s a -> s {_laoMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
laoInstanceId :: Lens' ListApprovedOrigins Text
laoInstanceId = lens _laoInstanceId (\s a -> s {_laoInstanceId = a})

instance AWSPager ListApprovedOrigins where
  page rq rs
    | stop (rs ^. laorsNextToken) = Nothing
    | stop (rs ^. laorsOrigins) = Nothing
    | otherwise = Just $ rq & laoNextToken .~ rs ^. laorsNextToken

instance AWSRequest ListApprovedOrigins where
  type Rs ListApprovedOrigins = ListApprovedOriginsResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListApprovedOriginsResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Origins" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListApprovedOrigins

instance NFData ListApprovedOrigins

instance ToHeaders ListApprovedOrigins where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListApprovedOrigins where
  toPath ListApprovedOrigins' {..} =
    mconcat ["/instance/", toBS _laoInstanceId, "/approved-origins"]

instance ToQuery ListApprovedOrigins where
  toQuery ListApprovedOrigins' {..} =
    mconcat
      ["nextToken" =: _laoNextToken, "maxResults" =: _laoMaxResults]

-- | /See:/ 'listApprovedOriginsResponse' smart constructor.
data ListApprovedOriginsResponse = ListApprovedOriginsResponse'
  { _laorsNextToken ::
      !(Maybe Text),
    _laorsOrigins :: !(Maybe [Text]),
    _laorsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListApprovedOriginsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laorsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'laorsOrigins' - The approved origins.
--
-- * 'laorsResponseStatus' - -- | The response status code.
listApprovedOriginsResponse ::
  -- | 'laorsResponseStatus'
  Int ->
  ListApprovedOriginsResponse
listApprovedOriginsResponse pResponseStatus_ =
  ListApprovedOriginsResponse'
    { _laorsNextToken = Nothing,
      _laorsOrigins = Nothing,
      _laorsResponseStatus = pResponseStatus_
    }

-- | If there are additional results, this is the token for the next set of results.
laorsNextToken :: Lens' ListApprovedOriginsResponse (Maybe Text)
laorsNextToken = lens _laorsNextToken (\s a -> s {_laorsNextToken = a})

-- | The approved origins.
laorsOrigins :: Lens' ListApprovedOriginsResponse [Text]
laorsOrigins = lens _laorsOrigins (\s a -> s {_laorsOrigins = a}) . _Default . _Coerce

-- | -- | The response status code.
laorsResponseStatus :: Lens' ListApprovedOriginsResponse Int
laorsResponseStatus = lens _laorsResponseStatus (\s a -> s {_laorsResponseStatus = a})

instance NFData ListApprovedOriginsResponse
