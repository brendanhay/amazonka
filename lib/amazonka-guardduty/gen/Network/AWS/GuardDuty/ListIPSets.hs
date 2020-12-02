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
-- Module      : Network.AWS.GuardDuty.ListIPSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the IPSets of the GuardDuty service specified by the detector ID. If you use this operation from a member account, the IPSets returned are the IPSets from the associated master account.
--
--
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListIPSets
  ( -- * Creating a Request
    listIPSets,
    ListIPSets,

    -- * Request Lenses
    lisNextToken,
    lisMaxResults,
    lisDetectorId,

    -- * Destructuring the Response
    listIPSetsResponse,
    ListIPSetsResponse,

    -- * Response Lenses
    lisrsNextToken,
    lisrsResponseStatus,
    lisrsIPSetIds,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listIPSets' smart constructor.
data ListIPSets = ListIPSets'
  { _lisNextToken :: !(Maybe Text),
    _lisMaxResults :: !(Maybe Nat),
    _lisDetectorId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListIPSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisNextToken' - You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- * 'lisMaxResults' - You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
--
-- * 'lisDetectorId' - The unique ID of the detector that the IPSet is associated with.
listIPSets ::
  -- | 'lisDetectorId'
  Text ->
  ListIPSets
listIPSets pDetectorId_ =
  ListIPSets'
    { _lisNextToken = Nothing,
      _lisMaxResults = Nothing,
      _lisDetectorId = pDetectorId_
    }

-- | You can use this parameter when paginating results. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
lisNextToken :: Lens' ListIPSets (Maybe Text)
lisNextToken = lens _lisNextToken (\s a -> s {_lisNextToken = a})

-- | You can use this parameter to indicate the maximum number of items you want in the response. The default value is 50. The maximum value is 50.
lisMaxResults :: Lens' ListIPSets (Maybe Natural)
lisMaxResults = lens _lisMaxResults (\s a -> s {_lisMaxResults = a}) . mapping _Nat

-- | The unique ID of the detector that the IPSet is associated with.
lisDetectorId :: Lens' ListIPSets Text
lisDetectorId = lens _lisDetectorId (\s a -> s {_lisDetectorId = a})

instance AWSPager ListIPSets where
  page rq rs
    | stop (rs ^. lisrsNextToken) = Nothing
    | stop (rs ^. lisrsIPSetIds) = Nothing
    | otherwise = Just $ rq & lisNextToken .~ rs ^. lisrsNextToken

instance AWSRequest ListIPSets where
  type Rs ListIPSets = ListIPSetsResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          ListIPSetsResponse'
            <$> (x .?> "nextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "ipSetIds" .!@ mempty)
      )

instance Hashable ListIPSets

instance NFData ListIPSets

instance ToHeaders ListIPSets where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListIPSets where
  toPath ListIPSets' {..} =
    mconcat ["/detector/", toBS _lisDetectorId, "/ipset"]

instance ToQuery ListIPSets where
  toQuery ListIPSets' {..} =
    mconcat
      ["nextToken" =: _lisNextToken, "maxResults" =: _lisMaxResults]

-- | /See:/ 'listIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { _lisrsNextToken ::
      !(Maybe Text),
    _lisrsResponseStatus :: !Int,
    _lisrsIPSetIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListIPSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lisrsNextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
--
-- * 'lisrsResponseStatus' - -- | The response status code.
--
-- * 'lisrsIPSetIds' - The IDs of the IPSet resources.
listIPSetsResponse ::
  -- | 'lisrsResponseStatus'
  Int ->
  ListIPSetsResponse
listIPSetsResponse pResponseStatus_ =
  ListIPSetsResponse'
    { _lisrsNextToken = Nothing,
      _lisrsResponseStatus = pResponseStatus_,
      _lisrsIPSetIds = mempty
    }

-- | The pagination parameter to be used on the next list operation to retrieve more items.
lisrsNextToken :: Lens' ListIPSetsResponse (Maybe Text)
lisrsNextToken = lens _lisrsNextToken (\s a -> s {_lisrsNextToken = a})

-- | -- | The response status code.
lisrsResponseStatus :: Lens' ListIPSetsResponse Int
lisrsResponseStatus = lens _lisrsResponseStatus (\s a -> s {_lisrsResponseStatus = a})

-- | The IDs of the IPSet resources.
lisrsIPSetIds :: Lens' ListIPSetsResponse [Text]
lisrsIPSetIds = lens _lisrsIPSetIds (\s a -> s {_lisrsIPSetIds = a}) . _Coerce

instance NFData ListIPSetsResponse
