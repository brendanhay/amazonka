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
-- Module      : Network.AWS.GuardDuty.ListThreatIntelSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the ThreatIntelSets of the GuardDuty service specified by the detector ID. If you use this operation from a member account, the ThreatIntelSets associated with the master account are returned.
--
--
--
-- This operation returns paginated results.
module Network.AWS.GuardDuty.ListThreatIntelSets
  ( -- * Creating a Request
    listThreatIntelSets,
    ListThreatIntelSets,

    -- * Request Lenses
    ltisNextToken,
    ltisMaxResults,
    ltisDetectorId,

    -- * Destructuring the Response
    listThreatIntelSetsResponse,
    ListThreatIntelSetsResponse,

    -- * Response Lenses
    ltisrsNextToken,
    ltisrsResponseStatus,
    ltisrsThreatIntelSetIds,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listThreatIntelSets' smart constructor.
data ListThreatIntelSets = ListThreatIntelSets'
  { _ltisNextToken ::
      !(Maybe Text),
    _ltisMaxResults :: !(Maybe Nat),
    _ltisDetectorId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThreatIntelSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltisNextToken' - You can use this parameter to paginate results in the response. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
--
-- * 'ltisMaxResults' - You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
--
-- * 'ltisDetectorId' - The unique ID of the detector that the threatIntelSet is associated with.
listThreatIntelSets ::
  -- | 'ltisDetectorId'
  Text ->
  ListThreatIntelSets
listThreatIntelSets pDetectorId_ =
  ListThreatIntelSets'
    { _ltisNextToken = Nothing,
      _ltisMaxResults = Nothing,
      _ltisDetectorId = pDetectorId_
    }

-- | You can use this parameter to paginate results in the response. Set the value of this parameter to null on your first call to the list action. For subsequent calls to the action, fill nextToken in the request with the value of NextToken from the previous response to continue listing data.
ltisNextToken :: Lens' ListThreatIntelSets (Maybe Text)
ltisNextToken = lens _ltisNextToken (\s a -> s {_ltisNextToken = a})

-- | You can use this parameter to indicate the maximum number of items that you want in the response. The default value is 50. The maximum value is 50.
ltisMaxResults :: Lens' ListThreatIntelSets (Maybe Natural)
ltisMaxResults = lens _ltisMaxResults (\s a -> s {_ltisMaxResults = a}) . mapping _Nat

-- | The unique ID of the detector that the threatIntelSet is associated with.
ltisDetectorId :: Lens' ListThreatIntelSets Text
ltisDetectorId = lens _ltisDetectorId (\s a -> s {_ltisDetectorId = a})

instance AWSPager ListThreatIntelSets where
  page rq rs
    | stop (rs ^. ltisrsNextToken) = Nothing
    | stop (rs ^. ltisrsThreatIntelSetIds) = Nothing
    | otherwise = Just $ rq & ltisNextToken .~ rs ^. ltisrsNextToken

instance AWSRequest ListThreatIntelSets where
  type Rs ListThreatIntelSets = ListThreatIntelSetsResponse
  request = get guardDuty
  response =
    receiveJSON
      ( \s h x ->
          ListThreatIntelSetsResponse'
            <$> (x .?> "nextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "threatIntelSetIds" .!@ mempty)
      )

instance Hashable ListThreatIntelSets

instance NFData ListThreatIntelSets

instance ToHeaders ListThreatIntelSets where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListThreatIntelSets where
  toPath ListThreatIntelSets' {..} =
    mconcat ["/detector/", toBS _ltisDetectorId, "/threatintelset"]

instance ToQuery ListThreatIntelSets where
  toQuery ListThreatIntelSets' {..} =
    mconcat
      ["nextToken" =: _ltisNextToken, "maxResults" =: _ltisMaxResults]

-- | /See:/ 'listThreatIntelSetsResponse' smart constructor.
data ListThreatIntelSetsResponse = ListThreatIntelSetsResponse'
  { _ltisrsNextToken ::
      !(Maybe Text),
    _ltisrsResponseStatus :: !Int,
    _ltisrsThreatIntelSetIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListThreatIntelSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltisrsNextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
--
-- * 'ltisrsResponseStatus' - -- | The response status code.
--
-- * 'ltisrsThreatIntelSetIds' - The IDs of the ThreatIntelSet resources.
listThreatIntelSetsResponse ::
  -- | 'ltisrsResponseStatus'
  Int ->
  ListThreatIntelSetsResponse
listThreatIntelSetsResponse pResponseStatus_ =
  ListThreatIntelSetsResponse'
    { _ltisrsNextToken = Nothing,
      _ltisrsResponseStatus = pResponseStatus_,
      _ltisrsThreatIntelSetIds = mempty
    }

-- | The pagination parameter to be used on the next list operation to retrieve more items.
ltisrsNextToken :: Lens' ListThreatIntelSetsResponse (Maybe Text)
ltisrsNextToken = lens _ltisrsNextToken (\s a -> s {_ltisrsNextToken = a})

-- | -- | The response status code.
ltisrsResponseStatus :: Lens' ListThreatIntelSetsResponse Int
ltisrsResponseStatus = lens _ltisrsResponseStatus (\s a -> s {_ltisrsResponseStatus = a})

-- | The IDs of the ThreatIntelSet resources.
ltisrsThreatIntelSetIds :: Lens' ListThreatIntelSetsResponse [Text]
ltisrsThreatIntelSetIds = lens _ltisrsThreatIntelSetIds (\s a -> s {_ltisrsThreatIntelSetIds = a}) . _Coerce

instance NFData ListThreatIntelSetsResponse
