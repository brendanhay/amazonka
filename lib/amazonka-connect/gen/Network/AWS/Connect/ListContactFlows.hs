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
-- Module      : Network.AWS.Connect.ListContactFlows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the contact flows for the specified Amazon Connect instance.
--
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
--
-- For more information about contact flows, see <https://docs.aws.amazon.com/connect/latest/adminguide/concepts-contact-flows.html Contact Flows> in the /Amazon Connect Administrator Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListContactFlows
  ( -- * Creating a Request
    listContactFlows,
    ListContactFlows,

    -- * Request Lenses
    lcfContactFlowTypes,
    lcfNextToken,
    lcfMaxResults,
    lcfInstanceId,

    -- * Destructuring the Response
    listContactFlowsResponse,
    ListContactFlowsResponse,

    -- * Response Lenses
    lcfrsContactFlowSummaryList,
    lcfrsNextToken,
    lcfrsResponseStatus,
  )
where

import Network.AWS.Connect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listContactFlows' smart constructor.
data ListContactFlows = ListContactFlows'
  { _lcfContactFlowTypes ::
      !(Maybe [ContactFlowType]),
    _lcfNextToken :: !(Maybe Text),
    _lcfMaxResults :: !(Maybe Nat),
    _lcfInstanceId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListContactFlows' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfContactFlowTypes' - The type of contact flow.
--
-- * 'lcfNextToken' - The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- * 'lcfMaxResults' - The maximimum number of results to return per page.
--
-- * 'lcfInstanceId' - The identifier of the Amazon Connect instance.
listContactFlows ::
  -- | 'lcfInstanceId'
  Text ->
  ListContactFlows
listContactFlows pInstanceId_ =
  ListContactFlows'
    { _lcfContactFlowTypes = Nothing,
      _lcfNextToken = Nothing,
      _lcfMaxResults = Nothing,
      _lcfInstanceId = pInstanceId_
    }

-- | The type of contact flow.
lcfContactFlowTypes :: Lens' ListContactFlows [ContactFlowType]
lcfContactFlowTypes = lens _lcfContactFlowTypes (\s a -> s {_lcfContactFlowTypes = a}) . _Default . _Coerce

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
lcfNextToken :: Lens' ListContactFlows (Maybe Text)
lcfNextToken = lens _lcfNextToken (\s a -> s {_lcfNextToken = a})

-- | The maximimum number of results to return per page.
lcfMaxResults :: Lens' ListContactFlows (Maybe Natural)
lcfMaxResults = lens _lcfMaxResults (\s a -> s {_lcfMaxResults = a}) . mapping _Nat

-- | The identifier of the Amazon Connect instance.
lcfInstanceId :: Lens' ListContactFlows Text
lcfInstanceId = lens _lcfInstanceId (\s a -> s {_lcfInstanceId = a})

instance AWSPager ListContactFlows where
  page rq rs
    | stop (rs ^. lcfrsNextToken) = Nothing
    | stop (rs ^. lcfrsContactFlowSummaryList) = Nothing
    | otherwise = Just $ rq & lcfNextToken .~ rs ^. lcfrsNextToken

instance AWSRequest ListContactFlows where
  type Rs ListContactFlows = ListContactFlowsResponse
  request = get connect
  response =
    receiveJSON
      ( \s h x ->
          ListContactFlowsResponse'
            <$> (x .?> "ContactFlowSummaryList" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListContactFlows

instance NFData ListContactFlows

instance ToHeaders ListContactFlows where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath ListContactFlows where
  toPath ListContactFlows' {..} =
    mconcat ["/contact-flows-summary/", toBS _lcfInstanceId]

instance ToQuery ListContactFlows where
  toQuery ListContactFlows' {..} =
    mconcat
      [ "contactFlowTypes"
          =: toQuery (toQueryList "member" <$> _lcfContactFlowTypes),
        "nextToken" =: _lcfNextToken,
        "maxResults" =: _lcfMaxResults
      ]

-- | /See:/ 'listContactFlowsResponse' smart constructor.
data ListContactFlowsResponse = ListContactFlowsResponse'
  { _lcfrsContactFlowSummaryList ::
      !(Maybe [ContactFlowSummary]),
    _lcfrsNextToken :: !(Maybe Text),
    _lcfrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListContactFlowsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcfrsContactFlowSummaryList' - Information about the contact flows.
--
-- * 'lcfrsNextToken' - If there are additional results, this is the token for the next set of results.
--
-- * 'lcfrsResponseStatus' - -- | The response status code.
listContactFlowsResponse ::
  -- | 'lcfrsResponseStatus'
  Int ->
  ListContactFlowsResponse
listContactFlowsResponse pResponseStatus_ =
  ListContactFlowsResponse'
    { _lcfrsContactFlowSummaryList = Nothing,
      _lcfrsNextToken = Nothing,
      _lcfrsResponseStatus = pResponseStatus_
    }

-- | Information about the contact flows.
lcfrsContactFlowSummaryList :: Lens' ListContactFlowsResponse [ContactFlowSummary]
lcfrsContactFlowSummaryList = lens _lcfrsContactFlowSummaryList (\s a -> s {_lcfrsContactFlowSummaryList = a}) . _Default . _Coerce

-- | If there are additional results, this is the token for the next set of results.
lcfrsNextToken :: Lens' ListContactFlowsResponse (Maybe Text)
lcfrsNextToken = lens _lcfrsNextToken (\s a -> s {_lcfrsNextToken = a})

-- | -- | The response status code.
lcfrsResponseStatus :: Lens' ListContactFlowsResponse Int
lcfrsResponseStatus = lens _lcfrsResponseStatus (\s a -> s {_lcfrsResponseStatus = a})

instance NFData ListContactFlowsResponse
