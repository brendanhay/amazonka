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
-- Module      : Network.AWS.AlexaBusiness.ListGatewayGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway group summaries. Use GetGatewayGroup to retrieve details of a specific gateway group.
module Network.AWS.AlexaBusiness.ListGatewayGroups
  ( -- * Creating a Request
    listGatewayGroups,
    ListGatewayGroups,

    -- * Request Lenses
    lggNextToken,
    lggMaxResults,

    -- * Destructuring the Response
    listGatewayGroupsResponse,
    ListGatewayGroupsResponse,

    -- * Response Lenses
    lggrsGatewayGroups,
    lggrsNextToken,
    lggrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGatewayGroups' smart constructor.
data ListGatewayGroups = ListGatewayGroups'
  { _lggNextToken ::
      !(Maybe Text),
    _lggMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGatewayGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lggNextToken' - The token used to paginate though multiple pages of gateway group summaries.
--
-- * 'lggMaxResults' - The maximum number of gateway group summaries to return. The default is 50.
listGatewayGroups ::
  ListGatewayGroups
listGatewayGroups =
  ListGatewayGroups'
    { _lggNextToken = Nothing,
      _lggMaxResults = Nothing
    }

-- | The token used to paginate though multiple pages of gateway group summaries.
lggNextToken :: Lens' ListGatewayGroups (Maybe Text)
lggNextToken = lens _lggNextToken (\s a -> s {_lggNextToken = a})

-- | The maximum number of gateway group summaries to return. The default is 50.
lggMaxResults :: Lens' ListGatewayGroups (Maybe Natural)
lggMaxResults = lens _lggMaxResults (\s a -> s {_lggMaxResults = a}) . mapping _Nat

instance AWSRequest ListGatewayGroups where
  type Rs ListGatewayGroups = ListGatewayGroupsResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          ListGatewayGroupsResponse'
            <$> (x .?> "GatewayGroups" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable ListGatewayGroups

instance NFData ListGatewayGroups

instance ToHeaders ListGatewayGroups where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.ListGatewayGroups" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListGatewayGroups where
  toJSON ListGatewayGroups' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lggNextToken,
            ("MaxResults" .=) <$> _lggMaxResults
          ]
      )

instance ToPath ListGatewayGroups where
  toPath = const "/"

instance ToQuery ListGatewayGroups where
  toQuery = const mempty

-- | /See:/ 'listGatewayGroupsResponse' smart constructor.
data ListGatewayGroupsResponse = ListGatewayGroupsResponse'
  { _lggrsGatewayGroups ::
      !(Maybe [GatewayGroupSummary]),
    _lggrsNextToken :: !(Maybe Text),
    _lggrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGatewayGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lggrsGatewayGroups' - The gateway groups in the list.
--
-- * 'lggrsNextToken' - The token used to paginate though multiple pages of gateway group summaries.
--
-- * 'lggrsResponseStatus' - -- | The response status code.
listGatewayGroupsResponse ::
  -- | 'lggrsResponseStatus'
  Int ->
  ListGatewayGroupsResponse
listGatewayGroupsResponse pResponseStatus_ =
  ListGatewayGroupsResponse'
    { _lggrsGatewayGroups = Nothing,
      _lggrsNextToken = Nothing,
      _lggrsResponseStatus = pResponseStatus_
    }

-- | The gateway groups in the list.
lggrsGatewayGroups :: Lens' ListGatewayGroupsResponse [GatewayGroupSummary]
lggrsGatewayGroups = lens _lggrsGatewayGroups (\s a -> s {_lggrsGatewayGroups = a}) . _Default . _Coerce

-- | The token used to paginate though multiple pages of gateway group summaries.
lggrsNextToken :: Lens' ListGatewayGroupsResponse (Maybe Text)
lggrsNextToken = lens _lggrsNextToken (\s a -> s {_lggrsNextToken = a})

-- | -- | The response status code.
lggrsResponseStatus :: Lens' ListGatewayGroupsResponse Int
lggrsResponseStatus = lens _lggrsResponseStatus (\s a -> s {_lggrsResponseStatus = a})

instance NFData ListGatewayGroupsResponse
