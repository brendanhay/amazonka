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
-- Module      : Network.AWS.AlexaBusiness.ListGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of gateway summaries. Use GetGateway to retrieve details of a specific gateway. An optional gateway group ARN can be provided to only retrieve gateway summaries of gateways that are associated with that gateway group ARN.
module Network.AWS.AlexaBusiness.ListGateways
  ( -- * Creating a Request
    listGateways,
    ListGateways,

    -- * Request Lenses
    lgNextToken,
    lgGatewayGroupARN,
    lgMaxResults,

    -- * Destructuring the Response
    listGatewaysResponse,
    ListGatewaysResponse,

    -- * Response Lenses
    lgrsNextToken,
    lgrsGateways,
    lgrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listGateways' smart constructor.
data ListGateways = ListGateways'
  { _lgNextToken :: !(Maybe Text),
    _lgGatewayGroupARN :: !(Maybe Text),
    _lgMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgNextToken' - The token used to paginate though multiple pages of gateway summaries.
--
-- * 'lgGatewayGroupARN' - The gateway group ARN for which to list gateways.
--
-- * 'lgMaxResults' - The maximum number of gateway summaries to return. The default is 50.
listGateways ::
  ListGateways
listGateways =
  ListGateways'
    { _lgNextToken = Nothing,
      _lgGatewayGroupARN = Nothing,
      _lgMaxResults = Nothing
    }

-- | The token used to paginate though multiple pages of gateway summaries.
lgNextToken :: Lens' ListGateways (Maybe Text)
lgNextToken = lens _lgNextToken (\s a -> s {_lgNextToken = a})

-- | The gateway group ARN for which to list gateways.
lgGatewayGroupARN :: Lens' ListGateways (Maybe Text)
lgGatewayGroupARN = lens _lgGatewayGroupARN (\s a -> s {_lgGatewayGroupARN = a})

-- | The maximum number of gateway summaries to return. The default is 50.
lgMaxResults :: Lens' ListGateways (Maybe Natural)
lgMaxResults = lens _lgMaxResults (\s a -> s {_lgMaxResults = a}) . mapping _Nat

instance AWSRequest ListGateways where
  type Rs ListGateways = ListGatewaysResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          ListGatewaysResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "Gateways" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListGateways

instance NFData ListGateways

instance ToHeaders ListGateways where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AlexaForBusiness.ListGateways" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListGateways where
  toJSON ListGateways' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lgNextToken,
            ("GatewayGroupArn" .=) <$> _lgGatewayGroupARN,
            ("MaxResults" .=) <$> _lgMaxResults
          ]
      )

instance ToPath ListGateways where
  toPath = const "/"

instance ToQuery ListGateways where
  toQuery = const mempty

-- | /See:/ 'listGatewaysResponse' smart constructor.
data ListGatewaysResponse = ListGatewaysResponse'
  { _lgrsNextToken ::
      !(Maybe Text),
    _lgrsGateways :: !(Maybe [GatewaySummary]),
    _lgrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgrsNextToken' - The token used to paginate though multiple pages of gateway summaries.
--
-- * 'lgrsGateways' - The gateways in the list.
--
-- * 'lgrsResponseStatus' - -- | The response status code.
listGatewaysResponse ::
  -- | 'lgrsResponseStatus'
  Int ->
  ListGatewaysResponse
listGatewaysResponse pResponseStatus_ =
  ListGatewaysResponse'
    { _lgrsNextToken = Nothing,
      _lgrsGateways = Nothing,
      _lgrsResponseStatus = pResponseStatus_
    }

-- | The token used to paginate though multiple pages of gateway summaries.
lgrsNextToken :: Lens' ListGatewaysResponse (Maybe Text)
lgrsNextToken = lens _lgrsNextToken (\s a -> s {_lgrsNextToken = a})

-- | The gateways in the list.
lgrsGateways :: Lens' ListGatewaysResponse [GatewaySummary]
lgrsGateways = lens _lgrsGateways (\s a -> s {_lgrsGateways = a}) . _Default . _Coerce

-- | -- | The response status code.
lgrsResponseStatus :: Lens' ListGatewaysResponse Int
lgrsResponseStatus = lens _lgrsResponseStatus (\s a -> s {_lgrsResponseStatus = a})

instance NFData ListGatewaysResponse
