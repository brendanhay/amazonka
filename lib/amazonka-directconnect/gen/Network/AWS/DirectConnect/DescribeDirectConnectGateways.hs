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
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGateways
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your Direct Connect gateways or only the specified Direct Connect gateway. Deleted Direct Connect gateways are not returned.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGateways
  ( -- * Creating a Request
    describeDirectConnectGateways,
    DescribeDirectConnectGateways,

    -- * Request Lenses
    ddcgDirectConnectGatewayId,
    ddcgNextToken,
    ddcgMaxResults,

    -- * Destructuring the Response
    describeDirectConnectGatewaysResponse,
    DescribeDirectConnectGatewaysResponse,

    -- * Response Lenses
    ddcgrsDirectConnectGateways,
    ddcgrsNextToken,
    ddcgrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDirectConnectGateways' smart constructor.
data DescribeDirectConnectGateways = DescribeDirectConnectGateways'
  { _ddcgDirectConnectGatewayId ::
      !(Maybe Text),
    _ddcgNextToken :: !(Maybe Text),
    _ddcgMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDirectConnectGateways' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'ddcgNextToken' - The token provided in the previous call to retrieve the next page.
--
-- * 'ddcgMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
describeDirectConnectGateways ::
  DescribeDirectConnectGateways
describeDirectConnectGateways =
  DescribeDirectConnectGateways'
    { _ddcgDirectConnectGatewayId =
        Nothing,
      _ddcgNextToken = Nothing,
      _ddcgMaxResults = Nothing
    }

-- | The ID of the Direct Connect gateway.
ddcgDirectConnectGatewayId :: Lens' DescribeDirectConnectGateways (Maybe Text)
ddcgDirectConnectGatewayId = lens _ddcgDirectConnectGatewayId (\s a -> s {_ddcgDirectConnectGatewayId = a})

-- | The token provided in the previous call to retrieve the next page.
ddcgNextToken :: Lens' DescribeDirectConnectGateways (Maybe Text)
ddcgNextToken = lens _ddcgNextToken (\s a -> s {_ddcgNextToken = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
ddcgMaxResults :: Lens' DescribeDirectConnectGateways (Maybe Int)
ddcgMaxResults = lens _ddcgMaxResults (\s a -> s {_ddcgMaxResults = a})

instance AWSPager DescribeDirectConnectGateways where
  page rq rs
    | stop (rs ^. ddcgrsNextToken) = Nothing
    | stop (rs ^. ddcgrsDirectConnectGateways) = Nothing
    | otherwise = Just $ rq & ddcgNextToken .~ rs ^. ddcgrsNextToken

instance AWSRequest DescribeDirectConnectGateways where
  type
    Rs DescribeDirectConnectGateways =
      DescribeDirectConnectGatewaysResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewaysResponse'
            <$> (x .?> "directConnectGateways" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDirectConnectGateways

instance NFData DescribeDirectConnectGateways

instance ToHeaders DescribeDirectConnectGateways where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("OvertureService.DescribeDirectConnectGateways" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeDirectConnectGateways where
  toJSON DescribeDirectConnectGateways' {..} =
    object
      ( catMaybes
          [ ("directConnectGatewayId" .=) <$> _ddcgDirectConnectGatewayId,
            ("nextToken" .=) <$> _ddcgNextToken,
            ("maxResults" .=) <$> _ddcgMaxResults
          ]
      )

instance ToPath DescribeDirectConnectGateways where
  toPath = const "/"

instance ToQuery DescribeDirectConnectGateways where
  toQuery = const mempty

-- | /See:/ 'describeDirectConnectGatewaysResponse' smart constructor.
data DescribeDirectConnectGatewaysResponse = DescribeDirectConnectGatewaysResponse'
  { _ddcgrsDirectConnectGateways ::
      !( Maybe
           [DirectConnectGateway]
       ),
    _ddcgrsNextToken ::
      !(Maybe Text),
    _ddcgrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDirectConnectGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgrsDirectConnectGateways' - The Direct Connect gateways.
--
-- * 'ddcgrsNextToken' - The token to retrieve the next page.
--
-- * 'ddcgrsResponseStatus' - -- | The response status code.
describeDirectConnectGatewaysResponse ::
  -- | 'ddcgrsResponseStatus'
  Int ->
  DescribeDirectConnectGatewaysResponse
describeDirectConnectGatewaysResponse pResponseStatus_ =
  DescribeDirectConnectGatewaysResponse'
    { _ddcgrsDirectConnectGateways =
        Nothing,
      _ddcgrsNextToken = Nothing,
      _ddcgrsResponseStatus = pResponseStatus_
    }

-- | The Direct Connect gateways.
ddcgrsDirectConnectGateways :: Lens' DescribeDirectConnectGatewaysResponse [DirectConnectGateway]
ddcgrsDirectConnectGateways = lens _ddcgrsDirectConnectGateways (\s a -> s {_ddcgrsDirectConnectGateways = a}) . _Default . _Coerce

-- | The token to retrieve the next page.
ddcgrsNextToken :: Lens' DescribeDirectConnectGatewaysResponse (Maybe Text)
ddcgrsNextToken = lens _ddcgrsNextToken (\s a -> s {_ddcgrsNextToken = a})

-- | -- | The response status code.
ddcgrsResponseStatus :: Lens' DescribeDirectConnectGatewaysResponse Int
ddcgrsResponseStatus = lens _ddcgrsResponseStatus (\s a -> s {_ddcgrsResponseStatus = a})

instance NFData DescribeDirectConnectGatewaysResponse
