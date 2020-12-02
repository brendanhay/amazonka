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
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the associations between your Direct Connect gateways and virtual private gateways. You must specify a Direct Connect gateway, a virtual private gateway, or both. If you specify a Direct Connect gateway, the response contains all virtual private gateways associated with the Direct Connect gateway. If you specify a virtual private gateway, the response contains all Direct Connect gateways associated with the virtual private gateway. If you specify both, the response contains the association between the Direct Connect gateway and the virtual private gateway.
--
--
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
  ( -- * Creating a Request
    describeDirectConnectGatewayAssociations,
    DescribeDirectConnectGatewayAssociations,

    -- * Request Lenses
    ddcgaVirtualGatewayId,
    ddcgaAssociationId,
    ddcgaAssociatedGatewayId,
    ddcgaDirectConnectGatewayId,
    ddcgaNextToken,
    ddcgaMaxResults,

    -- * Destructuring the Response
    describeDirectConnectGatewayAssociationsResponse,
    DescribeDirectConnectGatewayAssociationsResponse,

    -- * Response Lenses
    ddcgarsNextToken,
    ddcgarsDirectConnectGatewayAssociations,
    ddcgarsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDirectConnectGatewayAssociations' smart constructor.
data DescribeDirectConnectGatewayAssociations = DescribeDirectConnectGatewayAssociations'
  { _ddcgaVirtualGatewayId ::
      !( Maybe
           Text
       ),
    _ddcgaAssociationId ::
      !( Maybe
           Text
       ),
    _ddcgaAssociatedGatewayId ::
      !( Maybe
           Text
       ),
    _ddcgaDirectConnectGatewayId ::
      !( Maybe
           Text
       ),
    _ddcgaNextToken ::
      !( Maybe
           Text
       ),
    _ddcgaMaxResults ::
      !( Maybe
           Int
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDirectConnectGatewayAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgaVirtualGatewayId' - The ID of the virtual private gateway.
--
-- * 'ddcgaAssociationId' - The ID of the Direct Connect gateway association.
--
-- * 'ddcgaAssociatedGatewayId' - The ID of the associated gateway.
--
-- * 'ddcgaDirectConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- * 'ddcgaNextToken' - The token provided in the previous call to retrieve the next page.
--
-- * 'ddcgaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
describeDirectConnectGatewayAssociations ::
  DescribeDirectConnectGatewayAssociations
describeDirectConnectGatewayAssociations =
  DescribeDirectConnectGatewayAssociations'
    { _ddcgaVirtualGatewayId =
        Nothing,
      _ddcgaAssociationId = Nothing,
      _ddcgaAssociatedGatewayId = Nothing,
      _ddcgaDirectConnectGatewayId = Nothing,
      _ddcgaNextToken = Nothing,
      _ddcgaMaxResults = Nothing
    }

-- | The ID of the virtual private gateway.
ddcgaVirtualGatewayId :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaVirtualGatewayId = lens _ddcgaVirtualGatewayId (\s a -> s {_ddcgaVirtualGatewayId = a})

-- | The ID of the Direct Connect gateway association.
ddcgaAssociationId :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaAssociationId = lens _ddcgaAssociationId (\s a -> s {_ddcgaAssociationId = a})

-- | The ID of the associated gateway.
ddcgaAssociatedGatewayId :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaAssociatedGatewayId = lens _ddcgaAssociatedGatewayId (\s a -> s {_ddcgaAssociatedGatewayId = a})

-- | The ID of the Direct Connect gateway.
ddcgaDirectConnectGatewayId :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaDirectConnectGatewayId = lens _ddcgaDirectConnectGatewayId (\s a -> s {_ddcgaDirectConnectGatewayId = a})

-- | The token provided in the previous call to retrieve the next page.
ddcgaNextToken :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaNextToken = lens _ddcgaNextToken (\s a -> s {_ddcgaNextToken = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value. If @MaxResults@ is given a value larger than 100, only 100 results are returned.
ddcgaMaxResults :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Int)
ddcgaMaxResults = lens _ddcgaMaxResults (\s a -> s {_ddcgaMaxResults = a})

instance AWSPager DescribeDirectConnectGatewayAssociations where
  page rq rs
    | stop (rs ^. ddcgarsNextToken) = Nothing
    | stop (rs ^. ddcgarsDirectConnectGatewayAssociations) = Nothing
    | otherwise = Just $ rq & ddcgaNextToken .~ rs ^. ddcgarsNextToken

instance AWSRequest DescribeDirectConnectGatewayAssociations where
  type
    Rs DescribeDirectConnectGatewayAssociations =
      DescribeDirectConnectGatewayAssociationsResponse
  request = postJSON directConnect
  response =
    receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAssociationsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "directConnectGatewayAssociations" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDirectConnectGatewayAssociations

instance NFData DescribeDirectConnectGatewayAssociations

instance ToHeaders DescribeDirectConnectGatewayAssociations where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "OvertureService.DescribeDirectConnectGatewayAssociations" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeDirectConnectGatewayAssociations where
  toJSON DescribeDirectConnectGatewayAssociations' {..} =
    object
      ( catMaybes
          [ ("virtualGatewayId" .=) <$> _ddcgaVirtualGatewayId,
            ("associationId" .=) <$> _ddcgaAssociationId,
            ("associatedGatewayId" .=) <$> _ddcgaAssociatedGatewayId,
            ("directConnectGatewayId" .=) <$> _ddcgaDirectConnectGatewayId,
            ("nextToken" .=) <$> _ddcgaNextToken,
            ("maxResults" .=) <$> _ddcgaMaxResults
          ]
      )

instance ToPath DescribeDirectConnectGatewayAssociations where
  toPath = const "/"

instance ToQuery DescribeDirectConnectGatewayAssociations where
  toQuery = const mempty

-- | /See:/ 'describeDirectConnectGatewayAssociationsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationsResponse = DescribeDirectConnectGatewayAssociationsResponse'
  { _ddcgarsNextToken ::
      !( Maybe
           Text
       ),
    _ddcgarsDirectConnectGatewayAssociations ::
      !( Maybe
           [DirectConnectGatewayAssociation]
       ),
    _ddcgarsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeDirectConnectGatewayAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgarsNextToken' - The token to retrieve the next page.
--
-- * 'ddcgarsDirectConnectGatewayAssociations' - Information about the associations.
--
-- * 'ddcgarsResponseStatus' - -- | The response status code.
describeDirectConnectGatewayAssociationsResponse ::
  -- | 'ddcgarsResponseStatus'
  Int ->
  DescribeDirectConnectGatewayAssociationsResponse
describeDirectConnectGatewayAssociationsResponse pResponseStatus_ =
  DescribeDirectConnectGatewayAssociationsResponse'
    { _ddcgarsNextToken =
        Nothing,
      _ddcgarsDirectConnectGatewayAssociations =
        Nothing,
      _ddcgarsResponseStatus = pResponseStatus_
    }

-- | The token to retrieve the next page.
ddcgarsNextToken :: Lens' DescribeDirectConnectGatewayAssociationsResponse (Maybe Text)
ddcgarsNextToken = lens _ddcgarsNextToken (\s a -> s {_ddcgarsNextToken = a})

-- | Information about the associations.
ddcgarsDirectConnectGatewayAssociations :: Lens' DescribeDirectConnectGatewayAssociationsResponse [DirectConnectGatewayAssociation]
ddcgarsDirectConnectGatewayAssociations = lens _ddcgarsDirectConnectGatewayAssociations (\s a -> s {_ddcgarsDirectConnectGatewayAssociations = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcgarsResponseStatus :: Lens' DescribeDirectConnectGatewayAssociationsResponse Int
ddcgarsResponseStatus = lens _ddcgarsResponseStatus (\s a -> s {_ddcgarsResponseStatus = a})

instance NFData DescribeDirectConnectGatewayAssociationsResponse
