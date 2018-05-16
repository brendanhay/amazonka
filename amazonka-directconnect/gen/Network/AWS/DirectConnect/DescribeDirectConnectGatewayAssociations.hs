{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all direct connect gateway and virtual private gateway (VGW) associations. Either a direct connect gateway ID or a VGW ID must be provided in the request. If a direct connect gateway ID is provided, the response returns all VGWs associated with the direct connect gateway. If a VGW ID is provided, the response returns all direct connect gateways associated with the VGW. If both are provided, the response only returns the association that matches both the direct connect gateway and the VGW.
--
--
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
    (
    -- * Creating a Request
      describeDirectConnectGatewayAssociations
    , DescribeDirectConnectGatewayAssociations
    -- * Request Lenses
    , ddcgaVirtualGatewayId
    , ddcgaDirectConnectGatewayId
    , ddcgaNextToken
    , ddcgaMaxResults

    -- * Destructuring the Response
    , describeDirectConnectGatewayAssociationsResponse
    , DescribeDirectConnectGatewayAssociationsResponse
    -- * Response Lenses
    , ddcgarsNextToken
    , ddcgarsDirectConnectGatewayAssociations
    , ddcgarsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the DescribeDirectConnectGatewayAssociations operation.
--
--
--
-- /See:/ 'describeDirectConnectGatewayAssociations' smart constructor.
data DescribeDirectConnectGatewayAssociations = DescribeDirectConnectGatewayAssociations'
  { _ddcgaVirtualGatewayId       :: !(Maybe Text)
  , _ddcgaDirectConnectGatewayId :: !(Maybe Text)
  , _ddcgaNextToken              :: !(Maybe Text)
  , _ddcgaMaxResults             :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectConnectGatewayAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgaVirtualGatewayId' - The ID of the virtual private gateway. Example: "vgw-abc123ef" Default: None
--
-- * 'ddcgaDirectConnectGatewayId' - The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
--
-- * 'ddcgaNextToken' - The token provided in the previous describe result to retrieve the next page of the result. Default: None
--
-- * 'ddcgaMaxResults' - The maximum number of direct connect gateway associations to return per page. Example: 15 Default: None
describeDirectConnectGatewayAssociations
    :: DescribeDirectConnectGatewayAssociations
describeDirectConnectGatewayAssociations =
  DescribeDirectConnectGatewayAssociations'
    { _ddcgaVirtualGatewayId = Nothing
    , _ddcgaDirectConnectGatewayId = Nothing
    , _ddcgaNextToken = Nothing
    , _ddcgaMaxResults = Nothing
    }


-- | The ID of the virtual private gateway. Example: "vgw-abc123ef" Default: None
ddcgaVirtualGatewayId :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaVirtualGatewayId = lens _ddcgaVirtualGatewayId (\ s a -> s{_ddcgaVirtualGatewayId = a})

-- | The ID of the direct connect gateway. Example: "abcd1234-dcba-5678-be23-cdef9876ab45" Default: None
ddcgaDirectConnectGatewayId :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaDirectConnectGatewayId = lens _ddcgaDirectConnectGatewayId (\ s a -> s{_ddcgaDirectConnectGatewayId = a})

-- | The token provided in the previous describe result to retrieve the next page of the result. Default: None
ddcgaNextToken :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Text)
ddcgaNextToken = lens _ddcgaNextToken (\ s a -> s{_ddcgaNextToken = a})

-- | The maximum number of direct connect gateway associations to return per page. Example: 15 Default: None
ddcgaMaxResults :: Lens' DescribeDirectConnectGatewayAssociations (Maybe Int)
ddcgaMaxResults = lens _ddcgaMaxResults (\ s a -> s{_ddcgaMaxResults = a})

instance AWSRequest
           DescribeDirectConnectGatewayAssociations
         where
        type Rs DescribeDirectConnectGatewayAssociations =
             DescribeDirectConnectGatewayAssociationsResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDirectConnectGatewayAssociationsResponse' <$>
                   (x .?> "nextToken") <*>
                     (x .?> "directConnectGatewayAssociations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeDirectConnectGatewayAssociations
         where

instance NFData
           DescribeDirectConnectGatewayAssociations
         where

instance ToHeaders
           DescribeDirectConnectGatewayAssociations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeDirectConnectGatewayAssociations"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON
           DescribeDirectConnectGatewayAssociations
         where
        toJSON DescribeDirectConnectGatewayAssociations'{..}
          = object
              (catMaybes
                 [("virtualGatewayId" .=) <$> _ddcgaVirtualGatewayId,
                  ("directConnectGatewayId" .=) <$>
                    _ddcgaDirectConnectGatewayId,
                  ("nextToken" .=) <$> _ddcgaNextToken,
                  ("maxResults" .=) <$> _ddcgaMaxResults])

instance ToPath
           DescribeDirectConnectGatewayAssociations
         where
        toPath = const "/"

instance ToQuery
           DescribeDirectConnectGatewayAssociations
         where
        toQuery = const mempty

-- | Container for the response from the DescribeDirectConnectGatewayAssociations API call
--
--
--
-- /See:/ 'describeDirectConnectGatewayAssociationsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationsResponse = DescribeDirectConnectGatewayAssociationsResponse'
  { _ddcgarsNextToken :: !(Maybe Text)
  , _ddcgarsDirectConnectGatewayAssociations :: !(Maybe [DirectConnectGatewayAssociation])
  , _ddcgarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDirectConnectGatewayAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcgarsNextToken' - Undocumented member.
--
-- * 'ddcgarsDirectConnectGatewayAssociations' - Information about the direct connect gateway associations.
--
-- * 'ddcgarsResponseStatus' - -- | The response status code.
describeDirectConnectGatewayAssociationsResponse
    :: Int -- ^ 'ddcgarsResponseStatus'
    -> DescribeDirectConnectGatewayAssociationsResponse
describeDirectConnectGatewayAssociationsResponse pResponseStatus_ =
  DescribeDirectConnectGatewayAssociationsResponse'
    { _ddcgarsNextToken = Nothing
    , _ddcgarsDirectConnectGatewayAssociations = Nothing
    , _ddcgarsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
ddcgarsNextToken :: Lens' DescribeDirectConnectGatewayAssociationsResponse (Maybe Text)
ddcgarsNextToken = lens _ddcgarsNextToken (\ s a -> s{_ddcgarsNextToken = a})

-- | Information about the direct connect gateway associations.
ddcgarsDirectConnectGatewayAssociations :: Lens' DescribeDirectConnectGatewayAssociationsResponse [DirectConnectGatewayAssociation]
ddcgarsDirectConnectGatewayAssociations = lens _ddcgarsDirectConnectGatewayAssociations (\ s a -> s{_ddcgarsDirectConnectGatewayAssociations = a}) . _Default . _Coerce

-- | -- | The response status code.
ddcgarsResponseStatus :: Lens' DescribeDirectConnectGatewayAssociationsResponse Int
ddcgarsResponseStatus = lens _ddcgarsResponseStatus (\ s a -> s{_ddcgarsResponseStatus = a})

instance NFData
           DescribeDirectConnectGatewayAssociationsResponse
         where
