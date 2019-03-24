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
-- Module      : Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the route table propagations for the specified transit gateway route table.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayRouteTablePropagations
    (
    -- * Creating a Request
      getTransitGatewayRouteTablePropagations
    , GetTransitGatewayRouteTablePropagations
    -- * Request Lenses
    , gtgrtpFilters
    , gtgrtpNextToken
    , gtgrtpDryRun
    , gtgrtpMaxResults
    , gtgrtpTransitGatewayRouteTableId

    -- * Destructuring the Response
    , getTransitGatewayRouteTablePropagationsResponse
    , GetTransitGatewayRouteTablePropagationsResponse
    -- * Response Lenses
    , gtgrtprsTransitGatewayRouteTablePropagations
    , gtgrtprsNextToken
    , gtgrtprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTransitGatewayRouteTablePropagations' smart constructor.
data GetTransitGatewayRouteTablePropagations = GetTransitGatewayRouteTablePropagations'
  { _gtgrtpFilters                    :: !(Maybe [Filter])
  , _gtgrtpNextToken                  :: !(Maybe Text)
  , _gtgrtpDryRun                     :: !(Maybe Bool)
  , _gtgrtpMaxResults                 :: !(Maybe Nat)
  , _gtgrtpTransitGatewayRouteTableId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTransitGatewayRouteTablePropagations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgrtpFilters' - One or more filters. The possible values are:     * @resource-id@ - The ID of the resource.     * @resource-type@ - The resource type (@vpc@ | @vpn@ ).     * @transit-gateway-attachment-id@ - The ID of the attachment.
--
-- * 'gtgrtpNextToken' - The token for the next page of results.
--
-- * 'gtgrtpDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gtgrtpMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'gtgrtpTransitGatewayRouteTableId' - The ID of the transit gateway route table.
getTransitGatewayRouteTablePropagations
    :: Text -- ^ 'gtgrtpTransitGatewayRouteTableId'
    -> GetTransitGatewayRouteTablePropagations
getTransitGatewayRouteTablePropagations pTransitGatewayRouteTableId_ =
  GetTransitGatewayRouteTablePropagations'
    { _gtgrtpFilters = Nothing
    , _gtgrtpNextToken = Nothing
    , _gtgrtpDryRun = Nothing
    , _gtgrtpMaxResults = Nothing
    , _gtgrtpTransitGatewayRouteTableId = pTransitGatewayRouteTableId_
    }


-- | One or more filters. The possible values are:     * @resource-id@ - The ID of the resource.     * @resource-type@ - The resource type (@vpc@ | @vpn@ ).     * @transit-gateway-attachment-id@ - The ID of the attachment.
gtgrtpFilters :: Lens' GetTransitGatewayRouteTablePropagations [Filter]
gtgrtpFilters = lens _gtgrtpFilters (\ s a -> s{_gtgrtpFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
gtgrtpNextToken :: Lens' GetTransitGatewayRouteTablePropagations (Maybe Text)
gtgrtpNextToken = lens _gtgrtpNextToken (\ s a -> s{_gtgrtpNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gtgrtpDryRun :: Lens' GetTransitGatewayRouteTablePropagations (Maybe Bool)
gtgrtpDryRun = lens _gtgrtpDryRun (\ s a -> s{_gtgrtpDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
gtgrtpMaxResults :: Lens' GetTransitGatewayRouteTablePropagations (Maybe Natural)
gtgrtpMaxResults = lens _gtgrtpMaxResults (\ s a -> s{_gtgrtpMaxResults = a}) . mapping _Nat

-- | The ID of the transit gateway route table.
gtgrtpTransitGatewayRouteTableId :: Lens' GetTransitGatewayRouteTablePropagations Text
gtgrtpTransitGatewayRouteTableId = lens _gtgrtpTransitGatewayRouteTableId (\ s a -> s{_gtgrtpTransitGatewayRouteTableId = a})

instance AWSPager
           GetTransitGatewayRouteTablePropagations
         where
        page rq rs
          | stop (rs ^. gtgrtprsNextToken) = Nothing
          | stop
              (rs ^. gtgrtprsTransitGatewayRouteTablePropagations)
            = Nothing
          | otherwise =
            Just $ rq &
              gtgrtpNextToken .~ rs ^. gtgrtprsNextToken

instance AWSRequest
           GetTransitGatewayRouteTablePropagations
         where
        type Rs GetTransitGatewayRouteTablePropagations =
             GetTransitGatewayRouteTablePropagationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 GetTransitGatewayRouteTablePropagationsResponse' <$>
                   (x .@? "transitGatewayRouteTablePropagations" .!@
                      mempty
                      >>= may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           GetTransitGatewayRouteTablePropagations
         where

instance NFData
           GetTransitGatewayRouteTablePropagations
         where

instance ToHeaders
           GetTransitGatewayRouteTablePropagations
         where
        toHeaders = const mempty

instance ToPath
           GetTransitGatewayRouteTablePropagations
         where
        toPath = const "/"

instance ToQuery
           GetTransitGatewayRouteTablePropagations
         where
        toQuery GetTransitGatewayRouteTablePropagations'{..}
          = mconcat
              ["Action" =:
                 ("GetTransitGatewayRouteTablePropagations" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _gtgrtpFilters),
               "NextToken" =: _gtgrtpNextToken,
               "DryRun" =: _gtgrtpDryRun,
               "MaxResults" =: _gtgrtpMaxResults,
               "TransitGatewayRouteTableId" =:
                 _gtgrtpTransitGatewayRouteTableId]

-- | /See:/ 'getTransitGatewayRouteTablePropagationsResponse' smart constructor.
data GetTransitGatewayRouteTablePropagationsResponse = GetTransitGatewayRouteTablePropagationsResponse'
  { _gtgrtprsTransitGatewayRouteTablePropagations :: !(Maybe [TransitGatewayRouteTablePropagation])
  , _gtgrtprsNextToken :: !(Maybe Text)
  , _gtgrtprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetTransitGatewayRouteTablePropagationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgrtprsTransitGatewayRouteTablePropagations' - Information about the route table propagations.
--
-- * 'gtgrtprsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'gtgrtprsResponseStatus' - -- | The response status code.
getTransitGatewayRouteTablePropagationsResponse
    :: Int -- ^ 'gtgrtprsResponseStatus'
    -> GetTransitGatewayRouteTablePropagationsResponse
getTransitGatewayRouteTablePropagationsResponse pResponseStatus_ =
  GetTransitGatewayRouteTablePropagationsResponse'
    { _gtgrtprsTransitGatewayRouteTablePropagations = Nothing
    , _gtgrtprsNextToken = Nothing
    , _gtgrtprsResponseStatus = pResponseStatus_
    }


-- | Information about the route table propagations.
gtgrtprsTransitGatewayRouteTablePropagations :: Lens' GetTransitGatewayRouteTablePropagationsResponse [TransitGatewayRouteTablePropagation]
gtgrtprsTransitGatewayRouteTablePropagations = lens _gtgrtprsTransitGatewayRouteTablePropagations (\ s a -> s{_gtgrtprsTransitGatewayRouteTablePropagations = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
gtgrtprsNextToken :: Lens' GetTransitGatewayRouteTablePropagationsResponse (Maybe Text)
gtgrtprsNextToken = lens _gtgrtprsNextToken (\ s a -> s{_gtgrtprsNextToken = a})

-- | -- | The response status code.
gtgrtprsResponseStatus :: Lens' GetTransitGatewayRouteTablePropagationsResponse Int
gtgrtprsResponseStatus = lens _gtgrtprsResponseStatus (\ s a -> s{_gtgrtprsResponseStatus = a})

instance NFData
           GetTransitGatewayRouteTablePropagationsResponse
         where
