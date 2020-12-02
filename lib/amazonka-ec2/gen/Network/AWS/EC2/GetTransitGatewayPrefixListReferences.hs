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
-- Module      : Network.AWS.EC2.GetTransitGatewayPrefixListReferences
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the prefix list references in a specified transit gateway route table.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayPrefixListReferences
  ( -- * Creating a Request
    getTransitGatewayPrefixListReferences,
    GetTransitGatewayPrefixListReferences,

    -- * Request Lenses
    gtgplrFilters,
    gtgplrNextToken,
    gtgplrDryRun,
    gtgplrMaxResults,
    gtgplrTransitGatewayRouteTableId,

    -- * Destructuring the Response
    getTransitGatewayPrefixListReferencesResponse,
    GetTransitGatewayPrefixListReferencesResponse,

    -- * Response Lenses
    gtgplrrsTransitGatewayPrefixListReferences,
    gtgplrrsNextToken,
    gtgplrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTransitGatewayPrefixListReferences' smart constructor.
data GetTransitGatewayPrefixListReferences = GetTransitGatewayPrefixListReferences'
  { _gtgplrFilters ::
      !( Maybe
           [Filter]
       ),
    _gtgplrNextToken ::
      !(Maybe Text),
    _gtgplrDryRun ::
      !(Maybe Bool),
    _gtgplrMaxResults ::
      !(Maybe Nat),
    _gtgplrTransitGatewayRouteTableId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetTransitGatewayPrefixListReferences' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgplrFilters' - One or more filters. The possible values are:     * @attachment.resource-id@ - The ID of the resource for the attachment.     * @attachment.resource-type@ - The type of resource for the attachment. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .     * @attachment.transit-gateway-attachment-id@ - The ID of the attachment.     * @is-blackhole@ - Whether traffic matching the route is blocked (@true@ | @false@ ).     * @prefix-list-id@ - The ID of the prefix list.     * @prefix-list-owner-id@ - The ID of the owner of the prefix list.     * @state@ - The state of the prefix list reference (@pending@ | @available@ | @modifying@ | @deleting@ ).
--
-- * 'gtgplrNextToken' - The token for the next page of results.
--
-- * 'gtgplrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gtgplrMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'gtgplrTransitGatewayRouteTableId' - The ID of the transit gateway route table.
getTransitGatewayPrefixListReferences ::
  -- | 'gtgplrTransitGatewayRouteTableId'
  Text ->
  GetTransitGatewayPrefixListReferences
getTransitGatewayPrefixListReferences pTransitGatewayRouteTableId_ =
  GetTransitGatewayPrefixListReferences'
    { _gtgplrFilters = Nothing,
      _gtgplrNextToken = Nothing,
      _gtgplrDryRun = Nothing,
      _gtgplrMaxResults = Nothing,
      _gtgplrTransitGatewayRouteTableId =
        pTransitGatewayRouteTableId_
    }

-- | One or more filters. The possible values are:     * @attachment.resource-id@ - The ID of the resource for the attachment.     * @attachment.resource-type@ - The type of resource for the attachment. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .     * @attachment.transit-gateway-attachment-id@ - The ID of the attachment.     * @is-blackhole@ - Whether traffic matching the route is blocked (@true@ | @false@ ).     * @prefix-list-id@ - The ID of the prefix list.     * @prefix-list-owner-id@ - The ID of the owner of the prefix list.     * @state@ - The state of the prefix list reference (@pending@ | @available@ | @modifying@ | @deleting@ ).
gtgplrFilters :: Lens' GetTransitGatewayPrefixListReferences [Filter]
gtgplrFilters = lens _gtgplrFilters (\s a -> s {_gtgplrFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
gtgplrNextToken :: Lens' GetTransitGatewayPrefixListReferences (Maybe Text)
gtgplrNextToken = lens _gtgplrNextToken (\s a -> s {_gtgplrNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gtgplrDryRun :: Lens' GetTransitGatewayPrefixListReferences (Maybe Bool)
gtgplrDryRun = lens _gtgplrDryRun (\s a -> s {_gtgplrDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
gtgplrMaxResults :: Lens' GetTransitGatewayPrefixListReferences (Maybe Natural)
gtgplrMaxResults = lens _gtgplrMaxResults (\s a -> s {_gtgplrMaxResults = a}) . mapping _Nat

-- | The ID of the transit gateway route table.
gtgplrTransitGatewayRouteTableId :: Lens' GetTransitGatewayPrefixListReferences Text
gtgplrTransitGatewayRouteTableId = lens _gtgplrTransitGatewayRouteTableId (\s a -> s {_gtgplrTransitGatewayRouteTableId = a})

instance AWSPager GetTransitGatewayPrefixListReferences where
  page rq rs
    | stop (rs ^. gtgplrrsNextToken) = Nothing
    | stop (rs ^. gtgplrrsTransitGatewayPrefixListReferences) = Nothing
    | otherwise =
      Just $ rq & gtgplrNextToken .~ rs ^. gtgplrrsNextToken

instance AWSRequest GetTransitGatewayPrefixListReferences where
  type
    Rs GetTransitGatewayPrefixListReferences =
      GetTransitGatewayPrefixListReferencesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetTransitGatewayPrefixListReferencesResponse'
            <$> ( x .@? "transitGatewayPrefixListReferenceSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable GetTransitGatewayPrefixListReferences

instance NFData GetTransitGatewayPrefixListReferences

instance ToHeaders GetTransitGatewayPrefixListReferences where
  toHeaders = const mempty

instance ToPath GetTransitGatewayPrefixListReferences where
  toPath = const "/"

instance ToQuery GetTransitGatewayPrefixListReferences where
  toQuery GetTransitGatewayPrefixListReferences' {..} =
    mconcat
      [ "Action"
          =: ("GetTransitGatewayPrefixListReferences" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _gtgplrFilters),
        "NextToken" =: _gtgplrNextToken,
        "DryRun" =: _gtgplrDryRun,
        "MaxResults" =: _gtgplrMaxResults,
        "TransitGatewayRouteTableId" =: _gtgplrTransitGatewayRouteTableId
      ]

-- | /See:/ 'getTransitGatewayPrefixListReferencesResponse' smart constructor.
data GetTransitGatewayPrefixListReferencesResponse = GetTransitGatewayPrefixListReferencesResponse'
  { _gtgplrrsTransitGatewayPrefixListReferences ::
      !( Maybe
           [TransitGatewayPrefixListReference]
       ),
    _gtgplrrsNextToken ::
      !( Maybe
           Text
       ),
    _gtgplrrsResponseStatus ::
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

-- | Creates a value of 'GetTransitGatewayPrefixListReferencesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgplrrsTransitGatewayPrefixListReferences' - Information about the prefix list references.
--
-- * 'gtgplrrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'gtgplrrsResponseStatus' - -- | The response status code.
getTransitGatewayPrefixListReferencesResponse ::
  -- | 'gtgplrrsResponseStatus'
  Int ->
  GetTransitGatewayPrefixListReferencesResponse
getTransitGatewayPrefixListReferencesResponse pResponseStatus_ =
  GetTransitGatewayPrefixListReferencesResponse'
    { _gtgplrrsTransitGatewayPrefixListReferences =
        Nothing,
      _gtgplrrsNextToken = Nothing,
      _gtgplrrsResponseStatus = pResponseStatus_
    }

-- | Information about the prefix list references.
gtgplrrsTransitGatewayPrefixListReferences :: Lens' GetTransitGatewayPrefixListReferencesResponse [TransitGatewayPrefixListReference]
gtgplrrsTransitGatewayPrefixListReferences = lens _gtgplrrsTransitGatewayPrefixListReferences (\s a -> s {_gtgplrrsTransitGatewayPrefixListReferences = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
gtgplrrsNextToken :: Lens' GetTransitGatewayPrefixListReferencesResponse (Maybe Text)
gtgplrrsNextToken = lens _gtgplrrsNextToken (\s a -> s {_gtgplrrsNextToken = a})

-- | -- | The response status code.
gtgplrrsResponseStatus :: Lens' GetTransitGatewayPrefixListReferencesResponse Int
gtgplrrsResponseStatus = lens _gtgplrrsResponseStatus (\s a -> s {_gtgplrrsResponseStatus = a})

instance NFData GetTransitGatewayPrefixListReferencesResponse
