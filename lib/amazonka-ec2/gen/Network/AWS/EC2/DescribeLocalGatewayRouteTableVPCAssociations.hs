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
-- Module      : Network.AWS.EC2.DescribeLocalGatewayRouteTableVPCAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified associations between VPCs and local gateway route tables.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGatewayRouteTableVPCAssociations
  ( -- * Creating a Request
    describeLocalGatewayRouteTableVPCAssociations,
    DescribeLocalGatewayRouteTableVPCAssociations,

    -- * Request Lenses
    dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds,
    dlgrtvpcaFilters,
    dlgrtvpcaNextToken,
    dlgrtvpcaDryRun,
    dlgrtvpcaMaxResults,

    -- * Destructuring the Response
    describeLocalGatewayRouteTableVPCAssociationsResponse,
    DescribeLocalGatewayRouteTableVPCAssociationsResponse,

    -- * Response Lenses
    dlgrtvpcarsLocalGatewayRouteTableVPCAssociations,
    dlgrtvpcarsNextToken,
    dlgrtvpcarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeLocalGatewayRouteTableVPCAssociations' smart constructor.
data DescribeLocalGatewayRouteTableVPCAssociations = DescribeLocalGatewayRouteTableVPCAssociations'
  { _dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds ::
      !( Maybe
           [Text]
       ),
    _dlgrtvpcaFilters ::
      !( Maybe
           [Filter]
       ),
    _dlgrtvpcaNextToken ::
      !( Maybe
           Text
       ),
    _dlgrtvpcaDryRun ::
      !( Maybe
           Bool
       ),
    _dlgrtvpcaMaxResults ::
      !( Maybe
           Nat
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeLocalGatewayRouteTableVPCAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds' - The IDs of the associations.
--
-- * 'dlgrtvpcaFilters' - One or more filters.     * @local-gateway-id@ - The ID of a local gateway.     * @local-gateway-route-table-id@ - The ID of the local gateway route table.     * @local-gateway-route-table-vpc-association-id@ - The ID of the association.     * @state@ - The state of the association.     * @vpc-id@ - The ID of the VPC.
--
-- * 'dlgrtvpcaNextToken' - The token for the next page of results.
--
-- * 'dlgrtvpcaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dlgrtvpcaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeLocalGatewayRouteTableVPCAssociations ::
  DescribeLocalGatewayRouteTableVPCAssociations
describeLocalGatewayRouteTableVPCAssociations =
  DescribeLocalGatewayRouteTableVPCAssociations'
    { _dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds =
        Nothing,
      _dlgrtvpcaFilters = Nothing,
      _dlgrtvpcaNextToken = Nothing,
      _dlgrtvpcaDryRun = Nothing,
      _dlgrtvpcaMaxResults = Nothing
    }

-- | The IDs of the associations.
dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds :: Lens' DescribeLocalGatewayRouteTableVPCAssociations [Text]
dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds = lens _dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds (\s a -> s {_dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds = a}) . _Default . _Coerce

-- | One or more filters.     * @local-gateway-id@ - The ID of a local gateway.     * @local-gateway-route-table-id@ - The ID of the local gateway route table.     * @local-gateway-route-table-vpc-association-id@ - The ID of the association.     * @state@ - The state of the association.     * @vpc-id@ - The ID of the VPC.
dlgrtvpcaFilters :: Lens' DescribeLocalGatewayRouteTableVPCAssociations [Filter]
dlgrtvpcaFilters = lens _dlgrtvpcaFilters (\s a -> s {_dlgrtvpcaFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dlgrtvpcaNextToken :: Lens' DescribeLocalGatewayRouteTableVPCAssociations (Maybe Text)
dlgrtvpcaNextToken = lens _dlgrtvpcaNextToken (\s a -> s {_dlgrtvpcaNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dlgrtvpcaDryRun :: Lens' DescribeLocalGatewayRouteTableVPCAssociations (Maybe Bool)
dlgrtvpcaDryRun = lens _dlgrtvpcaDryRun (\s a -> s {_dlgrtvpcaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dlgrtvpcaMaxResults :: Lens' DescribeLocalGatewayRouteTableVPCAssociations (Maybe Natural)
dlgrtvpcaMaxResults = lens _dlgrtvpcaMaxResults (\s a -> s {_dlgrtvpcaMaxResults = a}) . mapping _Nat

instance AWSPager DescribeLocalGatewayRouteTableVPCAssociations where
  page rq rs
    | stop (rs ^. dlgrtvpcarsNextToken) = Nothing
    | stop (rs ^. dlgrtvpcarsLocalGatewayRouteTableVPCAssociations) =
      Nothing
    | otherwise =
      Just $ rq & dlgrtvpcaNextToken .~ rs ^. dlgrtvpcarsNextToken

instance AWSRequest DescribeLocalGatewayRouteTableVPCAssociations where
  type
    Rs DescribeLocalGatewayRouteTableVPCAssociations =
      DescribeLocalGatewayRouteTableVPCAssociationsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeLocalGatewayRouteTableVPCAssociationsResponse'
            <$> ( x .@? "localGatewayRouteTableVpcAssociationSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeLocalGatewayRouteTableVPCAssociations

instance NFData DescribeLocalGatewayRouteTableVPCAssociations

instance ToHeaders DescribeLocalGatewayRouteTableVPCAssociations where
  toHeaders = const mempty

instance ToPath DescribeLocalGatewayRouteTableVPCAssociations where
  toPath = const "/"

instance ToQuery DescribeLocalGatewayRouteTableVPCAssociations where
  toQuery DescribeLocalGatewayRouteTableVPCAssociations' {..} =
    mconcat
      [ "Action"
          =: ("DescribeLocalGatewayRouteTableVpcAssociations" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          ( toQueryList "LocalGatewayRouteTableVpcAssociationId"
              <$> _dlgrtvpcaLocalGatewayRouteTableVPCAssociationIds
          ),
        toQuery (toQueryList "Filter" <$> _dlgrtvpcaFilters),
        "NextToken" =: _dlgrtvpcaNextToken,
        "DryRun" =: _dlgrtvpcaDryRun,
        "MaxResults" =: _dlgrtvpcaMaxResults
      ]

-- | /See:/ 'describeLocalGatewayRouteTableVPCAssociationsResponse' smart constructor.
data DescribeLocalGatewayRouteTableVPCAssociationsResponse = DescribeLocalGatewayRouteTableVPCAssociationsResponse'
  { _dlgrtvpcarsLocalGatewayRouteTableVPCAssociations ::
      !( Maybe
           [LocalGatewayRouteTableVPCAssociation]
       ),
    _dlgrtvpcarsNextToken ::
      !( Maybe
           Text
       ),
    _dlgrtvpcarsResponseStatus ::
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

-- | Creates a value of 'DescribeLocalGatewayRouteTableVPCAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgrtvpcarsLocalGatewayRouteTableVPCAssociations' - Information about the associations.
--
-- * 'dlgrtvpcarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dlgrtvpcarsResponseStatus' - -- | The response status code.
describeLocalGatewayRouteTableVPCAssociationsResponse ::
  -- | 'dlgrtvpcarsResponseStatus'
  Int ->
  DescribeLocalGatewayRouteTableVPCAssociationsResponse
describeLocalGatewayRouteTableVPCAssociationsResponse
  pResponseStatus_ =
    DescribeLocalGatewayRouteTableVPCAssociationsResponse'
      { _dlgrtvpcarsLocalGatewayRouteTableVPCAssociations =
          Nothing,
        _dlgrtvpcarsNextToken = Nothing,
        _dlgrtvpcarsResponseStatus =
          pResponseStatus_
      }

-- | Information about the associations.
dlgrtvpcarsLocalGatewayRouteTableVPCAssociations :: Lens' DescribeLocalGatewayRouteTableVPCAssociationsResponse [LocalGatewayRouteTableVPCAssociation]
dlgrtvpcarsLocalGatewayRouteTableVPCAssociations = lens _dlgrtvpcarsLocalGatewayRouteTableVPCAssociations (\s a -> s {_dlgrtvpcarsLocalGatewayRouteTableVPCAssociations = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dlgrtvpcarsNextToken :: Lens' DescribeLocalGatewayRouteTableVPCAssociationsResponse (Maybe Text)
dlgrtvpcarsNextToken = lens _dlgrtvpcarsNextToken (\s a -> s {_dlgrtvpcarsNextToken = a})

-- | -- | The response status code.
dlgrtvpcarsResponseStatus :: Lens' DescribeLocalGatewayRouteTableVPCAssociationsResponse Int
dlgrtvpcarsResponseStatus = lens _dlgrtvpcarsResponseStatus (\s a -> s {_dlgrtvpcarsResponseStatus = a})

instance
  NFData
    DescribeLocalGatewayRouteTableVPCAssociationsResponse
