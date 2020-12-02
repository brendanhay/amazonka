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
-- Module      : Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the transit gateway multicast domain.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
  ( -- * Creating a Request
    getTransitGatewayMulticastDomainAssociations,
    GetTransitGatewayMulticastDomainAssociations,

    -- * Request Lenses
    gtgmdaFilters,
    gtgmdaTransitGatewayMulticastDomainId,
    gtgmdaNextToken,
    gtgmdaDryRun,
    gtgmdaMaxResults,

    -- * Destructuring the Response
    getTransitGatewayMulticastDomainAssociationsResponse,
    GetTransitGatewayMulticastDomainAssociationsResponse,

    -- * Response Lenses
    gtgmdarsNextToken,
    gtgmdarsMulticastDomainAssociations,
    gtgmdarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getTransitGatewayMulticastDomainAssociations' smart constructor.
data GetTransitGatewayMulticastDomainAssociations = GetTransitGatewayMulticastDomainAssociations'
  { _gtgmdaFilters ::
      !( Maybe
           [Filter]
       ),
    _gtgmdaTransitGatewayMulticastDomainId ::
      !( Maybe
           Text
       ),
    _gtgmdaNextToken ::
      !( Maybe
           Text
       ),
    _gtgmdaDryRun ::
      !( Maybe
           Bool
       ),
    _gtgmdaMaxResults ::
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

-- | Creates a value of 'GetTransitGatewayMulticastDomainAssociations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgmdaFilters' - One or more filters. The possible values are:     * @resource-id@ - The ID of the resource.     * @resource-type@ - The type of resource. The valid value is: @vpc@ .     * @state@ - The state of the subnet association. Valid values are @associated@ | @associating@ | @disassociated@ | @disassociating@ .     * @subnet-id@ - The ID of the subnet.     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
--
-- * 'gtgmdaTransitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- * 'gtgmdaNextToken' - The token for the next page of results.
--
-- * 'gtgmdaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'gtgmdaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
getTransitGatewayMulticastDomainAssociations ::
  GetTransitGatewayMulticastDomainAssociations
getTransitGatewayMulticastDomainAssociations =
  GetTransitGatewayMulticastDomainAssociations'
    { _gtgmdaFilters =
        Nothing,
      _gtgmdaTransitGatewayMulticastDomainId = Nothing,
      _gtgmdaNextToken = Nothing,
      _gtgmdaDryRun = Nothing,
      _gtgmdaMaxResults = Nothing
    }

-- | One or more filters. The possible values are:     * @resource-id@ - The ID of the resource.     * @resource-type@ - The type of resource. The valid value is: @vpc@ .     * @state@ - The state of the subnet association. Valid values are @associated@ | @associating@ | @disassociated@ | @disassociating@ .     * @subnet-id@ - The ID of the subnet.     * @transit-gateway-attachment-id@ - The id of the transit gateway attachment.
gtgmdaFilters :: Lens' GetTransitGatewayMulticastDomainAssociations [Filter]
gtgmdaFilters = lens _gtgmdaFilters (\s a -> s {_gtgmdaFilters = a}) . _Default . _Coerce

-- | The ID of the transit gateway multicast domain.
gtgmdaTransitGatewayMulticastDomainId :: Lens' GetTransitGatewayMulticastDomainAssociations (Maybe Text)
gtgmdaTransitGatewayMulticastDomainId = lens _gtgmdaTransitGatewayMulticastDomainId (\s a -> s {_gtgmdaTransitGatewayMulticastDomainId = a})

-- | The token for the next page of results.
gtgmdaNextToken :: Lens' GetTransitGatewayMulticastDomainAssociations (Maybe Text)
gtgmdaNextToken = lens _gtgmdaNextToken (\s a -> s {_gtgmdaNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
gtgmdaDryRun :: Lens' GetTransitGatewayMulticastDomainAssociations (Maybe Bool)
gtgmdaDryRun = lens _gtgmdaDryRun (\s a -> s {_gtgmdaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
gtgmdaMaxResults :: Lens' GetTransitGatewayMulticastDomainAssociations (Maybe Natural)
gtgmdaMaxResults = lens _gtgmdaMaxResults (\s a -> s {_gtgmdaMaxResults = a}) . mapping _Nat

instance AWSPager GetTransitGatewayMulticastDomainAssociations where
  page rq rs
    | stop (rs ^. gtgmdarsNextToken) = Nothing
    | stop (rs ^. gtgmdarsMulticastDomainAssociations) = Nothing
    | otherwise =
      Just $ rq & gtgmdaNextToken .~ rs ^. gtgmdarsNextToken

instance AWSRequest GetTransitGatewayMulticastDomainAssociations where
  type
    Rs GetTransitGatewayMulticastDomainAssociations =
      GetTransitGatewayMulticastDomainAssociationsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetTransitGatewayMulticastDomainAssociationsResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "multicastDomainAssociations" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable GetTransitGatewayMulticastDomainAssociations

instance NFData GetTransitGatewayMulticastDomainAssociations

instance ToHeaders GetTransitGatewayMulticastDomainAssociations where
  toHeaders = const mempty

instance ToPath GetTransitGatewayMulticastDomainAssociations where
  toPath = const "/"

instance ToQuery GetTransitGatewayMulticastDomainAssociations where
  toQuery GetTransitGatewayMulticastDomainAssociations' {..} =
    mconcat
      [ "Action"
          =: ("GetTransitGatewayMulticastDomainAssociations" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _gtgmdaFilters),
        "TransitGatewayMulticastDomainId"
          =: _gtgmdaTransitGatewayMulticastDomainId,
        "NextToken" =: _gtgmdaNextToken,
        "DryRun" =: _gtgmdaDryRun,
        "MaxResults" =: _gtgmdaMaxResults
      ]

-- | /See:/ 'getTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data GetTransitGatewayMulticastDomainAssociationsResponse = GetTransitGatewayMulticastDomainAssociationsResponse'
  { _gtgmdarsNextToken ::
      !( Maybe
           Text
       ),
    _gtgmdarsMulticastDomainAssociations ::
      !( Maybe
           [TransitGatewayMulticastDomainAssociation]
       ),
    _gtgmdarsResponseStatus ::
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

-- | Creates a value of 'GetTransitGatewayMulticastDomainAssociationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gtgmdarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'gtgmdarsMulticastDomainAssociations' - Information about the multicast domain associations.
--
-- * 'gtgmdarsResponseStatus' - -- | The response status code.
getTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'gtgmdarsResponseStatus'
  Int ->
  GetTransitGatewayMulticastDomainAssociationsResponse
getTransitGatewayMulticastDomainAssociationsResponse
  pResponseStatus_ =
    GetTransitGatewayMulticastDomainAssociationsResponse'
      { _gtgmdarsNextToken =
          Nothing,
        _gtgmdarsMulticastDomainAssociations =
          Nothing,
        _gtgmdarsResponseStatus =
          pResponseStatus_
      }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
gtgmdarsNextToken :: Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Maybe Text)
gtgmdarsNextToken = lens _gtgmdarsNextToken (\s a -> s {_gtgmdarsNextToken = a})

-- | Information about the multicast domain associations.
gtgmdarsMulticastDomainAssociations :: Lens' GetTransitGatewayMulticastDomainAssociationsResponse [TransitGatewayMulticastDomainAssociation]
gtgmdarsMulticastDomainAssociations = lens _gtgmdarsMulticastDomainAssociations (\s a -> s {_gtgmdarsMulticastDomainAssociations = a}) . _Default . _Coerce

-- | -- | The response status code.
gtgmdarsResponseStatus :: Lens' GetTransitGatewayMulticastDomainAssociationsResponse Int
gtgmdarsResponseStatus = lens _gtgmdarsResponseStatus (\s a -> s {_gtgmdarsResponseStatus = a})

instance
  NFData
    GetTransitGatewayMulticastDomainAssociationsResponse
