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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more attachments between resources and transit gateways. By default, all attachments are described. Alternatively, you can filter the results by attachment ID, attachment state, resource ID, or resource owner.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayAttachments
  ( -- * Creating a Request
    describeTransitGatewayAttachments,
    DescribeTransitGatewayAttachments,

    -- * Request Lenses
    dtgaFilters,
    dtgaNextToken,
    dtgaTransitGatewayAttachmentIds,
    dtgaDryRun,
    dtgaMaxResults,

    -- * Destructuring the Response
    describeTransitGatewayAttachmentsResponse,
    DescribeTransitGatewayAttachmentsResponse,

    -- * Response Lenses
    dtgarsNextToken,
    dtgarsTransitGatewayAttachments,
    dtgarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGatewayAttachments' smart constructor.
data DescribeTransitGatewayAttachments = DescribeTransitGatewayAttachments'
  { _dtgaFilters ::
      !(Maybe [Filter]),
    _dtgaNextToken ::
      !(Maybe Text),
    _dtgaTransitGatewayAttachmentIds ::
      !(Maybe [Text]),
    _dtgaDryRun ::
      !(Maybe Bool),
    _dtgaMaxResults ::
      !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTransitGatewayAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgaFilters' - One or more filters. The possible values are:     * @association.state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).     * @association.transit-gateway-route-table-id@ - The ID of the route table for the transit gateway.     * @resource-id@ - The ID of the resource.     * @resource-owner-id@ - The ID of the AWS account that owns the resource.     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .     * @state@ - The state of the attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ .     * @transit-gateway-attachment-id@ - The ID of the attachment.     * @transit-gateway-id@ - The ID of the transit gateway.     * @transit-gateway-owner-id@ - The ID of the AWS account that owns the transit gateway.
--
-- * 'dtgaNextToken' - The token for the next page of results.
--
-- * 'dtgaTransitGatewayAttachmentIds' - The IDs of the attachments.
--
-- * 'dtgaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGatewayAttachments ::
  DescribeTransitGatewayAttachments
describeTransitGatewayAttachments =
  DescribeTransitGatewayAttachments'
    { _dtgaFilters = Nothing,
      _dtgaNextToken = Nothing,
      _dtgaTransitGatewayAttachmentIds = Nothing,
      _dtgaDryRun = Nothing,
      _dtgaMaxResults = Nothing
    }

-- | One or more filters. The possible values are:     * @association.state@ - The state of the association (@associating@ | @associated@ | @disassociating@ ).     * @association.transit-gateway-route-table-id@ - The ID of the route table for the transit gateway.     * @resource-id@ - The ID of the resource.     * @resource-owner-id@ - The ID of the AWS account that owns the resource.     * @resource-type@ - The resource type. Valid values are @vpc@ | @vpn@ | @direct-connect-gateway@ | @peering@ .     * @state@ - The state of the attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ .     * @transit-gateway-attachment-id@ - The ID of the attachment.     * @transit-gateway-id@ - The ID of the transit gateway.     * @transit-gateway-owner-id@ - The ID of the AWS account that owns the transit gateway.
dtgaFilters :: Lens' DescribeTransitGatewayAttachments [Filter]
dtgaFilters = lens _dtgaFilters (\s a -> s {_dtgaFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgaNextToken :: Lens' DescribeTransitGatewayAttachments (Maybe Text)
dtgaNextToken = lens _dtgaNextToken (\s a -> s {_dtgaNextToken = a})

-- | The IDs of the attachments.
dtgaTransitGatewayAttachmentIds :: Lens' DescribeTransitGatewayAttachments [Text]
dtgaTransitGatewayAttachmentIds = lens _dtgaTransitGatewayAttachmentIds (\s a -> s {_dtgaTransitGatewayAttachmentIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgaDryRun :: Lens' DescribeTransitGatewayAttachments (Maybe Bool)
dtgaDryRun = lens _dtgaDryRun (\s a -> s {_dtgaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgaMaxResults :: Lens' DescribeTransitGatewayAttachments (Maybe Natural)
dtgaMaxResults = lens _dtgaMaxResults (\s a -> s {_dtgaMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTransitGatewayAttachments where
  page rq rs
    | stop (rs ^. dtgarsNextToken) = Nothing
    | stop (rs ^. dtgarsTransitGatewayAttachments) = Nothing
    | otherwise = Just $ rq & dtgaNextToken .~ rs ^. dtgarsNextToken

instance AWSRequest DescribeTransitGatewayAttachments where
  type
    Rs DescribeTransitGatewayAttachments =
      DescribeTransitGatewayAttachmentsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeTransitGatewayAttachmentsResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "transitGatewayAttachments" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTransitGatewayAttachments

instance NFData DescribeTransitGatewayAttachments

instance ToHeaders DescribeTransitGatewayAttachments where
  toHeaders = const mempty

instance ToPath DescribeTransitGatewayAttachments where
  toPath = const "/"

instance ToQuery DescribeTransitGatewayAttachments where
  toQuery DescribeTransitGatewayAttachments' {..} =
    mconcat
      [ "Action" =: ("DescribeTransitGatewayAttachments" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dtgaFilters),
        "NextToken" =: _dtgaNextToken,
        toQuery
          ( toQueryList "TransitGatewayAttachmentIds"
              <$> _dtgaTransitGatewayAttachmentIds
          ),
        "DryRun" =: _dtgaDryRun,
        "MaxResults" =: _dtgaMaxResults
      ]

-- | /See:/ 'describeTransitGatewayAttachmentsResponse' smart constructor.
data DescribeTransitGatewayAttachmentsResponse = DescribeTransitGatewayAttachmentsResponse'
  { _dtgarsNextToken ::
      !( Maybe
           Text
       ),
    _dtgarsTransitGatewayAttachments ::
      !( Maybe
           [TransitGatewayAttachment]
       ),
    _dtgarsResponseStatus ::
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

-- | Creates a value of 'DescribeTransitGatewayAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgarsTransitGatewayAttachments' - Information about the attachments.
--
-- * 'dtgarsResponseStatus' - -- | The response status code.
describeTransitGatewayAttachmentsResponse ::
  -- | 'dtgarsResponseStatus'
  Int ->
  DescribeTransitGatewayAttachmentsResponse
describeTransitGatewayAttachmentsResponse pResponseStatus_ =
  DescribeTransitGatewayAttachmentsResponse'
    { _dtgarsNextToken =
        Nothing,
      _dtgarsTransitGatewayAttachments = Nothing,
      _dtgarsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgarsNextToken :: Lens' DescribeTransitGatewayAttachmentsResponse (Maybe Text)
dtgarsNextToken = lens _dtgarsNextToken (\s a -> s {_dtgarsNextToken = a})

-- | Information about the attachments.
dtgarsTransitGatewayAttachments :: Lens' DescribeTransitGatewayAttachmentsResponse [TransitGatewayAttachment]
dtgarsTransitGatewayAttachments = lens _dtgarsTransitGatewayAttachments (\s a -> s {_dtgarsTransitGatewayAttachments = a}) . _Default . _Coerce

-- | -- | The response status code.
dtgarsResponseStatus :: Lens' DescribeTransitGatewayAttachmentsResponse Int
dtgarsResponseStatus = lens _dtgarsResponseStatus (\s a -> s {_dtgarsResponseStatus = a})

instance NFData DescribeTransitGatewayAttachmentsResponse
