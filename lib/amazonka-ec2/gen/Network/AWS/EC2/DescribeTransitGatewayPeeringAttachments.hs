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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your transit gateway peering attachments.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayPeeringAttachments
  ( -- * Creating a Request
    describeTransitGatewayPeeringAttachments,
    DescribeTransitGatewayPeeringAttachments,

    -- * Request Lenses
    dtgpaFilters,
    dtgpaNextToken,
    dtgpaTransitGatewayAttachmentIds,
    dtgpaDryRun,
    dtgpaMaxResults,

    -- * Destructuring the Response
    describeTransitGatewayPeeringAttachmentsResponse,
    DescribeTransitGatewayPeeringAttachmentsResponse,

    -- * Response Lenses
    dtgpasrsTransitGatewayPeeringAttachments,
    dtgpasrsNextToken,
    dtgpasrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGatewayPeeringAttachments' smart constructor.
data DescribeTransitGatewayPeeringAttachments = DescribeTransitGatewayPeeringAttachments'
  { _dtgpaFilters ::
      !( Maybe
           [Filter]
       ),
    _dtgpaNextToken ::
      !( Maybe
           Text
       ),
    _dtgpaTransitGatewayAttachmentIds ::
      !( Maybe
           [Text]
       ),
    _dtgpaDryRun ::
      !( Maybe
           Bool
       ),
    _dtgpaMaxResults ::
      !( Maybe
           Nat
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTransitGatewayPeeringAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgpaFilters' - One or more filters. The possible values are:     * @transit-gateway-attachment-id@ - The ID of the transit gateway attachment.     * @local-owner-id@ - The ID of your AWS account.     * @remote-owner-id@ - The ID of the AWS account in the remote Region that owns the transit gateway.     * @state@ - The state of the peering attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.     * @transit-gateway-id@ - The ID of the transit gateway.
--
-- * 'dtgpaNextToken' - The token for the next page of results.
--
-- * 'dtgpaTransitGatewayAttachmentIds' - One or more IDs of the transit gateway peering attachments.
--
-- * 'dtgpaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgpaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGatewayPeeringAttachments ::
  DescribeTransitGatewayPeeringAttachments
describeTransitGatewayPeeringAttachments =
  DescribeTransitGatewayPeeringAttachments'
    { _dtgpaFilters =
        Nothing,
      _dtgpaNextToken = Nothing,
      _dtgpaTransitGatewayAttachmentIds = Nothing,
      _dtgpaDryRun = Nothing,
      _dtgpaMaxResults = Nothing
    }

-- | One or more filters. The possible values are:     * @transit-gateway-attachment-id@ - The ID of the transit gateway attachment.     * @local-owner-id@ - The ID of your AWS account.     * @remote-owner-id@ - The ID of the AWS account in the remote Region that owns the transit gateway.     * @state@ - The state of the peering attachment. Valid values are @available@ | @deleted@ | @deleting@ | @failed@ | @failing@ | @initiatingRequest@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources that have a tag with a specific key, regardless of the tag value.     * @transit-gateway-id@ - The ID of the transit gateway.
dtgpaFilters :: Lens' DescribeTransitGatewayPeeringAttachments [Filter]
dtgpaFilters = lens _dtgpaFilters (\s a -> s {_dtgpaFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgpaNextToken :: Lens' DescribeTransitGatewayPeeringAttachments (Maybe Text)
dtgpaNextToken = lens _dtgpaNextToken (\s a -> s {_dtgpaNextToken = a})

-- | One or more IDs of the transit gateway peering attachments.
dtgpaTransitGatewayAttachmentIds :: Lens' DescribeTransitGatewayPeeringAttachments [Text]
dtgpaTransitGatewayAttachmentIds = lens _dtgpaTransitGatewayAttachmentIds (\s a -> s {_dtgpaTransitGatewayAttachmentIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgpaDryRun :: Lens' DescribeTransitGatewayPeeringAttachments (Maybe Bool)
dtgpaDryRun = lens _dtgpaDryRun (\s a -> s {_dtgpaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgpaMaxResults :: Lens' DescribeTransitGatewayPeeringAttachments (Maybe Natural)
dtgpaMaxResults = lens _dtgpaMaxResults (\s a -> s {_dtgpaMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTransitGatewayPeeringAttachments where
  page rq rs
    | stop (rs ^. dtgpasrsNextToken) = Nothing
    | stop (rs ^. dtgpasrsTransitGatewayPeeringAttachments) = Nothing
    | otherwise = Just $ rq & dtgpaNextToken .~ rs ^. dtgpasrsNextToken

instance AWSRequest DescribeTransitGatewayPeeringAttachments where
  type
    Rs DescribeTransitGatewayPeeringAttachments =
      DescribeTransitGatewayPeeringAttachmentsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeTransitGatewayPeeringAttachmentsResponse'
            <$> ( x .@? "transitGatewayPeeringAttachments" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTransitGatewayPeeringAttachments

instance NFData DescribeTransitGatewayPeeringAttachments

instance ToHeaders DescribeTransitGatewayPeeringAttachments where
  toHeaders = const mempty

instance ToPath DescribeTransitGatewayPeeringAttachments where
  toPath = const "/"

instance ToQuery DescribeTransitGatewayPeeringAttachments where
  toQuery DescribeTransitGatewayPeeringAttachments' {..} =
    mconcat
      [ "Action"
          =: ("DescribeTransitGatewayPeeringAttachments" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dtgpaFilters),
        "NextToken" =: _dtgpaNextToken,
        toQuery
          ( toQueryList "TransitGatewayAttachmentIds"
              <$> _dtgpaTransitGatewayAttachmentIds
          ),
        "DryRun" =: _dtgpaDryRun,
        "MaxResults" =: _dtgpaMaxResults
      ]

-- | /See:/ 'describeTransitGatewayPeeringAttachmentsResponse' smart constructor.
data DescribeTransitGatewayPeeringAttachmentsResponse = DescribeTransitGatewayPeeringAttachmentsResponse'
  { _dtgpasrsTransitGatewayPeeringAttachments ::
      !( Maybe
           [TransitGatewayPeeringAttachment]
       ),
    _dtgpasrsNextToken ::
      !( Maybe
           Text
       ),
    _dtgpasrsResponseStatus ::
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

-- | Creates a value of 'DescribeTransitGatewayPeeringAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgpasrsTransitGatewayPeeringAttachments' - The transit gateway peering attachments.
--
-- * 'dtgpasrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgpasrsResponseStatus' - -- | The response status code.
describeTransitGatewayPeeringAttachmentsResponse ::
  -- | 'dtgpasrsResponseStatus'
  Int ->
  DescribeTransitGatewayPeeringAttachmentsResponse
describeTransitGatewayPeeringAttachmentsResponse pResponseStatus_ =
  DescribeTransitGatewayPeeringAttachmentsResponse'
    { _dtgpasrsTransitGatewayPeeringAttachments =
        Nothing,
      _dtgpasrsNextToken = Nothing,
      _dtgpasrsResponseStatus = pResponseStatus_
    }

-- | The transit gateway peering attachments.
dtgpasrsTransitGatewayPeeringAttachments :: Lens' DescribeTransitGatewayPeeringAttachmentsResponse [TransitGatewayPeeringAttachment]
dtgpasrsTransitGatewayPeeringAttachments = lens _dtgpasrsTransitGatewayPeeringAttachments (\s a -> s {_dtgpasrsTransitGatewayPeeringAttachments = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgpasrsNextToken :: Lens' DescribeTransitGatewayPeeringAttachmentsResponse (Maybe Text)
dtgpasrsNextToken = lens _dtgpasrsNextToken (\s a -> s {_dtgpasrsNextToken = a})

-- | -- | The response status code.
dtgpasrsResponseStatus :: Lens' DescribeTransitGatewayPeeringAttachmentsResponse Int
dtgpasrsResponseStatus = lens _dtgpasrsResponseStatus (\s a -> s {_dtgpasrsResponseStatus = a})

instance NFData DescribeTransitGatewayPeeringAttachmentsResponse
