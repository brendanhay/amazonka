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
-- Module      : Network.AWS.EC2.DescribeTransitGatewayVPCAttachments
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more VPC attachments. By default, all VPC attachments are described. Alternatively, you can filter the results.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGatewayVPCAttachments
    (
    -- * Creating a Request
      describeTransitGatewayVPCAttachments
    , DescribeTransitGatewayVPCAttachments
    -- * Request Lenses
    , dtgvpcaFilters
    , dtgvpcaNextToken
    , dtgvpcaTransitGatewayAttachmentIds
    , dtgvpcaDryRun
    , dtgvpcaMaxResults

    -- * Destructuring the Response
    , describeTransitGatewayVPCAttachmentsResponse
    , DescribeTransitGatewayVPCAttachmentsResponse
    -- * Response Lenses
    , dtgvarsTransitGatewayVPCAttachments
    , dtgvarsNextToken
    , dtgvarsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTransitGatewayVPCAttachments' smart constructor.
data DescribeTransitGatewayVPCAttachments = DescribeTransitGatewayVPCAttachments'
  { _dtgvpcaFilters                     :: !(Maybe [Filter])
  , _dtgvpcaNextToken                   :: !(Maybe Text)
  , _dtgvpcaTransitGatewayAttachmentIds :: !(Maybe [Text])
  , _dtgvpcaDryRun                      :: !(Maybe Bool)
  , _dtgvpcaMaxResults                  :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGatewayVPCAttachments' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgvpcaFilters' - One or more filters. The possible values are:     * @state@ - The state of the attachment (@available@ | @deleted@ | @deleting@ | @failed@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @transit-gateway-attachment-id@ - The ID of the attachment.     * @transit-gateway-id@ - The ID of the transit gateway.     * @vpc-id@ - The ID of the VPC.
--
-- * 'dtgvpcaNextToken' - The token for the next page of results.
--
-- * 'dtgvpcaTransitGatewayAttachmentIds' - The IDs of the attachments.
--
-- * 'dtgvpcaDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtgvpcaMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTransitGatewayVPCAttachments
    :: DescribeTransitGatewayVPCAttachments
describeTransitGatewayVPCAttachments =
  DescribeTransitGatewayVPCAttachments'
    { _dtgvpcaFilters = Nothing
    , _dtgvpcaNextToken = Nothing
    , _dtgvpcaTransitGatewayAttachmentIds = Nothing
    , _dtgvpcaDryRun = Nothing
    , _dtgvpcaMaxResults = Nothing
    }


-- | One or more filters. The possible values are:     * @state@ - The state of the attachment (@available@ | @deleted@ | @deleting@ | @failed@ | @modifying@ | @pendingAcceptance@ | @pending@ | @rollingBack@ | @rejected@ | @rejecting@ ).     * @transit-gateway-attachment-id@ - The ID of the attachment.     * @transit-gateway-id@ - The ID of the transit gateway.     * @vpc-id@ - The ID of the VPC.
dtgvpcaFilters :: Lens' DescribeTransitGatewayVPCAttachments [Filter]
dtgvpcaFilters = lens _dtgvpcaFilters (\ s a -> s{_dtgvpcaFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtgvpcaNextToken :: Lens' DescribeTransitGatewayVPCAttachments (Maybe Text)
dtgvpcaNextToken = lens _dtgvpcaNextToken (\ s a -> s{_dtgvpcaNextToken = a})

-- | The IDs of the attachments.
dtgvpcaTransitGatewayAttachmentIds :: Lens' DescribeTransitGatewayVPCAttachments [Text]
dtgvpcaTransitGatewayAttachmentIds = lens _dtgvpcaTransitGatewayAttachmentIds (\ s a -> s{_dtgvpcaTransitGatewayAttachmentIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtgvpcaDryRun :: Lens' DescribeTransitGatewayVPCAttachments (Maybe Bool)
dtgvpcaDryRun = lens _dtgvpcaDryRun (\ s a -> s{_dtgvpcaDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtgvpcaMaxResults :: Lens' DescribeTransitGatewayVPCAttachments (Maybe Natural)
dtgvpcaMaxResults = lens _dtgvpcaMaxResults (\ s a -> s{_dtgvpcaMaxResults = a}) . mapping _Nat

instance AWSPager
           DescribeTransitGatewayVPCAttachments
         where
        page rq rs
          | stop (rs ^. dtgvarsNextToken) = Nothing
          | stop (rs ^. dtgvarsTransitGatewayVPCAttachments) =
            Nothing
          | otherwise =
            Just $ rq &
              dtgvpcaNextToken .~ rs ^. dtgvarsNextToken

instance AWSRequest
           DescribeTransitGatewayVPCAttachments
         where
        type Rs DescribeTransitGatewayVPCAttachments =
             DescribeTransitGatewayVPCAttachmentsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeTransitGatewayVPCAttachmentsResponse' <$>
                   (x .@? "transitGatewayVpcAttachments" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeTransitGatewayVPCAttachments
         where

instance NFData DescribeTransitGatewayVPCAttachments
         where

instance ToHeaders
           DescribeTransitGatewayVPCAttachments
         where
        toHeaders = const mempty

instance ToPath DescribeTransitGatewayVPCAttachments
         where
        toPath = const "/"

instance ToQuery DescribeTransitGatewayVPCAttachments
         where
        toQuery DescribeTransitGatewayVPCAttachments'{..}
          = mconcat
              ["Action" =:
                 ("DescribeTransitGatewayVpcAttachments" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dtgvpcaFilters),
               "NextToken" =: _dtgvpcaNextToken,
               toQuery
                 (toQueryList "TransitGatewayAttachmentIds" <$>
                    _dtgvpcaTransitGatewayAttachmentIds),
               "DryRun" =: _dtgvpcaDryRun,
               "MaxResults" =: _dtgvpcaMaxResults]

-- | /See:/ 'describeTransitGatewayVPCAttachmentsResponse' smart constructor.
data DescribeTransitGatewayVPCAttachmentsResponse = DescribeTransitGatewayVPCAttachmentsResponse'
  { _dtgvarsTransitGatewayVPCAttachments :: !(Maybe [TransitGatewayVPCAttachment])
  , _dtgvarsNextToken :: !(Maybe Text)
  , _dtgvarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTransitGatewayVPCAttachmentsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtgvarsTransitGatewayVPCAttachments' - Information about the VPC attachments.
--
-- * 'dtgvarsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dtgvarsResponseStatus' - -- | The response status code.
describeTransitGatewayVPCAttachmentsResponse
    :: Int -- ^ 'dtgvarsResponseStatus'
    -> DescribeTransitGatewayVPCAttachmentsResponse
describeTransitGatewayVPCAttachmentsResponse pResponseStatus_ =
  DescribeTransitGatewayVPCAttachmentsResponse'
    { _dtgvarsTransitGatewayVPCAttachments = Nothing
    , _dtgvarsNextToken = Nothing
    , _dtgvarsResponseStatus = pResponseStatus_
    }


-- | Information about the VPC attachments.
dtgvarsTransitGatewayVPCAttachments :: Lens' DescribeTransitGatewayVPCAttachmentsResponse [TransitGatewayVPCAttachment]
dtgvarsTransitGatewayVPCAttachments = lens _dtgvarsTransitGatewayVPCAttachments (\ s a -> s{_dtgvarsTransitGatewayVPCAttachments = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dtgvarsNextToken :: Lens' DescribeTransitGatewayVPCAttachmentsResponse (Maybe Text)
dtgvarsNextToken = lens _dtgvarsNextToken (\ s a -> s{_dtgvarsNextToken = a})

-- | -- | The response status code.
dtgvarsResponseStatus :: Lens' DescribeTransitGatewayVPCAttachmentsResponse Int
dtgvarsResponseStatus = lens _dtgvarsResponseStatus (\ s a -> s{_dtgvarsResponseStatus = a})

instance NFData
           DescribeTransitGatewayVPCAttachmentsResponse
         where
