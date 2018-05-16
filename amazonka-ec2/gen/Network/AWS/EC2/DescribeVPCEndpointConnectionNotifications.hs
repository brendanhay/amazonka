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
-- Module      : Network.AWS.EC2.DescribeVPCEndpointConnectionNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the connection notifications for VPC endpoints and VPC endpoint services.
--
--
module Network.AWS.EC2.DescribeVPCEndpointConnectionNotifications
    (
    -- * Creating a Request
      describeVPCEndpointConnectionNotifications
    , DescribeVPCEndpointConnectionNotifications
    -- * Request Lenses
    , dvpcecnFilters
    , dvpcecnNextToken
    , dvpcecnConnectionNotificationId
    , dvpcecnDryRun
    , dvpcecnMaxResults

    -- * Destructuring the Response
    , describeVPCEndpointConnectionNotificationsResponse
    , DescribeVPCEndpointConnectionNotificationsResponse
    -- * Response Lenses
    , dvpcecnrsConnectionNotificationSet
    , dvpcecnrsNextToken
    , dvpcecnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVPCEndpointConnectionNotifications' smart constructor.
data DescribeVPCEndpointConnectionNotifications = DescribeVPCEndpointConnectionNotifications'
  { _dvpcecnFilters                  :: !(Maybe [Filter])
  , _dvpcecnNextToken                :: !(Maybe Text)
  , _dvpcecnConnectionNotificationId :: !(Maybe Text)
  , _dvpcecnDryRun                   :: !(Maybe Bool)
  , _dvpcecnMaxResults               :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointConnectionNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcecnFilters' - One or more filters.     * @connection-notification-arn@ - The ARN of SNS topic for the notification.     * @connection-notification-id@ - The ID of the notification.     * @connection-notification-state@ - The state of the notification (@Enabled@ | @Disabled@ ).     * @connection-notification-type@ - The type of notification (@Topic@ ).     * @service-id@ - The ID of the endpoint service.     * @vpc-endpoint-id@ - The ID of the VPC endpoint.
--
-- * 'dvpcecnNextToken' - The token to request the next page of results.
--
-- * 'dvpcecnConnectionNotificationId' - The ID of the notification.
--
-- * 'dvpcecnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvpcecnMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value.
describeVPCEndpointConnectionNotifications
    :: DescribeVPCEndpointConnectionNotifications
describeVPCEndpointConnectionNotifications =
  DescribeVPCEndpointConnectionNotifications'
    { _dvpcecnFilters = Nothing
    , _dvpcecnNextToken = Nothing
    , _dvpcecnConnectionNotificationId = Nothing
    , _dvpcecnDryRun = Nothing
    , _dvpcecnMaxResults = Nothing
    }


-- | One or more filters.     * @connection-notification-arn@ - The ARN of SNS topic for the notification.     * @connection-notification-id@ - The ID of the notification.     * @connection-notification-state@ - The state of the notification (@Enabled@ | @Disabled@ ).     * @connection-notification-type@ - The type of notification (@Topic@ ).     * @service-id@ - The ID of the endpoint service.     * @vpc-endpoint-id@ - The ID of the VPC endpoint.
dvpcecnFilters :: Lens' DescribeVPCEndpointConnectionNotifications [Filter]
dvpcecnFilters = lens _dvpcecnFilters (\ s a -> s{_dvpcecnFilters = a}) . _Default . _Coerce

-- | The token to request the next page of results.
dvpcecnNextToken :: Lens' DescribeVPCEndpointConnectionNotifications (Maybe Text)
dvpcecnNextToken = lens _dvpcecnNextToken (\ s a -> s{_dvpcecnNextToken = a})

-- | The ID of the notification.
dvpcecnConnectionNotificationId :: Lens' DescribeVPCEndpointConnectionNotifications (Maybe Text)
dvpcecnConnectionNotificationId = lens _dvpcecnConnectionNotificationId (\ s a -> s{_dvpcecnConnectionNotificationId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvpcecnDryRun :: Lens' DescribeVPCEndpointConnectionNotifications (Maybe Bool)
dvpcecnDryRun = lens _dvpcecnDryRun (\ s a -> s{_dvpcecnDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another request with the returned @NextToken@ value.
dvpcecnMaxResults :: Lens' DescribeVPCEndpointConnectionNotifications (Maybe Int)
dvpcecnMaxResults = lens _dvpcecnMaxResults (\ s a -> s{_dvpcecnMaxResults = a})

instance AWSRequest
           DescribeVPCEndpointConnectionNotifications
         where
        type Rs DescribeVPCEndpointConnectionNotifications =
             DescribeVPCEndpointConnectionNotificationsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeVPCEndpointConnectionNotificationsResponse'
                   <$>
                   (x .@? "connectionNotificationSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeVPCEndpointConnectionNotifications
         where

instance NFData
           DescribeVPCEndpointConnectionNotifications
         where

instance ToHeaders
           DescribeVPCEndpointConnectionNotifications
         where
        toHeaders = const mempty

instance ToPath
           DescribeVPCEndpointConnectionNotifications
         where
        toPath = const "/"

instance ToQuery
           DescribeVPCEndpointConnectionNotifications
         where
        toQuery
          DescribeVPCEndpointConnectionNotifications'{..}
          = mconcat
              ["Action" =:
                 ("DescribeVpcEndpointConnectionNotifications" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery (toQueryList "Filter" <$> _dvpcecnFilters),
               "NextToken" =: _dvpcecnNextToken,
               "ConnectionNotificationId" =:
                 _dvpcecnConnectionNotificationId,
               "DryRun" =: _dvpcecnDryRun,
               "MaxResults" =: _dvpcecnMaxResults]

-- | /See:/ 'describeVPCEndpointConnectionNotificationsResponse' smart constructor.
data DescribeVPCEndpointConnectionNotificationsResponse = DescribeVPCEndpointConnectionNotificationsResponse'
  { _dvpcecnrsConnectionNotificationSet :: !(Maybe [ConnectionNotification])
  , _dvpcecnrsNextToken                 :: !(Maybe Text)
  , _dvpcecnrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVPCEndpointConnectionNotificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvpcecnrsConnectionNotificationSet' - One or more notifications.
--
-- * 'dvpcecnrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dvpcecnrsResponseStatus' - -- | The response status code.
describeVPCEndpointConnectionNotificationsResponse
    :: Int -- ^ 'dvpcecnrsResponseStatus'
    -> DescribeVPCEndpointConnectionNotificationsResponse
describeVPCEndpointConnectionNotificationsResponse pResponseStatus_ =
  DescribeVPCEndpointConnectionNotificationsResponse'
    { _dvpcecnrsConnectionNotificationSet = Nothing
    , _dvpcecnrsNextToken = Nothing
    , _dvpcecnrsResponseStatus = pResponseStatus_
    }


-- | One or more notifications.
dvpcecnrsConnectionNotificationSet :: Lens' DescribeVPCEndpointConnectionNotificationsResponse [ConnectionNotification]
dvpcecnrsConnectionNotificationSet = lens _dvpcecnrsConnectionNotificationSet (\ s a -> s{_dvpcecnrsConnectionNotificationSet = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dvpcecnrsNextToken :: Lens' DescribeVPCEndpointConnectionNotificationsResponse (Maybe Text)
dvpcecnrsNextToken = lens _dvpcecnrsNextToken (\ s a -> s{_dvpcecnrsNextToken = a})

-- | -- | The response status code.
dvpcecnrsResponseStatus :: Lens' DescribeVPCEndpointConnectionNotificationsResponse Int
dvpcecnrsResponseStatus = lens _dvpcecnrsResponseStatus (\ s a -> s{_dvpcecnrsResponseStatus = a})

instance NFData
           DescribeVPCEndpointConnectionNotificationsResponse
         where
