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
-- Module      : Network.AWS.EC2.CreateVPCEndpointConnectionNotification
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a connection notification for a specified VPC endpoint or VPC endpoint service. A connection notification notifies you of specific endpoint events. You must create an SNS topic to receive notifications. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Create a Topic> in the /Amazon Simple Notification Service Developer Guide/ .
--
--
-- You can create a connection notification for interface endpoints only.
--
module Network.AWS.EC2.CreateVPCEndpointConnectionNotification
    (
    -- * Creating a Request
      createVPCEndpointConnectionNotification
    , CreateVPCEndpointConnectionNotification
    -- * Request Lenses
    , cvecnClientToken
    , cvecnServiceId
    , cvecnVPCEndpointId
    , cvecnDryRun
    , cvecnConnectionNotificationARN
    , cvecnConnectionEvents

    -- * Destructuring the Response
    , createVPCEndpointConnectionNotificationResponse
    , CreateVPCEndpointConnectionNotificationResponse
    -- * Response Lenses
    , cvecnrsClientToken
    , cvecnrsConnectionNotification
    , cvecnrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVPCEndpointConnectionNotification' smart constructor.
data CreateVPCEndpointConnectionNotification = CreateVPCEndpointConnectionNotification'
  { _cvecnClientToken               :: !(Maybe Text)
  , _cvecnServiceId                 :: !(Maybe Text)
  , _cvecnVPCEndpointId             :: !(Maybe Text)
  , _cvecnDryRun                    :: !(Maybe Bool)
  , _cvecnConnectionNotificationARN :: !Text
  , _cvecnConnectionEvents          :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCEndpointConnectionNotification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvecnClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
--
-- * 'cvecnServiceId' - The ID of the endpoint service.
--
-- * 'cvecnVPCEndpointId' - The ID of the endpoint.
--
-- * 'cvecnDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cvecnConnectionNotificationARN' - The ARN of the SNS topic for the notifications.
--
-- * 'cvecnConnectionEvents' - One or more endpoint events for which to receive notifications. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
createVPCEndpointConnectionNotification
    :: Text -- ^ 'cvecnConnectionNotificationARN'
    -> CreateVPCEndpointConnectionNotification
createVPCEndpointConnectionNotification pConnectionNotificationARN_ =
  CreateVPCEndpointConnectionNotification'
    { _cvecnClientToken = Nothing
    , _cvecnServiceId = Nothing
    , _cvecnVPCEndpointId = Nothing
    , _cvecnDryRun = Nothing
    , _cvecnConnectionNotificationARN = pConnectionNotificationARN_
    , _cvecnConnectionEvents = mempty
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html How to Ensure Idempotency> .
cvecnClientToken :: Lens' CreateVPCEndpointConnectionNotification (Maybe Text)
cvecnClientToken = lens _cvecnClientToken (\ s a -> s{_cvecnClientToken = a})

-- | The ID of the endpoint service.
cvecnServiceId :: Lens' CreateVPCEndpointConnectionNotification (Maybe Text)
cvecnServiceId = lens _cvecnServiceId (\ s a -> s{_cvecnServiceId = a})

-- | The ID of the endpoint.
cvecnVPCEndpointId :: Lens' CreateVPCEndpointConnectionNotification (Maybe Text)
cvecnVPCEndpointId = lens _cvecnVPCEndpointId (\ s a -> s{_cvecnVPCEndpointId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cvecnDryRun :: Lens' CreateVPCEndpointConnectionNotification (Maybe Bool)
cvecnDryRun = lens _cvecnDryRun (\ s a -> s{_cvecnDryRun = a})

-- | The ARN of the SNS topic for the notifications.
cvecnConnectionNotificationARN :: Lens' CreateVPCEndpointConnectionNotification Text
cvecnConnectionNotificationARN = lens _cvecnConnectionNotificationARN (\ s a -> s{_cvecnConnectionNotificationARN = a})

-- | One or more endpoint events for which to receive notifications. Valid values are @Accept@ , @Connect@ , @Delete@ , and @Reject@ .
cvecnConnectionEvents :: Lens' CreateVPCEndpointConnectionNotification [Text]
cvecnConnectionEvents = lens _cvecnConnectionEvents (\ s a -> s{_cvecnConnectionEvents = a}) . _Coerce

instance AWSRequest
           CreateVPCEndpointConnectionNotification
         where
        type Rs CreateVPCEndpointConnectionNotification =
             CreateVPCEndpointConnectionNotificationResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateVPCEndpointConnectionNotificationResponse' <$>
                   (x .@? "clientToken") <*>
                     (x .@? "connectionNotification")
                     <*> (pure (fromEnum s)))

instance Hashable
           CreateVPCEndpointConnectionNotification
         where

instance NFData
           CreateVPCEndpointConnectionNotification
         where

instance ToHeaders
           CreateVPCEndpointConnectionNotification
         where
        toHeaders = const mempty

instance ToPath
           CreateVPCEndpointConnectionNotification
         where
        toPath = const "/"

instance ToQuery
           CreateVPCEndpointConnectionNotification
         where
        toQuery CreateVPCEndpointConnectionNotification'{..}
          = mconcat
              ["Action" =:
                 ("CreateVpcEndpointConnectionNotification" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _cvecnClientToken,
               "ServiceId" =: _cvecnServiceId,
               "VpcEndpointId" =: _cvecnVPCEndpointId,
               "DryRun" =: _cvecnDryRun,
               "ConnectionNotificationArn" =:
                 _cvecnConnectionNotificationARN,
               toQueryList "ConnectionEvents"
                 _cvecnConnectionEvents]

-- | /See:/ 'createVPCEndpointConnectionNotificationResponse' smart constructor.
data CreateVPCEndpointConnectionNotificationResponse = CreateVPCEndpointConnectionNotificationResponse'
  { _cvecnrsClientToken            :: !(Maybe Text)
  , _cvecnrsConnectionNotification :: !(Maybe ConnectionNotification)
  , _cvecnrsResponseStatus         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVPCEndpointConnectionNotificationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvecnrsClientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
--
-- * 'cvecnrsConnectionNotification' - Information about the notification.
--
-- * 'cvecnrsResponseStatus' - -- | The response status code.
createVPCEndpointConnectionNotificationResponse
    :: Int -- ^ 'cvecnrsResponseStatus'
    -> CreateVPCEndpointConnectionNotificationResponse
createVPCEndpointConnectionNotificationResponse pResponseStatus_ =
  CreateVPCEndpointConnectionNotificationResponse'
    { _cvecnrsClientToken = Nothing
    , _cvecnrsConnectionNotification = Nothing
    , _cvecnrsResponseStatus = pResponseStatus_
    }


-- | Unique, case-sensitive identifier you provide to ensure the idempotency of the request.
cvecnrsClientToken :: Lens' CreateVPCEndpointConnectionNotificationResponse (Maybe Text)
cvecnrsClientToken = lens _cvecnrsClientToken (\ s a -> s{_cvecnrsClientToken = a})

-- | Information about the notification.
cvecnrsConnectionNotification :: Lens' CreateVPCEndpointConnectionNotificationResponse (Maybe ConnectionNotification)
cvecnrsConnectionNotification = lens _cvecnrsConnectionNotification (\ s a -> s{_cvecnrsConnectionNotification = a})

-- | -- | The response status code.
cvecnrsResponseStatus :: Lens' CreateVPCEndpointConnectionNotificationResponse Int
cvecnrsResponseStatus = lens _cvecnrsResponseStatus (\ s a -> s{_cvecnrsResponseStatus = a})

instance NFData
           CreateVPCEndpointConnectionNotificationResponse
         where
