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
-- Module      : Network.AWS.WorkDocs.CreateNotificationSubscription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configure WorkDocs to use Amazon SNS notifications.
--
--
-- The endpoint receives a confirmation message, and must confirm the subscription. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SendMessageToHttp.html#SendMessageToHttp.confirm Confirm the Subscription> in the /Amazon Simple Notification Service Developer Guide/ .
--
module Network.AWS.WorkDocs.CreateNotificationSubscription
    (
    -- * Creating a Request
      createNotificationSubscription
    , CreateNotificationSubscription
    -- * Request Lenses
    , cnsOrganizationId
    , cnsEndpoint
    , cnsProtocol
    , cnsSubscriptionType

    -- * Destructuring the Response
    , createNotificationSubscriptionResponse
    , CreateNotificationSubscriptionResponse
    -- * Response Lenses
    , cnsrsSubscription
    , cnsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkDocs.Types
import Network.AWS.WorkDocs.Types.Product

-- | /See:/ 'createNotificationSubscription' smart constructor.
data CreateNotificationSubscription = CreateNotificationSubscription'
  { _cnsOrganizationId   :: !Text
  , _cnsEndpoint         :: !Text
  , _cnsProtocol         :: !SubscriptionProtocolType
  , _cnsSubscriptionType :: !SubscriptionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotificationSubscription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnsOrganizationId' - The ID of the organization.
--
-- * 'cnsEndpoint' - The endpoint to receive the notifications. If the protocol is HTTPS, the endpoint is a URL that begins with "https://".
--
-- * 'cnsProtocol' - The protocol to use. The supported value is https, which delivers JSON-encoded messages using HTTPS POST.
--
-- * 'cnsSubscriptionType' - The notification type.
createNotificationSubscription
    :: Text -- ^ 'cnsOrganizationId'
    -> Text -- ^ 'cnsEndpoint'
    -> SubscriptionProtocolType -- ^ 'cnsProtocol'
    -> SubscriptionType -- ^ 'cnsSubscriptionType'
    -> CreateNotificationSubscription
createNotificationSubscription pOrganizationId_ pEndpoint_ pProtocol_ pSubscriptionType_ =
  CreateNotificationSubscription'
    { _cnsOrganizationId = pOrganizationId_
    , _cnsEndpoint = pEndpoint_
    , _cnsProtocol = pProtocol_
    , _cnsSubscriptionType = pSubscriptionType_
    }


-- | The ID of the organization.
cnsOrganizationId :: Lens' CreateNotificationSubscription Text
cnsOrganizationId = lens _cnsOrganizationId (\ s a -> s{_cnsOrganizationId = a})

-- | The endpoint to receive the notifications. If the protocol is HTTPS, the endpoint is a URL that begins with "https://".
cnsEndpoint :: Lens' CreateNotificationSubscription Text
cnsEndpoint = lens _cnsEndpoint (\ s a -> s{_cnsEndpoint = a})

-- | The protocol to use. The supported value is https, which delivers JSON-encoded messages using HTTPS POST.
cnsProtocol :: Lens' CreateNotificationSubscription SubscriptionProtocolType
cnsProtocol = lens _cnsProtocol (\ s a -> s{_cnsProtocol = a})

-- | The notification type.
cnsSubscriptionType :: Lens' CreateNotificationSubscription SubscriptionType
cnsSubscriptionType = lens _cnsSubscriptionType (\ s a -> s{_cnsSubscriptionType = a})

instance AWSRequest CreateNotificationSubscription
         where
        type Rs CreateNotificationSubscription =
             CreateNotificationSubscriptionResponse
        request = postJSON workDocs
        response
          = receiveJSON
              (\ s h x ->
                 CreateNotificationSubscriptionResponse' <$>
                   (x .?> "Subscription") <*> (pure (fromEnum s)))

instance Hashable CreateNotificationSubscription
         where

instance NFData CreateNotificationSubscription where

instance ToHeaders CreateNotificationSubscription
         where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateNotificationSubscription where
        toJSON CreateNotificationSubscription'{..}
          = object
              (catMaybes
                 [Just ("Endpoint" .= _cnsEndpoint),
                  Just ("Protocol" .= _cnsProtocol),
                  Just ("SubscriptionType" .= _cnsSubscriptionType)])

instance ToPath CreateNotificationSubscription where
        toPath CreateNotificationSubscription'{..}
          = mconcat
              ["/api/v1/organizations/", toBS _cnsOrganizationId,
               "/subscriptions"]

instance ToQuery CreateNotificationSubscription where
        toQuery = const mempty

-- | /See:/ 'createNotificationSubscriptionResponse' smart constructor.
data CreateNotificationSubscriptionResponse = CreateNotificationSubscriptionResponse'
  { _cnsrsSubscription   :: !(Maybe Subscription)
  , _cnsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNotificationSubscriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnsrsSubscription' - The subscription.
--
-- * 'cnsrsResponseStatus' - -- | The response status code.
createNotificationSubscriptionResponse
    :: Int -- ^ 'cnsrsResponseStatus'
    -> CreateNotificationSubscriptionResponse
createNotificationSubscriptionResponse pResponseStatus_ =
  CreateNotificationSubscriptionResponse'
    {_cnsrsSubscription = Nothing, _cnsrsResponseStatus = pResponseStatus_}


-- | The subscription.
cnsrsSubscription :: Lens' CreateNotificationSubscriptionResponse (Maybe Subscription)
cnsrsSubscription = lens _cnsrsSubscription (\ s a -> s{_cnsrsSubscription = a})

-- | -- | The response status code.
cnsrsResponseStatus :: Lens' CreateNotificationSubscriptionResponse Int
cnsrsResponseStatus = lens _cnsrsResponseStatus (\ s a -> s{_cnsrsResponseStatus = a})

instance NFData
           CreateNotificationSubscriptionResponse
         where
