{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateNotificationSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configure Amazon WorkDocs to use Amazon SNS notifications. The endpoint receives a confirmation message, and must confirm the subscription.
--
-- For more information, see <https://docs.aws.amazon.com/workdocs/latest/developerguide/subscribe-notifications.html Subscribe to Notifications> in the /Amazon WorkDocs Developer Guide/ .
module Network.AWS.WorkDocs.CreateNotificationSubscription
  ( -- * Creating a request
    CreateNotificationSubscription (..),
    mkCreateNotificationSubscription,

    -- ** Request lenses
    cnsSubscriptionType,
    cnsProtocol,
    cnsEndpoint,
    cnsOrganizationId,

    -- * Destructuring the response
    CreateNotificationSubscriptionResponse (..),
    mkCreateNotificationSubscriptionResponse,

    -- ** Response lenses
    cnsrsSubscription,
    cnsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkCreateNotificationSubscription' smart constructor.
data CreateNotificationSubscription = CreateNotificationSubscription'
  { -- | The notification type.
    subscriptionType :: SubscriptionType,
    -- | The protocol to use. The supported value is https, which delivers JSON-encoded messages using HTTPS POST.
    protocol :: SubscriptionProtocolType,
    -- | The endpoint to receive the notifications. If the protocol is HTTPS, the endpoint is a URL that begins with @https@ .
    endpoint :: Lude.Text,
    -- | The ID of the organization.
    organizationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNotificationSubscription' with the minimum fields required to make a request.
--
-- * 'subscriptionType' - The notification type.
-- * 'protocol' - The protocol to use. The supported value is https, which delivers JSON-encoded messages using HTTPS POST.
-- * 'endpoint' - The endpoint to receive the notifications. If the protocol is HTTPS, the endpoint is a URL that begins with @https@ .
-- * 'organizationId' - The ID of the organization.
mkCreateNotificationSubscription ::
  -- | 'subscriptionType'
  SubscriptionType ->
  -- | 'protocol'
  SubscriptionProtocolType ->
  -- | 'endpoint'
  Lude.Text ->
  -- | 'organizationId'
  Lude.Text ->
  CreateNotificationSubscription
mkCreateNotificationSubscription
  pSubscriptionType_
  pProtocol_
  pEndpoint_
  pOrganizationId_ =
    CreateNotificationSubscription'
      { subscriptionType =
          pSubscriptionType_,
        protocol = pProtocol_,
        endpoint = pEndpoint_,
        organizationId = pOrganizationId_
      }

-- | The notification type.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsSubscriptionType :: Lens.Lens' CreateNotificationSubscription SubscriptionType
cnsSubscriptionType = Lens.lens (subscriptionType :: CreateNotificationSubscription -> SubscriptionType) (\s a -> s {subscriptionType = a} :: CreateNotificationSubscription)
{-# DEPRECATED cnsSubscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead." #-}

-- | The protocol to use. The supported value is https, which delivers JSON-encoded messages using HTTPS POST.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsProtocol :: Lens.Lens' CreateNotificationSubscription SubscriptionProtocolType
cnsProtocol = Lens.lens (protocol :: CreateNotificationSubscription -> SubscriptionProtocolType) (\s a -> s {protocol = a} :: CreateNotificationSubscription)
{-# DEPRECATED cnsProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The endpoint to receive the notifications. If the protocol is HTTPS, the endpoint is a URL that begins with @https@ .
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsEndpoint :: Lens.Lens' CreateNotificationSubscription Lude.Text
cnsEndpoint = Lens.lens (endpoint :: CreateNotificationSubscription -> Lude.Text) (\s a -> s {endpoint = a} :: CreateNotificationSubscription)
{-# DEPRECATED cnsEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The ID of the organization.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsOrganizationId :: Lens.Lens' CreateNotificationSubscription Lude.Text
cnsOrganizationId = Lens.lens (organizationId :: CreateNotificationSubscription -> Lude.Text) (\s a -> s {organizationId = a} :: CreateNotificationSubscription)
{-# DEPRECATED cnsOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

instance Lude.AWSRequest CreateNotificationSubscription where
  type
    Rs CreateNotificationSubscription =
      CreateNotificationSubscriptionResponse
  request = Req.postJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateNotificationSubscriptionResponse'
            Lude.<$> (x Lude..?> "Subscription") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNotificationSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNotificationSubscription where
  toJSON CreateNotificationSubscription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SubscriptionType" Lude..= subscriptionType),
            Lude.Just ("Protocol" Lude..= protocol),
            Lude.Just ("Endpoint" Lude..= endpoint)
          ]
      )

instance Lude.ToPath CreateNotificationSubscription where
  toPath CreateNotificationSubscription' {..} =
    Lude.mconcat
      [ "/api/v1/organizations/",
        Lude.toBS organizationId,
        "/subscriptions"
      ]

instance Lude.ToQuery CreateNotificationSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateNotificationSubscriptionResponse' smart constructor.
data CreateNotificationSubscriptionResponse = CreateNotificationSubscriptionResponse'
  { -- | The subscription.
    subscription :: Lude.Maybe Subscription,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNotificationSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'subscription' - The subscription.
-- * 'responseStatus' - The response status code.
mkCreateNotificationSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNotificationSubscriptionResponse
mkCreateNotificationSubscriptionResponse pResponseStatus_ =
  CreateNotificationSubscriptionResponse'
    { subscription =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The subscription.
--
-- /Note:/ Consider using 'subscription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsrsSubscription :: Lens.Lens' CreateNotificationSubscriptionResponse (Lude.Maybe Subscription)
cnsrsSubscription = Lens.lens (subscription :: CreateNotificationSubscriptionResponse -> Lude.Maybe Subscription) (\s a -> s {subscription = a} :: CreateNotificationSubscriptionResponse)
{-# DEPRECATED cnsrsSubscription "Use generic-lens or generic-optics with 'subscription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnsrsResponseStatus :: Lens.Lens' CreateNotificationSubscriptionResponse Lude.Int
cnsrsResponseStatus = Lens.lens (responseStatus :: CreateNotificationSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNotificationSubscriptionResponse)
{-# DEPRECATED cnsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
