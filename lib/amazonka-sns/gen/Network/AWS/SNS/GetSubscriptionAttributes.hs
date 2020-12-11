{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all of the properties of a subscription.
module Network.AWS.SNS.GetSubscriptionAttributes
  ( -- * Creating a request
    GetSubscriptionAttributes (..),
    mkGetSubscriptionAttributes,

    -- ** Request lenses
    gsaSubscriptionARN,

    -- * Destructuring the response
    GetSubscriptionAttributesResponse (..),
    mkGetSubscriptionAttributesResponse,

    -- ** Response lenses
    gsarsAttributes,
    gsarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for GetSubscriptionAttributes.
--
-- /See:/ 'mkGetSubscriptionAttributes' smart constructor.
newtype GetSubscriptionAttributes = GetSubscriptionAttributes'
  { subscriptionARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionAttributes' with the minimum fields required to make a request.
--
-- * 'subscriptionARN' - The ARN of the subscription whose properties you want to get.
mkGetSubscriptionAttributes ::
  -- | 'subscriptionARN'
  Lude.Text ->
  GetSubscriptionAttributes
mkGetSubscriptionAttributes pSubscriptionARN_ =
  GetSubscriptionAttributes' {subscriptionARN = pSubscriptionARN_}

-- | The ARN of the subscription whose properties you want to get.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsaSubscriptionARN :: Lens.Lens' GetSubscriptionAttributes Lude.Text
gsaSubscriptionARN = Lens.lens (subscriptionARN :: GetSubscriptionAttributes -> Lude.Text) (\s a -> s {subscriptionARN = a} :: GetSubscriptionAttributes)
{-# DEPRECATED gsaSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

instance Lude.AWSRequest GetSubscriptionAttributes where
  type
    Rs GetSubscriptionAttributes =
      GetSubscriptionAttributesResponse
  request = Req.postQuery snsService
  response =
    Res.receiveXMLWrapper
      "GetSubscriptionAttributesResult"
      ( \s h x ->
          GetSubscriptionAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLMap "entry" "key" "value")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSubscriptionAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetSubscriptionAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSubscriptionAttributes where
  toQuery GetSubscriptionAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetSubscriptionAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "SubscriptionArn" Lude.=: subscriptionARN
      ]

-- | Response for GetSubscriptionAttributes action.
--
-- /See:/ 'mkGetSubscriptionAttributesResponse' smart constructor.
data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse'
  { attributes ::
      Lude.Maybe
        ( Lude.HashMap
            Lude.Text
            (Lude.Text)
        ),
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSubscriptionAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - A map of the subscription's attributes. Attributes in this map include the following:
--
--
--     * @ConfirmationWasAuthenticated@ – @true@ if the subscription confirmation request was authenticated.
--
--
--     * @DeliveryPolicy@ – The JSON serialization of the subscription's delivery policy.
--
--
--     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy that takes into account the topic delivery policy and account system defaults.
--
--
--     * @FilterPolicy@ – The filter policy JSON that is assigned to the subscription. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-message-filtering.html Amazon SNS Message Filtering> in the /Amazon SNS Developer Guide/ .
--
--
--     * @Owner@ – The AWS account ID of the subscription's owner.
--
--
--     * @PendingConfirmation@ – @true@ if the subscription hasn't been confirmed. To confirm a pending subscription, call the @ConfirmSubscription@ action with a confirmation token.
--
--
--     * @RawMessageDelivery@ – @true@ if raw message delivery is enabled for the subscription. Raw messages are free of JSON formatting and can be sent to HTTP/S and Amazon SQS endpoints.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
--     * @SubscriptionArn@ – The subscription's ARN.
--
--
--     * @TopicArn@ – The topic ARN that the subscription is associated with.
--
--
-- * 'responseStatus' - The response status code.
mkGetSubscriptionAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSubscriptionAttributesResponse
mkGetSubscriptionAttributesResponse pResponseStatus_ =
  GetSubscriptionAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A map of the subscription's attributes. Attributes in this map include the following:
--
--
--     * @ConfirmationWasAuthenticated@ – @true@ if the subscription confirmation request was authenticated.
--
--
--     * @DeliveryPolicy@ – The JSON serialization of the subscription's delivery policy.
--
--
--     * @EffectiveDeliveryPolicy@ – The JSON serialization of the effective delivery policy that takes into account the topic delivery policy and account system defaults.
--
--
--     * @FilterPolicy@ – The filter policy JSON that is assigned to the subscription. For more information, see <https://docs.aws.amazon.com/sns/latest/dg/sns-message-filtering.html Amazon SNS Message Filtering> in the /Amazon SNS Developer Guide/ .
--
--
--     * @Owner@ – The AWS account ID of the subscription's owner.
--
--
--     * @PendingConfirmation@ – @true@ if the subscription hasn't been confirmed. To confirm a pending subscription, call the @ConfirmSubscription@ action with a confirmation token.
--
--
--     * @RawMessageDelivery@ – @true@ if raw message delivery is enabled for the subscription. Raw messages are free of JSON formatting and can be sent to HTTP/S and Amazon SQS endpoints.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
--     * @SubscriptionArn@ – The subscription's ARN.
--
--
--     * @TopicArn@ – The topic ARN that the subscription is associated with.
--
--
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsarsAttributes :: Lens.Lens' GetSubscriptionAttributesResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gsarsAttributes = Lens.lens (attributes :: GetSubscriptionAttributesResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {attributes = a} :: GetSubscriptionAttributesResponse)
{-# DEPRECATED gsarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsarsResponseStatus :: Lens.Lens' GetSubscriptionAttributesResponse Lude.Int
gsarsResponseStatus = Lens.lens (responseStatus :: GetSubscriptionAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSubscriptionAttributesResponse)
{-# DEPRECATED gsarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
