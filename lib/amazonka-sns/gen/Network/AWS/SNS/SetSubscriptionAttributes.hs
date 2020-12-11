{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a subscription owner to set an attribute of the subscription to a new value.
module Network.AWS.SNS.SetSubscriptionAttributes
  ( -- * Creating a request
    SetSubscriptionAttributes (..),
    mkSetSubscriptionAttributes,

    -- ** Request lenses
    ssaAttributeValue,
    ssaSubscriptionARN,
    ssaAttributeName,

    -- * Destructuring the response
    SetSubscriptionAttributesResponse (..),
    mkSetSubscriptionAttributesResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for SetSubscriptionAttributes action.
--
-- /See:/ 'mkSetSubscriptionAttributes' smart constructor.
data SetSubscriptionAttributes = SetSubscriptionAttributes'
  { attributeValue ::
      Lude.Maybe Lude.Text,
    subscriptionARN :: Lude.Text,
    attributeName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetSubscriptionAttributes' with the minimum fields required to make a request.
--
-- * 'attributeName' - A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that this action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @FilterPolicy@ – The simple JSON object that lets your subscriber receive only a subset of messages, rather than receiving every message published to the topic.
--
--
--     * @RawMessageDelivery@ – When set to @true@ , enables raw message delivery to Amazon SQS or HTTP/S endpoints. This eliminates the need for the endpoints to process JSON formatting, which is otherwise created for Amazon SNS metadata.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
-- * 'attributeValue' - The new value for the attribute in JSON format.
-- * 'subscriptionARN' - The ARN of the subscription to modify.
mkSetSubscriptionAttributes ::
  -- | 'subscriptionARN'
  Lude.Text ->
  -- | 'attributeName'
  Lude.Text ->
  SetSubscriptionAttributes
mkSetSubscriptionAttributes pSubscriptionARN_ pAttributeName_ =
  SetSubscriptionAttributes'
    { attributeValue = Lude.Nothing,
      subscriptionARN = pSubscriptionARN_,
      attributeName = pAttributeName_
    }

-- | The new value for the attribute in JSON format.
--
-- /Note:/ Consider using 'attributeValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssaAttributeValue :: Lens.Lens' SetSubscriptionAttributes (Lude.Maybe Lude.Text)
ssaAttributeValue = Lens.lens (attributeValue :: SetSubscriptionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {attributeValue = a} :: SetSubscriptionAttributes)
{-# DEPRECATED ssaAttributeValue "Use generic-lens or generic-optics with 'attributeValue' instead." #-}

-- | The ARN of the subscription to modify.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssaSubscriptionARN :: Lens.Lens' SetSubscriptionAttributes Lude.Text
ssaSubscriptionARN = Lens.lens (subscriptionARN :: SetSubscriptionAttributes -> Lude.Text) (\s a -> s {subscriptionARN = a} :: SetSubscriptionAttributes)
{-# DEPRECATED ssaSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

-- | A map of attributes with their corresponding values.
--
-- The following lists the names, descriptions, and values of the special request parameters that this action uses:
--
--     * @DeliveryPolicy@ – The policy that defines how Amazon SNS retries failed deliveries to HTTP/S endpoints.
--
--
--     * @FilterPolicy@ – The simple JSON object that lets your subscriber receive only a subset of messages, rather than receiving every message published to the topic.
--
--
--     * @RawMessageDelivery@ – When set to @true@ , enables raw message delivery to Amazon SQS or HTTP/S endpoints. This eliminates the need for the endpoints to process JSON formatting, which is otherwise created for Amazon SNS metadata.
--
--
--     * @RedrivePolicy@ – When specified, sends undeliverable messages to the specified Amazon SQS dead-letter queue. Messages that can't be delivered due to client errors (for example, when the subscribed endpoint is unreachable) or server errors (for example, when the service that powers the subscribed endpoint becomes unavailable) are held in the dead-letter queue for further analysis or reprocessing.
--
--
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssaAttributeName :: Lens.Lens' SetSubscriptionAttributes Lude.Text
ssaAttributeName = Lens.lens (attributeName :: SetSubscriptionAttributes -> Lude.Text) (\s a -> s {attributeName = a} :: SetSubscriptionAttributes)
{-# DEPRECATED ssaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

instance Lude.AWSRequest SetSubscriptionAttributes where
  type
    Rs SetSubscriptionAttributes =
      SetSubscriptionAttributesResponse
  request = Req.postQuery snsService
  response = Res.receiveNull SetSubscriptionAttributesResponse'

instance Lude.ToHeaders SetSubscriptionAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetSubscriptionAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery SetSubscriptionAttributes where
  toQuery SetSubscriptionAttributes' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetSubscriptionAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "AttributeValue" Lude.=: attributeValue,
        "SubscriptionArn" Lude.=: subscriptionARN,
        "AttributeName" Lude.=: attributeName
      ]

-- | /See:/ 'mkSetSubscriptionAttributesResponse' smart constructor.
data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetSubscriptionAttributesResponse' with the minimum fields required to make a request.
mkSetSubscriptionAttributesResponse ::
  SetSubscriptionAttributesResponse
mkSetSubscriptionAttributesResponse =
  SetSubscriptionAttributesResponse'
