-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SNS.Types.Subscription
  ( Subscription (..),

    -- * Smart constructor
    mkSubscription,

    -- * Lenses
    sProtocol,
    sOwner,
    sTopicARN,
    sEndpoint,
    sSubscriptionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { protocol :: Lude.Maybe Lude.Text,
    owner :: Lude.Maybe Lude.Text,
    topicARN :: Lude.Maybe Lude.Text,
    endpoint :: Lude.Maybe Lude.Text,
    subscriptionARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subscription' with the minimum fields required to make a request.
--
-- * 'endpoint' - The subscription's endpoint (format depends on the protocol).
-- * 'owner' - The subscription's owner.
-- * 'protocol' - The subscription's protocol.
-- * 'subscriptionARN' - The subscription's ARN.
-- * 'topicARN' - The ARN of the subscription's topic.
mkSubscription ::
  Subscription
mkSubscription =
  Subscription'
    { protocol = Lude.Nothing,
      owner = Lude.Nothing,
      topicARN = Lude.Nothing,
      endpoint = Lude.Nothing,
      subscriptionARN = Lude.Nothing
    }

-- | The subscription's protocol.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProtocol :: Lens.Lens' Subscription (Lude.Maybe Lude.Text)
sProtocol = Lens.lens (protocol :: Subscription -> Lude.Maybe Lude.Text) (\s a -> s {protocol = a} :: Subscription)
{-# DEPRECATED sProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The subscription's owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwner :: Lens.Lens' Subscription (Lude.Maybe Lude.Text)
sOwner = Lens.lens (owner :: Subscription -> Lude.Maybe Lude.Text) (\s a -> s {owner = a} :: Subscription)
{-# DEPRECATED sOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The ARN of the subscription's topic.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicARN :: Lens.Lens' Subscription (Lude.Maybe Lude.Text)
sTopicARN = Lens.lens (topicARN :: Subscription -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: Subscription)
{-# DEPRECATED sTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The subscription's endpoint (format depends on the protocol).
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndpoint :: Lens.Lens' Subscription (Lude.Maybe Lude.Text)
sEndpoint = Lens.lens (endpoint :: Subscription -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: Subscription)
{-# DEPRECATED sEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The subscription's ARN.
--
-- /Note:/ Consider using 'subscriptionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionARN :: Lens.Lens' Subscription (Lude.Maybe Lude.Text)
sSubscriptionARN = Lens.lens (subscriptionARN :: Subscription -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionARN = a} :: Subscription)
{-# DEPRECATED sSubscriptionARN "Use generic-lens or generic-optics with 'subscriptionARN' instead." #-}

instance Lude.FromXML Subscription where
  parseXML x =
    Subscription'
      Lude.<$> (x Lude..@? "Protocol")
      Lude.<*> (x Lude..@? "Owner")
      Lude.<*> (x Lude..@? "TopicArn")
      Lude.<*> (x Lude..@? "Endpoint")
      Lude.<*> (x Lude..@? "SubscriptionArn")
