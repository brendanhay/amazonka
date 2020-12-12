{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.Subscription
  ( Subscription (..),

    -- * Smart constructor
    mkSubscription,

    -- * Lenses
    sProtocol,
    sEndPoint,
    sSubscriptionId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.SubscriptionProtocolType

-- | Describes a subscription.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { protocol ::
      Lude.Maybe SubscriptionProtocolType,
    endPoint :: Lude.Maybe Lude.Text,
    subscriptionId :: Lude.Maybe Lude.Text
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
-- * 'endPoint' - The endpoint of the subscription.
-- * 'protocol' - The protocol of the subscription.
-- * 'subscriptionId' - The ID of the subscription.
mkSubscription ::
  Subscription
mkSubscription =
  Subscription'
    { protocol = Lude.Nothing,
      endPoint = Lude.Nothing,
      subscriptionId = Lude.Nothing
    }

-- | The protocol of the subscription.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProtocol :: Lens.Lens' Subscription (Lude.Maybe SubscriptionProtocolType)
sProtocol = Lens.lens (protocol :: Subscription -> Lude.Maybe SubscriptionProtocolType) (\s a -> s {protocol = a} :: Subscription)
{-# DEPRECATED sProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The endpoint of the subscription.
--
-- /Note:/ Consider using 'endPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndPoint :: Lens.Lens' Subscription (Lude.Maybe Lude.Text)
sEndPoint = Lens.lens (endPoint :: Subscription -> Lude.Maybe Lude.Text) (\s a -> s {endPoint = a} :: Subscription)
{-# DEPRECATED sEndPoint "Use generic-lens or generic-optics with 'endPoint' instead." #-}

-- | The ID of the subscription.
--
-- /Note:/ Consider using 'subscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionId :: Lens.Lens' Subscription (Lude.Maybe Lude.Text)
sSubscriptionId = Lens.lens (subscriptionId :: Subscription -> Lude.Maybe Lude.Text) (\s a -> s {subscriptionId = a} :: Subscription)
{-# DEPRECATED sSubscriptionId "Use generic-lens or generic-optics with 'subscriptionId' instead." #-}

instance Lude.FromJSON Subscription where
  parseJSON =
    Lude.withObject
      "Subscription"
      ( \x ->
          Subscription'
            Lude.<$> (x Lude..:? "Protocol")
            Lude.<*> (x Lude..:? "EndPoint")
            Lude.<*> (x Lude..:? "SubscriptionId")
      )
