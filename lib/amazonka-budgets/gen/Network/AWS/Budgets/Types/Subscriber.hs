-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Subscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Subscriber
  ( Subscriber (..),

    -- * Smart constructor
    mkSubscriber,

    -- * Lenses
    sSubscriptionType,
    sAddress,
  )
where

import Network.AWS.Budgets.Types.SubscriptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The subscriber to a budget notification. The subscriber consists of a subscription type and either an Amazon SNS topic or an email address.
--
-- For example, an email subscriber would have the following parameters:
--
--     * A @subscriptionType@ of @EMAIL@
--
--
--     * An @address@ of @example@example.com@
--
--
--
-- /See:/ 'mkSubscriber' smart constructor.
data Subscriber = Subscriber'
  { subscriptionType :: SubscriptionType,
    address :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Subscriber' with the minimum fields required to make a request.
--
-- * 'address' - The address that AWS sends budget notifications to, either an SNS topic or an email.
--
-- When you create a subscriber, the value of @Address@ can't contain line breaks.
-- * 'subscriptionType' - The type of notification that AWS sends to a subscriber.
mkSubscriber ::
  -- | 'subscriptionType'
  SubscriptionType ->
  -- | 'address'
  Lude.Sensitive Lude.Text ->
  Subscriber
mkSubscriber pSubscriptionType_ pAddress_ =
  Subscriber'
    { subscriptionType = pSubscriptionType_,
      address = pAddress_
    }

-- | The type of notification that AWS sends to a subscriber.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionType :: Lens.Lens' Subscriber SubscriptionType
sSubscriptionType = Lens.lens (subscriptionType :: Subscriber -> SubscriptionType) (\s a -> s {subscriptionType = a} :: Subscriber)
{-# DEPRECATED sSubscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead." #-}

-- | The address that AWS sends budget notifications to, either an SNS topic or an email.
--
-- When you create a subscriber, the value of @Address@ can't contain line breaks.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAddress :: Lens.Lens' Subscriber (Lude.Sensitive Lude.Text)
sAddress = Lens.lens (address :: Subscriber -> Lude.Sensitive Lude.Text) (\s a -> s {address = a} :: Subscriber)
{-# DEPRECATED sAddress "Use generic-lens or generic-optics with 'address' instead." #-}

instance Lude.FromJSON Subscriber where
  parseJSON =
    Lude.withObject
      "Subscriber"
      ( \x ->
          Subscriber'
            Lude.<$> (x Lude..: "SubscriptionType") Lude.<*> (x Lude..: "Address")
      )

instance Lude.ToJSON Subscriber where
  toJSON Subscriber' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SubscriptionType" Lude..= subscriptionType),
            Lude.Just ("Address" Lude..= address)
          ]
      )
