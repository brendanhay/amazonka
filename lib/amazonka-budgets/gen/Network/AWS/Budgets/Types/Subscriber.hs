{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.Subscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Budgets.Types.Subscriber
  ( Subscriber (..)
  -- * Smart constructor
  , mkSubscriber
  -- * Lenses
  , sSubscriptionType
  , sAddress
  ) where

import qualified Network.AWS.Budgets.Types.Address as Types
import qualified Network.AWS.Budgets.Types.SubscriptionType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

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
  { subscriptionType :: Types.SubscriptionType
    -- ^ The type of notification that AWS sends to a subscriber.
  , address :: Types.Address
    -- ^ The address that AWS sends budget notifications to, either an SNS topic or an email.
--
-- When you create a subscriber, the value of @Address@ can't contain line breaks.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subscriber' value with any optional fields omitted.
mkSubscriber
    :: Types.SubscriptionType -- ^ 'subscriptionType'
    -> Types.Address -- ^ 'address'
    -> Subscriber
mkSubscriber subscriptionType address
  = Subscriber'{subscriptionType, address}

-- | The type of notification that AWS sends to a subscriber.
--
-- /Note:/ Consider using 'subscriptionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionType :: Lens.Lens' Subscriber Types.SubscriptionType
sSubscriptionType = Lens.field @"subscriptionType"
{-# INLINEABLE sSubscriptionType #-}
{-# DEPRECATED subscriptionType "Use generic-lens or generic-optics with 'subscriptionType' instead"  #-}

-- | The address that AWS sends budget notifications to, either an SNS topic or an email.
--
-- When you create a subscriber, the value of @Address@ can't contain line breaks.
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAddress :: Lens.Lens' Subscriber Types.Address
sAddress = Lens.field @"address"
{-# INLINEABLE sAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

instance Core.FromJSON Subscriber where
        toJSON Subscriber{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SubscriptionType" Core..= subscriptionType),
                  Core.Just ("Address" Core..= address)])

instance Core.FromJSON Subscriber where
        parseJSON
          = Core.withObject "Subscriber" Core.$
              \ x ->
                Subscriber' Core.<$>
                  (x Core..: "SubscriptionType") Core.<*> x Core..: "Address"
