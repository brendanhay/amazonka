{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SNS.Types.Subscription
  ( Subscription (..)
  -- * Smart constructor
  , mkSubscription
  -- * Lenses
  , sEndpoint
  , sOwner
  , sProtocol
  , sSubscriptionArn
  , sTopicArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SNS.Types.Account as Types
import qualified Network.AWS.SNS.Types.Endpoint as Types
import qualified Network.AWS.SNS.Types.Protocol as Types
import qualified Network.AWS.SNS.Types.SubscriptionArn as Types
import qualified Network.AWS.SNS.Types.TopicARN as Types

-- | A wrapper type for the attributes of an Amazon SNS subscription.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { endpoint :: Core.Maybe Types.Endpoint
    -- ^ The subscription's endpoint (format depends on the protocol).
  , owner :: Core.Maybe Types.Account
    -- ^ The subscription's owner.
  , protocol :: Core.Maybe Types.Protocol
    -- ^ The subscription's protocol.
  , subscriptionArn :: Core.Maybe Types.SubscriptionArn
    -- ^ The subscription's ARN.
  , topicArn :: Core.Maybe Types.TopicARN
    -- ^ The ARN of the subscription's topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subscription' value with any optional fields omitted.
mkSubscription
    :: Subscription
mkSubscription
  = Subscription'{endpoint = Core.Nothing, owner = Core.Nothing,
                  protocol = Core.Nothing, subscriptionArn = Core.Nothing,
                  topicArn = Core.Nothing}

-- | The subscription's endpoint (format depends on the protocol).
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndpoint :: Lens.Lens' Subscription (Core.Maybe Types.Endpoint)
sEndpoint = Lens.field @"endpoint"
{-# INLINEABLE sEndpoint #-}
{-# DEPRECATED endpoint "Use generic-lens or generic-optics with 'endpoint' instead"  #-}

-- | The subscription's owner.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sOwner :: Lens.Lens' Subscription (Core.Maybe Types.Account)
sOwner = Lens.field @"owner"
{-# INLINEABLE sOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

-- | The subscription's protocol.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProtocol :: Lens.Lens' Subscription (Core.Maybe Types.Protocol)
sProtocol = Lens.field @"protocol"
{-# INLINEABLE sProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The subscription's ARN.
--
-- /Note:/ Consider using 'subscriptionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionArn :: Lens.Lens' Subscription (Core.Maybe Types.SubscriptionArn)
sSubscriptionArn = Lens.field @"subscriptionArn"
{-# INLINEABLE sSubscriptionArn #-}
{-# DEPRECATED subscriptionArn "Use generic-lens or generic-optics with 'subscriptionArn' instead"  #-}

-- | The ARN of the subscription's topic.
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sTopicArn :: Lens.Lens' Subscription (Core.Maybe Types.TopicARN)
sTopicArn = Lens.field @"topicArn"
{-# INLINEABLE sTopicArn #-}
{-# DEPRECATED topicArn "Use generic-lens or generic-optics with 'topicArn' instead"  #-}

instance Core.FromXML Subscription where
        parseXML x
          = Subscription' Core.<$>
              (x Core..@? "Endpoint") Core.<*> x Core..@? "Owner" Core.<*>
                x Core..@? "Protocol"
                Core.<*> x Core..@? "SubscriptionArn"
                Core.<*> x Core..@? "TopicArn"
