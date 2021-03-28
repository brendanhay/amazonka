{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.Subscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.Subscription
  ( Subscription (..)
  -- * Smart constructor
  , mkSubscription
  -- * Lenses
  , sEndPoint
  , sProtocol
  , sSubscriptionId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.SubscriptionEndPointType as Types
import qualified Network.AWS.WorkDocs.Types.SubscriptionProtocolType as Types

-- | Describes a subscription.
--
-- /See:/ 'mkSubscription' smart constructor.
data Subscription = Subscription'
  { endPoint :: Core.Maybe Types.SubscriptionEndPointType
    -- ^ The endpoint of the subscription.
  , protocol :: Core.Maybe Types.SubscriptionProtocolType
    -- ^ The protocol of the subscription.
  , subscriptionId :: Core.Maybe Types.IdType
    -- ^ The ID of the subscription.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subscription' value with any optional fields omitted.
mkSubscription
    :: Subscription
mkSubscription
  = Subscription'{endPoint = Core.Nothing, protocol = Core.Nothing,
                  subscriptionId = Core.Nothing}

-- | The endpoint of the subscription.
--
-- /Note:/ Consider using 'endPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sEndPoint :: Lens.Lens' Subscription (Core.Maybe Types.SubscriptionEndPointType)
sEndPoint = Lens.field @"endPoint"
{-# INLINEABLE sEndPoint #-}
{-# DEPRECATED endPoint "Use generic-lens or generic-optics with 'endPoint' instead"  #-}

-- | The protocol of the subscription.
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sProtocol :: Lens.Lens' Subscription (Core.Maybe Types.SubscriptionProtocolType)
sProtocol = Lens.field @"protocol"
{-# INLINEABLE sProtocol #-}
{-# DEPRECATED protocol "Use generic-lens or generic-optics with 'protocol' instead"  #-}

-- | The ID of the subscription.
--
-- /Note:/ Consider using 'subscriptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubscriptionId :: Lens.Lens' Subscription (Core.Maybe Types.IdType)
sSubscriptionId = Lens.field @"subscriptionId"
{-# INLINEABLE sSubscriptionId #-}
{-# DEPRECATED subscriptionId "Use generic-lens or generic-optics with 'subscriptionId' instead"  #-}

instance Core.FromJSON Subscription where
        parseJSON
          = Core.withObject "Subscription" Core.$
              \ x ->
                Subscription' Core.<$>
                  (x Core..:? "EndPoint") Core.<*> x Core..:? "Protocol" Core.<*>
                    x Core..:? "SubscriptionId"
