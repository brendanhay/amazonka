{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Subscriber
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Subscriber
  ( Subscriber (..)
  -- * Smart constructor
  , mkSubscriber
  -- * Lenses
  , sAddress
  , sStatus
  , sType
  ) where

import qualified Network.AWS.CostExplorer.Types.Address as Types
import qualified Network.AWS.CostExplorer.Types.SubscriberStatus as Types
import qualified Network.AWS.CostExplorer.Types.SubscriberType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The recipient of @AnomalySubscription@ notifications. 
--
-- /See:/ 'mkSubscriber' smart constructor.
data Subscriber = Subscriber'
  { address :: Core.Maybe Types.Address
    -- ^ The email address or SNS Amazon Resource Name (ARN), depending on the @Type@ . 
  , status :: Core.Maybe Types.SubscriberStatus
    -- ^ Indicates if the subscriber accepts the notifications. 
  , type' :: Core.Maybe Types.SubscriberType
    -- ^ The notification delivery channel. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subscriber' value with any optional fields omitted.
mkSubscriber
    :: Subscriber
mkSubscriber
  = Subscriber'{address = Core.Nothing, status = Core.Nothing,
                type' = Core.Nothing}

-- | The email address or SNS Amazon Resource Name (ARN), depending on the @Type@ . 
--
-- /Note:/ Consider using 'address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sAddress :: Lens.Lens' Subscriber (Core.Maybe Types.Address)
sAddress = Lens.field @"address"
{-# INLINEABLE sAddress #-}
{-# DEPRECATED address "Use generic-lens or generic-optics with 'address' instead"  #-}

-- | Indicates if the subscriber accepts the notifications. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sStatus :: Lens.Lens' Subscriber (Core.Maybe Types.SubscriberStatus)
sStatus = Lens.field @"status"
{-# INLINEABLE sStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The notification delivery channel. 
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sType :: Lens.Lens' Subscriber (Core.Maybe Types.SubscriberType)
sType = Lens.field @"type'"
{-# INLINEABLE sType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Subscriber where
        toJSON Subscriber{..}
          = Core.object
              (Core.catMaybes
                 [("Address" Core..=) Core.<$> address,
                  ("Status" Core..=) Core.<$> status,
                  ("Type" Core..=) Core.<$> type'])

instance Core.FromJSON Subscriber where
        parseJSON
          = Core.withObject "Subscriber" Core.$
              \ x ->
                Subscriber' Core.<$>
                  (x Core..:? "Address") Core.<*> x Core..:? "Status" Core.<*>
                    x Core..:? "Type"
