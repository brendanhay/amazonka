{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotDatafeedSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.SpotDatafeedSubscription
  ( SpotDatafeedSubscription (..)
  -- * Smart constructor
  , mkSpotDatafeedSubscription
  -- * Lenses
  , sdsBucket
  , sdsFault
  , sdsOwnerId
  , sdsPrefix
  , sdsState
  ) where

import qualified Network.AWS.EC2.Types.DatafeedSubscriptionState as Types
import qualified Network.AWS.EC2.Types.SpotInstanceStateFault as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the data feed for a Spot Instance.
--
-- /See:/ 'mkSpotDatafeedSubscription' smart constructor.
data SpotDatafeedSubscription = SpotDatafeedSubscription'
  { bucket :: Core.Maybe Core.Text
    -- ^ The name of the Amazon S3 bucket where the Spot Instance data feed is located.
  , fault :: Core.Maybe Types.SpotInstanceStateFault
    -- ^ The fault codes for the Spot Instance request, if any.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the account.
  , prefix :: Core.Maybe Core.Text
    -- ^ The prefix for the data feed files.
  , state :: Core.Maybe Types.DatafeedSubscriptionState
    -- ^ The state of the Spot Instance data feed subscription.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotDatafeedSubscription' value with any optional fields omitted.
mkSpotDatafeedSubscription
    :: SpotDatafeedSubscription
mkSpotDatafeedSubscription
  = SpotDatafeedSubscription'{bucket = Core.Nothing,
                              fault = Core.Nothing, ownerId = Core.Nothing,
                              prefix = Core.Nothing, state = Core.Nothing}

-- | The name of the Amazon S3 bucket where the Spot Instance data feed is located.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsBucket :: Lens.Lens' SpotDatafeedSubscription (Core.Maybe Core.Text)
sdsBucket = Lens.field @"bucket"
{-# INLINEABLE sdsBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | The fault codes for the Spot Instance request, if any.
--
-- /Note:/ Consider using 'fault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsFault :: Lens.Lens' SpotDatafeedSubscription (Core.Maybe Types.SpotInstanceStateFault)
sdsFault = Lens.field @"fault"
{-# INLINEABLE sdsFault #-}
{-# DEPRECATED fault "Use generic-lens or generic-optics with 'fault' instead"  #-}

-- | The AWS account ID of the account.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsOwnerId :: Lens.Lens' SpotDatafeedSubscription (Core.Maybe Core.Text)
sdsOwnerId = Lens.field @"ownerId"
{-# INLINEABLE sdsOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The prefix for the data feed files.
--
-- /Note:/ Consider using 'prefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsPrefix :: Lens.Lens' SpotDatafeedSubscription (Core.Maybe Core.Text)
sdsPrefix = Lens.field @"prefix"
{-# INLINEABLE sdsPrefix #-}
{-# DEPRECATED prefix "Use generic-lens or generic-optics with 'prefix' instead"  #-}

-- | The state of the Spot Instance data feed subscription.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdsState :: Lens.Lens' SpotDatafeedSubscription (Core.Maybe Types.DatafeedSubscriptionState)
sdsState = Lens.field @"state"
{-# INLINEABLE sdsState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

instance Core.FromXML SpotDatafeedSubscription where
        parseXML x
          = SpotDatafeedSubscription' Core.<$>
              (x Core..@? "bucket") Core.<*> x Core..@? "fault" Core.<*>
                x Core..@? "ownerId"
                Core.<*> x Core..@? "prefix"
                Core.<*> x Core..@? "state"
