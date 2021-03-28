{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.RoutingProfileQueueReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Connect.Types.RoutingProfileQueueReference
  ( RoutingProfileQueueReference (..)
  -- * Smart constructor
  , mkRoutingProfileQueueReference
  -- * Lenses
  , rpqrQueueId
  , rpqrChannel
  ) where

import qualified Network.AWS.Connect.Types.Channel as Types
import qualified Network.AWS.Connect.Types.QueueId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the channel and queue identifier for a routing profile.
--
-- /See:/ 'mkRoutingProfileQueueReference' smart constructor.
data RoutingProfileQueueReference = RoutingProfileQueueReference'
  { queueId :: Types.QueueId
    -- ^ The identifier of the queue.
  , channel :: Types.Channel
    -- ^ The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RoutingProfileQueueReference' value with any optional fields omitted.
mkRoutingProfileQueueReference
    :: Types.QueueId -- ^ 'queueId'
    -> Types.Channel -- ^ 'channel'
    -> RoutingProfileQueueReference
mkRoutingProfileQueueReference queueId channel
  = RoutingProfileQueueReference'{queueId, channel}

-- | The identifier of the queue.
--
-- /Note:/ Consider using 'queueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqrQueueId :: Lens.Lens' RoutingProfileQueueReference Types.QueueId
rpqrQueueId = Lens.field @"queueId"
{-# INLINEABLE rpqrQueueId #-}
{-# DEPRECATED queueId "Use generic-lens or generic-optics with 'queueId' instead"  #-}

-- | The channels agents can handle in the Contact Control Panel (CCP) for this routing profile.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpqrChannel :: Lens.Lens' RoutingProfileQueueReference Types.Channel
rpqrChannel = Lens.field @"channel"
{-# INLINEABLE rpqrChannel #-}
{-# DEPRECATED channel "Use generic-lens or generic-optics with 'channel' instead"  #-}

instance Core.FromJSON RoutingProfileQueueReference where
        toJSON RoutingProfileQueueReference{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("QueueId" Core..= queueId),
                  Core.Just ("Channel" Core..= channel)])
