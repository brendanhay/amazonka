{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ActivateEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a partner event source that has been deactivated. Once activated, your matching event bus will start receiving events from the event source.
module Network.AWS.CloudWatchEvents.ActivateEventSource
    (
    -- * Creating a request
      ActivateEventSource (..)
    , mkActivateEventSource
    -- ** Request lenses
    , aesName

    -- * Destructuring the response
    , ActivateEventSourceResponse (..)
    , mkActivateEventSourceResponse
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkActivateEventSource' smart constructor.
newtype ActivateEventSource = ActivateEventSource'
  { name :: Types.EventSourceName
    -- ^ The name of the partner event source to activate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ActivateEventSource' value with any optional fields omitted.
mkActivateEventSource
    :: Types.EventSourceName -- ^ 'name'
    -> ActivateEventSource
mkActivateEventSource name = ActivateEventSource'{name}

-- | The name of the partner event source to activate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesName :: Lens.Lens' ActivateEventSource Types.EventSourceName
aesName = Lens.field @"name"
{-# INLINEABLE aesName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery ActivateEventSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ActivateEventSource where
        toHeaders ActivateEventSource{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.ActivateEventSource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ActivateEventSource where
        toJSON ActivateEventSource{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest ActivateEventSource where
        type Rs ActivateEventSource = ActivateEventSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull ActivateEventSourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkActivateEventSourceResponse' smart constructor.
data ActivateEventSourceResponse = ActivateEventSourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActivateEventSourceResponse' value with any optional fields omitted.
mkActivateEventSourceResponse
    :: ActivateEventSourceResponse
mkActivateEventSourceResponse = ActivateEventSourceResponse'
