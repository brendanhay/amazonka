{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeactivateEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this operation to temporarily stop receiving events from the specified partner event source. The matching event bus is not deleted. 
--
-- When you deactivate a partner event source, the source goes into PENDING state. If it remains in PENDING state for more than two weeks, it is deleted.
-- To activate a deactivated partner event source, use 'ActivateEventSource' .
module Network.AWS.CloudWatchEvents.DeactivateEventSource
    (
    -- * Creating a request
      DeactivateEventSource (..)
    , mkDeactivateEventSource
    -- ** Request lenses
    , desfName

    -- * Destructuring the response
    , DeactivateEventSourceResponse (..)
    , mkDeactivateEventSourceResponse
    ) where

import qualified Network.AWS.CloudWatchEvents.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeactivateEventSource' smart constructor.
newtype DeactivateEventSource = DeactivateEventSource'
  { name :: Types.Name
    -- ^ The name of the partner event source to deactivate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivateEventSource' value with any optional fields omitted.
mkDeactivateEventSource
    :: Types.Name -- ^ 'name'
    -> DeactivateEventSource
mkDeactivateEventSource name = DeactivateEventSource'{name}

-- | The name of the partner event source to deactivate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desfName :: Lens.Lens' DeactivateEventSource Types.Name
desfName = Lens.field @"name"
{-# INLINEABLE desfName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeactivateEventSource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeactivateEventSource where
        toHeaders DeactivateEventSource{..}
          = Core.pure ("X-Amz-Target", "AWSEvents.DeactivateEventSource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeactivateEventSource where
        toJSON DeactivateEventSource{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeactivateEventSource where
        type Rs DeactivateEventSource = DeactivateEventSourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeactivateEventSourceResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeactivateEventSourceResponse' smart constructor.
data DeactivateEventSourceResponse = DeactivateEventSourceResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeactivateEventSourceResponse' value with any optional fields omitted.
mkDeactivateEventSourceResponse
    :: DeactivateEventSourceResponse
mkDeactivateEventSourceResponse = DeactivateEventSourceResponse'
