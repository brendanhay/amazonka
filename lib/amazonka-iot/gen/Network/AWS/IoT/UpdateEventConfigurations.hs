{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateEventConfigurations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the event configurations.
module Network.AWS.IoT.UpdateEventConfigurations
    (
    -- * Creating a request
      UpdateEventConfigurations (..)
    , mkUpdateEventConfigurations
    -- ** Request lenses
    , uecEventConfigurations

    -- * Destructuring the response
    , UpdateEventConfigurationsResponse (..)
    , mkUpdateEventConfigurationsResponse
    -- ** Response lenses
    , uecrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEventConfigurations' smart constructor.
newtype UpdateEventConfigurations = UpdateEventConfigurations'
  { eventConfigurations :: Core.Maybe (Core.HashMap Types.EventType Types.Configuration)
    -- ^ The new event configuration values.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEventConfigurations' value with any optional fields omitted.
mkUpdateEventConfigurations
    :: UpdateEventConfigurations
mkUpdateEventConfigurations
  = UpdateEventConfigurations'{eventConfigurations = Core.Nothing}

-- | The new event configuration values.
--
-- /Note:/ Consider using 'eventConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecEventConfigurations :: Lens.Lens' UpdateEventConfigurations (Core.Maybe (Core.HashMap Types.EventType Types.Configuration))
uecEventConfigurations = Lens.field @"eventConfigurations"
{-# INLINEABLE uecEventConfigurations #-}
{-# DEPRECATED eventConfigurations "Use generic-lens or generic-optics with 'eventConfigurations' instead"  #-}

instance Core.ToQuery UpdateEventConfigurations where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateEventConfigurations where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON UpdateEventConfigurations where
        toJSON UpdateEventConfigurations{..}
          = Core.object
              (Core.catMaybes
                 [("eventConfigurations" Core..=) Core.<$> eventConfigurations])

instance Core.AWSRequest UpdateEventConfigurations where
        type Rs UpdateEventConfigurations =
             UpdateEventConfigurationsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath = "/event-configurations",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateEventConfigurationsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateEventConfigurationsResponse' smart constructor.
newtype UpdateEventConfigurationsResponse = UpdateEventConfigurationsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEventConfigurationsResponse' value with any optional fields omitted.
mkUpdateEventConfigurationsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateEventConfigurationsResponse
mkUpdateEventConfigurationsResponse responseStatus
  = UpdateEventConfigurationsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uecrrsResponseStatus :: Lens.Lens' UpdateEventConfigurationsResponse Core.Int
uecrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uecrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
