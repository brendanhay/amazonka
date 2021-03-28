{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.GetDiscoverySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a short summary of discovered assets.
--
-- This API operation takes no request parameters and is called as is at the command prompt as shown in the example.
module Network.AWS.Discovery.GetDiscoverySummary
    (
    -- * Creating a request
      GetDiscoverySummary (..)
    , mkGetDiscoverySummary

    -- * Destructuring the response
    , GetDiscoverySummaryResponse (..)
    , mkGetDiscoverySummaryResponse
    -- ** Response lenses
    , gdsrrsAgentSummary
    , gdsrrsApplications
    , gdsrrsConnectorSummary
    , gdsrrsServers
    , gdsrrsServersMappedToApplications
    , gdsrrsServersMappedtoTags
    , gdsrrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetDiscoverySummary' smart constructor.
data GetDiscoverySummary = GetDiscoverySummary'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDiscoverySummary' value with any optional fields omitted.
mkGetDiscoverySummary
    :: GetDiscoverySummary
mkGetDiscoverySummary = GetDiscoverySummary'

instance Core.ToQuery GetDiscoverySummary where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetDiscoverySummary where
        toHeaders GetDiscoverySummary{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.GetDiscoverySummary")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetDiscoverySummary where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest GetDiscoverySummary where
        type Rs GetDiscoverySummary = GetDiscoverySummaryResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDiscoverySummaryResponse' Core.<$>
                   (x Core..:? "agentSummary") Core.<*> x Core..:? "applications"
                     Core.<*> x Core..:? "connectorSummary"
                     Core.<*> x Core..:? "servers"
                     Core.<*> x Core..:? "serversMappedToApplications"
                     Core.<*> x Core..:? "serversMappedtoTags"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDiscoverySummaryResponse' smart constructor.
data GetDiscoverySummaryResponse = GetDiscoverySummaryResponse'
  { agentSummary :: Core.Maybe Types.CustomerAgentInfo
    -- ^ Details about discovered agents, including agent status and health.
  , applications :: Core.Maybe Core.Integer
    -- ^ The number of applications discovered.
  , connectorSummary :: Core.Maybe Types.CustomerConnectorInfo
    -- ^ Details about discovered connectors, including connector status and health.
  , servers :: Core.Maybe Core.Integer
    -- ^ The number of servers discovered.
  , serversMappedToApplications :: Core.Maybe Core.Integer
    -- ^ The number of servers mapped to applications.
  , serversMappedtoTags :: Core.Maybe Core.Integer
    -- ^ The number of servers mapped to tags.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDiscoverySummaryResponse' value with any optional fields omitted.
mkGetDiscoverySummaryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDiscoverySummaryResponse
mkGetDiscoverySummaryResponse responseStatus
  = GetDiscoverySummaryResponse'{agentSummary = Core.Nothing,
                                 applications = Core.Nothing, connectorSummary = Core.Nothing,
                                 servers = Core.Nothing, serversMappedToApplications = Core.Nothing,
                                 serversMappedtoTags = Core.Nothing, responseStatus}

-- | Details about discovered agents, including agent status and health.
--
-- /Note:/ Consider using 'agentSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsAgentSummary :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Types.CustomerAgentInfo)
gdsrrsAgentSummary = Lens.field @"agentSummary"
{-# INLINEABLE gdsrrsAgentSummary #-}
{-# DEPRECATED agentSummary "Use generic-lens or generic-optics with 'agentSummary' instead"  #-}

-- | The number of applications discovered.
--
-- /Note:/ Consider using 'applications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsApplications :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
gdsrrsApplications = Lens.field @"applications"
{-# INLINEABLE gdsrrsApplications #-}
{-# DEPRECATED applications "Use generic-lens or generic-optics with 'applications' instead"  #-}

-- | Details about discovered connectors, including connector status and health.
--
-- /Note:/ Consider using 'connectorSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsConnectorSummary :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Types.CustomerConnectorInfo)
gdsrrsConnectorSummary = Lens.field @"connectorSummary"
{-# INLINEABLE gdsrrsConnectorSummary #-}
{-# DEPRECATED connectorSummary "Use generic-lens or generic-optics with 'connectorSummary' instead"  #-}

-- | The number of servers discovered.
--
-- /Note:/ Consider using 'servers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsServers :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
gdsrrsServers = Lens.field @"servers"
{-# INLINEABLE gdsrrsServers #-}
{-# DEPRECATED servers "Use generic-lens or generic-optics with 'servers' instead"  #-}

-- | The number of servers mapped to applications.
--
-- /Note:/ Consider using 'serversMappedToApplications' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsServersMappedToApplications :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
gdsrrsServersMappedToApplications = Lens.field @"serversMappedToApplications"
{-# INLINEABLE gdsrrsServersMappedToApplications #-}
{-# DEPRECATED serversMappedToApplications "Use generic-lens or generic-optics with 'serversMappedToApplications' instead"  #-}

-- | The number of servers mapped to tags.
--
-- /Note:/ Consider using 'serversMappedtoTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsServersMappedtoTags :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
gdsrrsServersMappedtoTags = Lens.field @"serversMappedtoTags"
{-# INLINEABLE gdsrrsServersMappedtoTags #-}
{-# DEPRECATED serversMappedtoTags "Use generic-lens or generic-optics with 'serversMappedtoTags' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdsrrsResponseStatus :: Lens.Lens' GetDiscoverySummaryResponse Core.Int
gdsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
