{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.GetServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the servers in your server catalog.
--
-- Before you can describe your servers, you must import them using 'ImportServerCatalog' .
--
-- This operation returns paginated results.
module Network.AWS.SMS.GetServers
    (
    -- * Creating a request
      GetServers (..)
    , mkGetServers
    -- ** Request lenses
    , gsMaxResults
    , gsNextToken
    , gsVmServerAddressList

    -- * Destructuring the response
    , GetServersResponse (..)
    , mkGetServersResponse
    -- ** Response lenses
    , gsrrsLastModifiedOn
    , gsrrsNextToken
    , gsrrsServerCatalogStatus
    , gsrrsServerList
    , gsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SMS.Types as Types

-- | /See:/ 'mkGetServers' smart constructor.
data GetServers = GetServers'
  { maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  , vmServerAddressList :: Core.Maybe [Types.VmServerAddress]
    -- ^ The server addresses.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetServers' value with any optional fields omitted.
mkGetServers
    :: GetServers
mkGetServers
  = GetServers'{maxResults = Core.Nothing, nextToken = Core.Nothing,
                vmServerAddressList = Core.Nothing}

-- | The maximum number of results to return in a single call. The default value is 50. To retrieve the remaining results, make another call with the returned @NextToken@ value.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsMaxResults :: Lens.Lens' GetServers (Core.Maybe Core.Int)
gsMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gsMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsNextToken :: Lens.Lens' GetServers (Core.Maybe Types.NextToken)
gsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The server addresses.
--
-- /Note:/ Consider using 'vmServerAddressList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsVmServerAddressList :: Lens.Lens' GetServers (Core.Maybe [Types.VmServerAddress])
gsVmServerAddressList = Lens.field @"vmServerAddressList"
{-# INLINEABLE gsVmServerAddressList #-}
{-# DEPRECATED vmServerAddressList "Use generic-lens or generic-optics with 'vmServerAddressList' instead"  #-}

instance Core.ToQuery GetServers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetServers where
        toHeaders GetServers{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSServerMigrationService_V2016_10_24.GetServers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetServers where
        toJSON GetServers{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("vmServerAddressList" Core..=) Core.<$> vmServerAddressList])

instance Core.AWSRequest GetServers where
        type Rs GetServers = GetServersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetServersResponse' Core.<$>
                   (x Core..:? "lastModifiedOn") Core.<*> x Core..:? "nextToken"
                     Core.<*> x Core..:? "serverCatalogStatus"
                     Core.<*> x Core..:? "serverList"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetServers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"serverList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetServersResponse' smart constructor.
data GetServersResponse = GetServersResponse'
  { lastModifiedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the server was last modified.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token required to retrieve the next set of results. This value is null when there are no more results to return.
  , serverCatalogStatus :: Core.Maybe Types.ServerCatalogStatus
    -- ^ The status of the server catalog.
  , serverList :: Core.Maybe [Types.Server]
    -- ^ Information about the servers.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetServersResponse' value with any optional fields omitted.
mkGetServersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetServersResponse
mkGetServersResponse responseStatus
  = GetServersResponse'{lastModifiedOn = Core.Nothing,
                        nextToken = Core.Nothing, serverCatalogStatus = Core.Nothing,
                        serverList = Core.Nothing, responseStatus}

-- | The time when the server was last modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsLastModifiedOn :: Lens.Lens' GetServersResponse (Core.Maybe Core.NominalDiffTime)
gsrrsLastModifiedOn = Lens.field @"lastModifiedOn"
{-# INLINEABLE gsrrsLastModifiedOn #-}
{-# DEPRECATED lastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead"  #-}

-- | The token required to retrieve the next set of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsNextToken :: Lens.Lens' GetServersResponse (Core.Maybe Types.NextToken)
gsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The status of the server catalog.
--
-- /Note:/ Consider using 'serverCatalogStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsServerCatalogStatus :: Lens.Lens' GetServersResponse (Core.Maybe Types.ServerCatalogStatus)
gsrrsServerCatalogStatus = Lens.field @"serverCatalogStatus"
{-# INLINEABLE gsrrsServerCatalogStatus #-}
{-# DEPRECATED serverCatalogStatus "Use generic-lens or generic-optics with 'serverCatalogStatus' instead"  #-}

-- | Information about the servers.
--
-- /Note:/ Consider using 'serverList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsServerList :: Lens.Lens' GetServersResponse (Core.Maybe [Types.Server])
gsrrsServerList = Lens.field @"serverList"
{-# INLINEABLE gsrrsServerList #-}
{-# DEPRECATED serverList "Use generic-lens or generic-optics with 'serverList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsrrsResponseStatus :: Lens.Lens' GetServersResponse Core.Int
gsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
