{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.DescribeServers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all configuration management servers that are identified with your account. Only the stored results from Amazon DynamoDB are returned. AWS OpsWorks CM does not query other services. 
--
-- This operation is synchronous. 
-- A @ResourceNotFoundException@ is thrown when the server does not exist. A @ValidationException@ is raised when parameters of the request are not valid. 
--
-- This operation returns paginated results.
module Network.AWS.OpsWorksCM.DescribeServers
    (
    -- * Creating a request
      DescribeServers (..)
    , mkDescribeServers
    -- ** Request lenses
    , dssMaxResults
    , dssNextToken
    , dssServerName

    -- * Destructuring the response
    , DescribeServersResponse (..)
    , mkDescribeServersResponse
    -- ** Response lenses
    , dsrfrsNextToken
    , dsrfrsServers
    , dsrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorksCM.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeServers' smart constructor.
data DescribeServers = DescribeServers'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ This is not currently implemented for @DescribeServers@ requests. 
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ This is not currently implemented for @DescribeServers@ requests. 
  , serverName :: Core.Maybe Types.ServerName
    -- ^ Describes the server with the specified ServerName.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeServers' value with any optional fields omitted.
mkDescribeServers
    :: DescribeServers
mkDescribeServers
  = DescribeServers'{maxResults = Core.Nothing,
                     nextToken = Core.Nothing, serverName = Core.Nothing}

-- | This is not currently implemented for @DescribeServers@ requests. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssMaxResults :: Lens.Lens' DescribeServers (Core.Maybe Core.Natural)
dssMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dssMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | This is not currently implemented for @DescribeServers@ requests. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssNextToken :: Lens.Lens' DescribeServers (Core.Maybe Types.NextToken)
dssNextToken = Lens.field @"nextToken"
{-# INLINEABLE dssNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Describes the server with the specified ServerName.
--
-- /Note:/ Consider using 'serverName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssServerName :: Lens.Lens' DescribeServers (Core.Maybe Types.ServerName)
dssServerName = Lens.field @"serverName"
{-# INLINEABLE dssServerName #-}
{-# DEPRECATED serverName "Use generic-lens or generic-optics with 'serverName' instead"  #-}

instance Core.ToQuery DescribeServers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeServers where
        toHeaders DescribeServers{..}
          = Core.pure
              ("X-Amz-Target", "OpsWorksCM_V2016_11_01.DescribeServers")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeServers where
        toJSON DescribeServers{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ServerName" Core..=) Core.<$> serverName])

instance Core.AWSRequest DescribeServers where
        type Rs DescribeServers = DescribeServersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeServersResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Servers" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeServers where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"servers" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeServersResponse' smart constructor.
data DescribeServersResponse = DescribeServersResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ This is not currently implemented for @DescribeServers@ requests. 
  , servers :: Core.Maybe [Types.Server]
    -- ^ Contains the response to a @DescribeServers@ request.
--
-- /For Chef Automate servers:/ If @DescribeServersResponse$Servers$EngineAttributes@ includes CHEF_MAJOR_UPGRADE_AVAILABLE, you can upgrade the Chef Automate server to Chef Automate 2. To be eligible for upgrade, a server running Chef Automate 1 must have had at least one successful maintenance run after November 1, 2019.
-- /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@ contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that is used by the Puppet API over TCP port number 8140. The CA certificate is also used to sign node certificates.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeServersResponse' value with any optional fields omitted.
mkDescribeServersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeServersResponse
mkDescribeServersResponse responseStatus
  = DescribeServersResponse'{nextToken = Core.Nothing,
                             servers = Core.Nothing, responseStatus}

-- | This is not currently implemented for @DescribeServers@ requests. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsNextToken :: Lens.Lens' DescribeServersResponse (Core.Maybe Core.Text)
dsrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Contains the response to a @DescribeServers@ request.
--
-- /For Chef Automate servers:/ If @DescribeServersResponse$Servers$EngineAttributes@ includes CHEF_MAJOR_UPGRADE_AVAILABLE, you can upgrade the Chef Automate server to Chef Automate 2. To be eligible for upgrade, a server running Chef Automate 1 must have had at least one successful maintenance run after November 1, 2019.
-- /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@ contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that is used by the Puppet API over TCP port number 8140. The CA certificate is also used to sign node certificates.
--
-- /Note:/ Consider using 'servers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsServers :: Lens.Lens' DescribeServersResponse (Core.Maybe [Types.Server])
dsrfrsServers = Lens.field @"servers"
{-# INLINEABLE dsrfrsServers #-}
{-# DEPRECATED servers "Use generic-lens or generic-optics with 'servers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeServersResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
