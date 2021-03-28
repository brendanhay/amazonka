{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RemoveIpRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes IP address blocks from a directory.
module Network.AWS.DirectoryService.RemoveIpRoutes
    (
    -- * Creating a request
      RemoveIpRoutes (..)
    , mkRemoveIpRoutes
    -- ** Request lenses
    , rirDirectoryId
    , rirCidrIps

    -- * Destructuring the response
    , RemoveIpRoutesResponse (..)
    , mkRemoveIpRoutesResponse
    -- ** Response lenses
    , rirrrsResponseStatus
    ) where

import qualified Network.AWS.DirectoryService.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRemoveIpRoutes' smart constructor.
data RemoveIpRoutes = RemoveIpRoutes'
  { directoryId :: Types.DirectoryId
    -- ^ Identifier (ID) of the directory from which you want to remove the IP addresses.
  , cidrIps :: [Types.CidrIp]
    -- ^ IP address blocks that you want to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveIpRoutes' value with any optional fields omitted.
mkRemoveIpRoutes
    :: Types.DirectoryId -- ^ 'directoryId'
    -> RemoveIpRoutes
mkRemoveIpRoutes directoryId
  = RemoveIpRoutes'{directoryId, cidrIps = Core.mempty}

-- | Identifier (ID) of the directory from which you want to remove the IP addresses.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirDirectoryId :: Lens.Lens' RemoveIpRoutes Types.DirectoryId
rirDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE rirDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | IP address blocks that you want to remove.
--
-- /Note:/ Consider using 'cidrIps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirCidrIps :: Lens.Lens' RemoveIpRoutes [Types.CidrIp]
rirCidrIps = Lens.field @"cidrIps"
{-# INLINEABLE rirCidrIps #-}
{-# DEPRECATED cidrIps "Use generic-lens or generic-optics with 'cidrIps' instead"  #-}

instance Core.ToQuery RemoveIpRoutes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RemoveIpRoutes where
        toHeaders RemoveIpRoutes{..}
          = Core.pure
              ("X-Amz-Target", "DirectoryService_20150416.RemoveIpRoutes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RemoveIpRoutes where
        toJSON RemoveIpRoutes{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("CidrIps" Core..= cidrIps)])

instance Core.AWSRequest RemoveIpRoutes where
        type Rs RemoveIpRoutes = RemoveIpRoutesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RemoveIpRoutesResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRemoveIpRoutesResponse' smart constructor.
newtype RemoveIpRoutesResponse = RemoveIpRoutesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RemoveIpRoutesResponse' value with any optional fields omitted.
mkRemoveIpRoutesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RemoveIpRoutesResponse
mkRemoveIpRoutesResponse responseStatus
  = RemoveIpRoutesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrrsResponseStatus :: Lens.Lens' RemoveIpRoutesResponse Core.Int
rirrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rirrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
