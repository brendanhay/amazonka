{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.AssociateIpGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified IP access control group with the specified directory.
module Network.AWS.WorkSpaces.AssociateIpGroups
    (
    -- * Creating a request
      AssociateIpGroups (..)
    , mkAssociateIpGroups
    -- ** Request lenses
    , aigDirectoryId
    , aigGroupIds

    -- * Destructuring the response
    , AssociateIpGroupsResponse (..)
    , mkAssociateIpGroupsResponse
    -- ** Response lenses
    , aigrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkAssociateIpGroups' smart constructor.
data AssociateIpGroups = AssociateIpGroups'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , groupIds :: [Types.IpGroupId]
    -- ^ The identifiers of one or more IP access control groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateIpGroups' value with any optional fields omitted.
mkAssociateIpGroups
    :: Types.DirectoryId -- ^ 'directoryId'
    -> AssociateIpGroups
mkAssociateIpGroups directoryId
  = AssociateIpGroups'{directoryId, groupIds = Core.mempty}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigDirectoryId :: Lens.Lens' AssociateIpGroups Types.DirectoryId
aigDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE aigDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The identifiers of one or more IP access control groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigGroupIds :: Lens.Lens' AssociateIpGroups [Types.IpGroupId]
aigGroupIds = Lens.field @"groupIds"
{-# INLINEABLE aigGroupIds #-}
{-# DEPRECATED groupIds "Use generic-lens or generic-optics with 'groupIds' instead"  #-}

instance Core.ToQuery AssociateIpGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateIpGroups where
        toHeaders AssociateIpGroups{..}
          = Core.pure ("X-Amz-Target", "WorkspacesService.AssociateIpGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateIpGroups where
        toJSON AssociateIpGroups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("GroupIds" Core..= groupIds)])

instance Core.AWSRequest AssociateIpGroups where
        type Rs AssociateIpGroups = AssociateIpGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AssociateIpGroupsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateIpGroupsResponse' smart constructor.
newtype AssociateIpGroupsResponse = AssociateIpGroupsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateIpGroupsResponse' value with any optional fields omitted.
mkAssociateIpGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateIpGroupsResponse
mkAssociateIpGroupsResponse responseStatus
  = AssociateIpGroupsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrrsResponseStatus :: Lens.Lens' AssociateIpGroupsResponse Core.Int
aigrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE aigrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
