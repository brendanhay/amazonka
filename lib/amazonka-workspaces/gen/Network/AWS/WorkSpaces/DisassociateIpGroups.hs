{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DisassociateIpGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified IP access control group from the specified directory.
module Network.AWS.WorkSpaces.DisassociateIpGroups
    (
    -- * Creating a request
      DisassociateIpGroups (..)
    , mkDisassociateIpGroups
    -- ** Request lenses
    , digDirectoryId
    , digGroupIds

    -- * Destructuring the response
    , DisassociateIpGroupsResponse (..)
    , mkDisassociateIpGroupsResponse
    -- ** Response lenses
    , digrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDisassociateIpGroups' smart constructor.
data DisassociateIpGroups = DisassociateIpGroups'
  { directoryId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , groupIds :: [Types.IpGroupId]
    -- ^ The identifiers of one or more IP access control groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateIpGroups' value with any optional fields omitted.
mkDisassociateIpGroups
    :: Types.DirectoryId -- ^ 'directoryId'
    -> DisassociateIpGroups
mkDisassociateIpGroups directoryId
  = DisassociateIpGroups'{directoryId, groupIds = Core.mempty}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'directoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digDirectoryId :: Lens.Lens' DisassociateIpGroups Types.DirectoryId
digDirectoryId = Lens.field @"directoryId"
{-# INLINEABLE digDirectoryId #-}
{-# DEPRECATED directoryId "Use generic-lens or generic-optics with 'directoryId' instead"  #-}

-- | The identifiers of one or more IP access control groups.
--
-- /Note:/ Consider using 'groupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digGroupIds :: Lens.Lens' DisassociateIpGroups [Types.IpGroupId]
digGroupIds = Lens.field @"groupIds"
{-# INLINEABLE digGroupIds #-}
{-# DEPRECATED groupIds "Use generic-lens or generic-optics with 'groupIds' instead"  #-}

instance Core.ToQuery DisassociateIpGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DisassociateIpGroups where
        toHeaders DisassociateIpGroups{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DisassociateIpGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DisassociateIpGroups where
        toJSON DisassociateIpGroups{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DirectoryId" Core..= directoryId),
                  Core.Just ("GroupIds" Core..= groupIds)])

instance Core.AWSRequest DisassociateIpGroups where
        type Rs DisassociateIpGroups = DisassociateIpGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DisassociateIpGroupsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisassociateIpGroupsResponse' smart constructor.
newtype DisassociateIpGroupsResponse = DisassociateIpGroupsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DisassociateIpGroupsResponse' value with any optional fields omitted.
mkDisassociateIpGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DisassociateIpGroupsResponse
mkDisassociateIpGroupsResponse responseStatus
  = DisassociateIpGroupsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
digrrsResponseStatus :: Lens.Lens' DisassociateIpGroupsResponse Core.Int
digrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE digrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
