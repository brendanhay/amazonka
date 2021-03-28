{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifySelfservicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the self-service WorkSpace management capabilities for your users. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users> .
module Network.AWS.WorkSpaces.ModifySelfservicePermissions
    (
    -- * Creating a request
      ModifySelfservicePermissions (..)
    , mkModifySelfservicePermissions
    -- ** Request lenses
    , mspResourceId
    , mspSelfservicePermissions

    -- * Destructuring the response
    , ModifySelfservicePermissionsResponse (..)
    , mkModifySelfservicePermissionsResponse
    -- ** Response lenses
    , msprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifySelfservicePermissions' smart constructor.
data ModifySelfservicePermissions = ModifySelfservicePermissions'
  { resourceId :: Types.DirectoryId
    -- ^ The identifier of the directory.
  , selfservicePermissions :: Types.SelfservicePermissions
    -- ^ The permissions to enable or disable self-service capabilities.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySelfservicePermissions' value with any optional fields omitted.
mkModifySelfservicePermissions
    :: Types.DirectoryId -- ^ 'resourceId'
    -> Types.SelfservicePermissions -- ^ 'selfservicePermissions'
    -> ModifySelfservicePermissions
mkModifySelfservicePermissions resourceId selfservicePermissions
  = ModifySelfservicePermissions'{resourceId, selfservicePermissions}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mspResourceId :: Lens.Lens' ModifySelfservicePermissions Types.DirectoryId
mspResourceId = Lens.field @"resourceId"
{-# INLINEABLE mspResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The permissions to enable or disable self-service capabilities.
--
-- /Note:/ Consider using 'selfservicePermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mspSelfservicePermissions :: Lens.Lens' ModifySelfservicePermissions Types.SelfservicePermissions
mspSelfservicePermissions = Lens.field @"selfservicePermissions"
{-# INLINEABLE mspSelfservicePermissions #-}
{-# DEPRECATED selfservicePermissions "Use generic-lens or generic-optics with 'selfservicePermissions' instead"  #-}

instance Core.ToQuery ModifySelfservicePermissions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifySelfservicePermissions where
        toHeaders ModifySelfservicePermissions{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.ModifySelfservicePermissions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifySelfservicePermissions where
        toJSON ModifySelfservicePermissions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just
                    ("SelfservicePermissions" Core..= selfservicePermissions)])

instance Core.AWSRequest ModifySelfservicePermissions where
        type Rs ModifySelfservicePermissions =
             ModifySelfservicePermissionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ModifySelfservicePermissionsResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifySelfservicePermissionsResponse' smart constructor.
newtype ModifySelfservicePermissionsResponse = ModifySelfservicePermissionsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifySelfservicePermissionsResponse' value with any optional fields omitted.
mkModifySelfservicePermissionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifySelfservicePermissionsResponse
mkModifySelfservicePermissionsResponse responseStatus
  = ModifySelfservicePermissionsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msprrsResponseStatus :: Lens.Lens' ModifySelfservicePermissionsResponse Core.Int
msprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE msprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
