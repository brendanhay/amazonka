{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify the default properties used to create WorkSpaces.
module Network.AWS.WorkSpaces.ModifyWorkspaceCreationProperties
    (
    -- * Creating a request
      ModifyWorkspaceCreationProperties (..)
    , mkModifyWorkspaceCreationProperties
    -- ** Request lenses
    , mwcpResourceId
    , mwcpWorkspaceCreationProperties

    -- * Destructuring the response
    , ModifyWorkspaceCreationPropertiesResponse (..)
    , mkModifyWorkspaceCreationPropertiesResponse
    -- ** Response lenses
    , mwcprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkModifyWorkspaceCreationProperties' smart constructor.
data ModifyWorkspaceCreationProperties = ModifyWorkspaceCreationProperties'
  { resourceId :: Types.ResourceId
    -- ^ The identifier of the directory.
  , workspaceCreationProperties :: Types.WorkspaceCreationProperties
    -- ^ The default properties for creating WorkSpaces.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceCreationProperties' value with any optional fields omitted.
mkModifyWorkspaceCreationProperties
    :: Types.ResourceId -- ^ 'resourceId'
    -> Types.WorkspaceCreationProperties -- ^ 'workspaceCreationProperties'
    -> ModifyWorkspaceCreationProperties
mkModifyWorkspaceCreationProperties resourceId
  workspaceCreationProperties
  = ModifyWorkspaceCreationProperties'{resourceId,
                                       workspaceCreationProperties}

-- | The identifier of the directory.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcpResourceId :: Lens.Lens' ModifyWorkspaceCreationProperties Types.ResourceId
mwcpResourceId = Lens.field @"resourceId"
{-# INLINEABLE mwcpResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | The default properties for creating WorkSpaces.
--
-- /Note:/ Consider using 'workspaceCreationProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcpWorkspaceCreationProperties :: Lens.Lens' ModifyWorkspaceCreationProperties Types.WorkspaceCreationProperties
mwcpWorkspaceCreationProperties = Lens.field @"workspaceCreationProperties"
{-# INLINEABLE mwcpWorkspaceCreationProperties #-}
{-# DEPRECATED workspaceCreationProperties "Use generic-lens or generic-optics with 'workspaceCreationProperties' instead"  #-}

instance Core.ToQuery ModifyWorkspaceCreationProperties where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ModifyWorkspaceCreationProperties where
        toHeaders ModifyWorkspaceCreationProperties{..}
          = Core.pure
              ("X-Amz-Target",
               "WorkspacesService.ModifyWorkspaceCreationProperties")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ModifyWorkspaceCreationProperties where
        toJSON ModifyWorkspaceCreationProperties{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just
                    ("WorkspaceCreationProperties" Core..=
                       workspaceCreationProperties)])

instance Core.AWSRequest ModifyWorkspaceCreationProperties where
        type Rs ModifyWorkspaceCreationProperties =
             ModifyWorkspaceCreationPropertiesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 ModifyWorkspaceCreationPropertiesResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyWorkspaceCreationPropertiesResponse' smart constructor.
newtype ModifyWorkspaceCreationPropertiesResponse = ModifyWorkspaceCreationPropertiesResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyWorkspaceCreationPropertiesResponse' value with any optional fields omitted.
mkModifyWorkspaceCreationPropertiesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ModifyWorkspaceCreationPropertiesResponse
mkModifyWorkspaceCreationPropertiesResponse responseStatus
  = ModifyWorkspaceCreationPropertiesResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwcprrsResponseStatus :: Lens.Lens' ModifyWorkspaceCreationPropertiesResponse Core.Int
mwcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE mwcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
