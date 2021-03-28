{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Shares or unshares an image with one account by specifying whether that account has permission to copy the image. If the copy image permission is granted, the image is shared with that account. If the copy image permission is revoked, the image is unshared with the account. For more information about sharing images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/share-custom-image.html Share or Unshare a Custom WorkSpaces Image> .
module Network.AWS.WorkSpaces.UpdateWorkspaceImagePermission
    (
    -- * Creating a request
      UpdateWorkspaceImagePermission (..)
    , mkUpdateWorkspaceImagePermission
    -- ** Request lenses
    , uwipImageId
    , uwipAllowCopyImage
    , uwipSharedAccountId

    -- * Destructuring the response
    , UpdateWorkspaceImagePermissionResponse (..)
    , mkUpdateWorkspaceImagePermissionResponse
    -- ** Response lenses
    , uwiprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkUpdateWorkspaceImagePermission' smart constructor.
data UpdateWorkspaceImagePermission = UpdateWorkspaceImagePermission'
  { imageId :: Types.WorkspaceImageId
    -- ^ The identifier of the image.
  , allowCopyImage :: Core.Bool
    -- ^ The permission to copy the image. This permission can be revoked only after an image has been shared.
  , sharedAccountId :: Types.SharedAccountId
    -- ^ The identifier of the AWS account to share or unshare the image with.
--
-- /Important:/ Before sharing the image, confirm that you are sharing to the correct AWS account ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkspaceImagePermission' value with any optional fields omitted.
mkUpdateWorkspaceImagePermission
    :: Types.WorkspaceImageId -- ^ 'imageId'
    -> Core.Bool -- ^ 'allowCopyImage'
    -> Types.SharedAccountId -- ^ 'sharedAccountId'
    -> UpdateWorkspaceImagePermission
mkUpdateWorkspaceImagePermission imageId allowCopyImage
  sharedAccountId
  = UpdateWorkspaceImagePermission'{imageId, allowCopyImage,
                                    sharedAccountId}

-- | The identifier of the image.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwipImageId :: Lens.Lens' UpdateWorkspaceImagePermission Types.WorkspaceImageId
uwipImageId = Lens.field @"imageId"
{-# INLINEABLE uwipImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The permission to copy the image. This permission can be revoked only after an image has been shared.
--
-- /Note:/ Consider using 'allowCopyImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwipAllowCopyImage :: Lens.Lens' UpdateWorkspaceImagePermission Core.Bool
uwipAllowCopyImage = Lens.field @"allowCopyImage"
{-# INLINEABLE uwipAllowCopyImage #-}
{-# DEPRECATED allowCopyImage "Use generic-lens or generic-optics with 'allowCopyImage' instead"  #-}

-- | The identifier of the AWS account to share or unshare the image with.
--
-- /Important:/ Before sharing the image, confirm that you are sharing to the correct AWS account ID.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwipSharedAccountId :: Lens.Lens' UpdateWorkspaceImagePermission Types.SharedAccountId
uwipSharedAccountId = Lens.field @"sharedAccountId"
{-# INLINEABLE uwipSharedAccountId #-}
{-# DEPRECATED sharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead"  #-}

instance Core.ToQuery UpdateWorkspaceImagePermission where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateWorkspaceImagePermission where
        toHeaders UpdateWorkspaceImagePermission{..}
          = Core.pure
              ("X-Amz-Target",
               "WorkspacesService.UpdateWorkspaceImagePermission")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateWorkspaceImagePermission where
        toJSON UpdateWorkspaceImagePermission{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ImageId" Core..= imageId),
                  Core.Just ("AllowCopyImage" Core..= allowCopyImage),
                  Core.Just ("SharedAccountId" Core..= sharedAccountId)])

instance Core.AWSRequest UpdateWorkspaceImagePermission where
        type Rs UpdateWorkspaceImagePermission =
             UpdateWorkspaceImagePermissionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateWorkspaceImagePermissionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateWorkspaceImagePermissionResponse' smart constructor.
newtype UpdateWorkspaceImagePermissionResponse = UpdateWorkspaceImagePermissionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateWorkspaceImagePermissionResponse' value with any optional fields omitted.
mkUpdateWorkspaceImagePermissionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateWorkspaceImagePermissionResponse
mkUpdateWorkspaceImagePermissionResponse responseStatus
  = UpdateWorkspaceImagePermissionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uwiprrsResponseStatus :: Lens.Lens' UpdateWorkspaceImagePermissionResponse Core.Int
uwiprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uwiprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
