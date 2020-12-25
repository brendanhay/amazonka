{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.DeleteImagePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions for the specified private image. After you delete permissions for an image, AWS accounts to which you previously granted these permissions can no longer use the image.
module Network.AWS.AppStream.DeleteImagePermissions
  ( -- * Creating a request
    DeleteImagePermissions (..),
    mkDeleteImagePermissions,

    -- ** Request lenses
    dipName,
    dipSharedAccountId,

    -- * Destructuring the response
    DeleteImagePermissionsResponse (..),
    mkDeleteImagePermissionsResponse,

    -- ** Response lenses
    diprrsResponseStatus,
  )
where

import qualified Network.AWS.AppStream.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteImagePermissions' smart constructor.
data DeleteImagePermissions = DeleteImagePermissions'
  { -- | The name of the private image.
    name :: Types.Name,
    -- | The 12-digit identifier of the AWS account for which to delete image permissions.
    sharedAccountId :: Types.SharedAccountId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImagePermissions' value with any optional fields omitted.
mkDeleteImagePermissions ::
  -- | 'name'
  Types.Name ->
  -- | 'sharedAccountId'
  Types.SharedAccountId ->
  DeleteImagePermissions
mkDeleteImagePermissions name sharedAccountId =
  DeleteImagePermissions' {name, sharedAccountId}

-- | The name of the private image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipName :: Lens.Lens' DeleteImagePermissions Types.Name
dipName = Lens.field @"name"
{-# DEPRECATED dipName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The 12-digit identifier of the AWS account for which to delete image permissions.
--
-- /Note:/ Consider using 'sharedAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipSharedAccountId :: Lens.Lens' DeleteImagePermissions Types.SharedAccountId
dipSharedAccountId = Lens.field @"sharedAccountId"
{-# DEPRECATED dipSharedAccountId "Use generic-lens or generic-optics with 'sharedAccountId' instead." #-}

instance Core.FromJSON DeleteImagePermissions where
  toJSON DeleteImagePermissions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("SharedAccountId" Core..= sharedAccountId)
          ]
      )

instance Core.AWSRequest DeleteImagePermissions where
  type Rs DeleteImagePermissions = DeleteImagePermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "PhotonAdminProxyService.DeleteImagePermissions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteImagePermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteImagePermissionsResponse' smart constructor.
newtype DeleteImagePermissionsResponse = DeleteImagePermissionsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteImagePermissionsResponse' value with any optional fields omitted.
mkDeleteImagePermissionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteImagePermissionsResponse
mkDeleteImagePermissionsResponse responseStatus =
  DeleteImagePermissionsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diprrsResponseStatus :: Lens.Lens' DeleteImagePermissionsResponse Core.Int
diprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
