{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.DeleteMailboxPermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes permissions granted to a member (user or group).
module Network.AWS.WorkMail.DeleteMailboxPermissions
  ( -- * Creating a request
    DeleteMailboxPermissions (..),
    mkDeleteMailboxPermissions,

    -- ** Request lenses
    dmpOrganizationId,
    dmpEntityId,
    dmpGranteeId,

    -- * Destructuring the response
    DeleteMailboxPermissionsResponse (..),
    mkDeleteMailboxPermissionsResponse,

    -- ** Response lenses
    dmprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkDeleteMailboxPermissions' smart constructor.
data DeleteMailboxPermissions = DeleteMailboxPermissions'
  { -- | The identifier of the organization under which the member (user or group) exists.
    organizationId :: Types.OrganizationId,
    -- | The identifier of the member (user or group) that owns the mailbox.
    entityId :: Types.EntityId,
    -- | The identifier of the member (user or group) for which to delete granted permissions.
    granteeId :: Types.GranteeId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMailboxPermissions' value with any optional fields omitted.
mkDeleteMailboxPermissions ::
  -- | 'organizationId'
  Types.OrganizationId ->
  -- | 'entityId'
  Types.EntityId ->
  -- | 'granteeId'
  Types.GranteeId ->
  DeleteMailboxPermissions
mkDeleteMailboxPermissions organizationId entityId granteeId =
  DeleteMailboxPermissions' {organizationId, entityId, granteeId}

-- | The identifier of the organization under which the member (user or group) exists.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpOrganizationId :: Lens.Lens' DeleteMailboxPermissions Types.OrganizationId
dmpOrganizationId = Lens.field @"organizationId"
{-# DEPRECATED dmpOrganizationId "Use generic-lens or generic-optics with 'organizationId' instead." #-}

-- | The identifier of the member (user or group) that owns the mailbox.
--
-- /Note:/ Consider using 'entityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpEntityId :: Lens.Lens' DeleteMailboxPermissions Types.EntityId
dmpEntityId = Lens.field @"entityId"
{-# DEPRECATED dmpEntityId "Use generic-lens or generic-optics with 'entityId' instead." #-}

-- | The identifier of the member (user or group) for which to delete granted permissions.
--
-- /Note:/ Consider using 'granteeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmpGranteeId :: Lens.Lens' DeleteMailboxPermissions Types.GranteeId
dmpGranteeId = Lens.field @"granteeId"
{-# DEPRECATED dmpGranteeId "Use generic-lens or generic-optics with 'granteeId' instead." #-}

instance Core.FromJSON DeleteMailboxPermissions where
  toJSON DeleteMailboxPermissions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("OrganizationId" Core..= organizationId),
            Core.Just ("EntityId" Core..= entityId),
            Core.Just ("GranteeId" Core..= granteeId)
          ]
      )

instance Core.AWSRequest DeleteMailboxPermissions where
  type Rs DeleteMailboxPermissions = DeleteMailboxPermissionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "WorkMailService.DeleteMailboxPermissions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteMailboxPermissionsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteMailboxPermissionsResponse' smart constructor.
newtype DeleteMailboxPermissionsResponse = DeleteMailboxPermissionsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteMailboxPermissionsResponse' value with any optional fields omitted.
mkDeleteMailboxPermissionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteMailboxPermissionsResponse
mkDeleteMailboxPermissionsResponse responseStatus =
  DeleteMailboxPermissionsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmprrsResponseStatus :: Lens.Lens' DeleteMailboxPermissionsResponse Core.Int
dmprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dmprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
