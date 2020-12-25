{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteServiceLinkedRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits a service-linked role deletion request and returns a @DeletionTaskId@ , which you can use to check the status of the deletion. Before you call this operation, confirm that the role has no active sessions and that any resources used by the role in the linked service are deleted. If you call this operation more than once for the same service-linked role and an earlier deletion task is not complete, then the @DeletionTaskId@ of the earlier request is returned.
--
-- If you submit a deletion request for a service-linked role whose linked service is still accessing a resource, then the deletion task fails. If it fails, the 'GetServiceLinkedRoleDeletionStatus' API operation returns the reason for the failure, usually including the resources that must be deleted. To delete the service-linked role, you must first remove those resources from the linked service and then submit the deletion request again. Resources are specific to the service that is linked to the role. For more information about removing resources from a service, see the <http://docs.aws.amazon.com/ AWS documentation> for your service.
-- For more information about service-linked roles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_terms-and-concepts.html#iam-term-service-linked-role Roles Terms and Concepts: AWS Service-Linked Role> in the /IAM User Guide/ .
module Network.AWS.IAM.DeleteServiceLinkedRole
  ( -- * Creating a request
    DeleteServiceLinkedRole (..),
    mkDeleteServiceLinkedRole,

    -- ** Request lenses
    dslrRoleName,

    -- * Destructuring the response
    DeleteServiceLinkedRoleResponse (..),
    mkDeleteServiceLinkedRoleResponse,

    -- ** Response lenses
    dslrrrsDeletionTaskId,
    dslrrrsResponseStatus,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteServiceLinkedRole' smart constructor.
newtype DeleteServiceLinkedRole = DeleteServiceLinkedRole'
  { -- | The name of the service-linked role to be deleted.
    roleName :: Types.RoleName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceLinkedRole' value with any optional fields omitted.
mkDeleteServiceLinkedRole ::
  -- | 'roleName'
  Types.RoleName ->
  DeleteServiceLinkedRole
mkDeleteServiceLinkedRole roleName =
  DeleteServiceLinkedRole' {roleName}

-- | The name of the service-linked role to be deleted.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrRoleName :: Lens.Lens' DeleteServiceLinkedRole Types.RoleName
dslrRoleName = Lens.field @"roleName"
{-# DEPRECATED dslrRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Core.AWSRequest DeleteServiceLinkedRole where
  type Rs DeleteServiceLinkedRole = DeleteServiceLinkedRoleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DeleteServiceLinkedRole")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "RoleName" roleName)
            )
      }
  response =
    Response.receiveXMLWrapper
      "DeleteServiceLinkedRoleResult"
      ( \s h x ->
          DeleteServiceLinkedRoleResponse'
            Core.<$> (x Core..@ "DeletionTaskId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteServiceLinkedRoleResponse' smart constructor.
data DeleteServiceLinkedRoleResponse = DeleteServiceLinkedRoleResponse'
  { -- | The deletion task identifier that you can use to check the status of the deletion. This identifier is returned in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
    deletionTaskId :: Types.DeletionTaskId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteServiceLinkedRoleResponse' value with any optional fields omitted.
mkDeleteServiceLinkedRoleResponse ::
  -- | 'deletionTaskId'
  Types.DeletionTaskId ->
  -- | 'responseStatus'
  Core.Int ->
  DeleteServiceLinkedRoleResponse
mkDeleteServiceLinkedRoleResponse deletionTaskId responseStatus =
  DeleteServiceLinkedRoleResponse' {deletionTaskId, responseStatus}

-- | The deletion task identifier that you can use to check the status of the deletion. This identifier is returned in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
--
-- /Note:/ Consider using 'deletionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrrrsDeletionTaskId :: Lens.Lens' DeleteServiceLinkedRoleResponse Types.DeletionTaskId
dslrrrsDeletionTaskId = Lens.field @"deletionTaskId"
{-# DEPRECATED dslrrrsDeletionTaskId "Use generic-lens or generic-optics with 'deletionTaskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrrrsResponseStatus :: Lens.Lens' DeleteServiceLinkedRoleResponse Core.Int
dslrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dslrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
