{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dslrrsResponseStatus,
    dslrrsDeletionTaskId,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteServiceLinkedRole' smart constructor.
newtype DeleteServiceLinkedRole = DeleteServiceLinkedRole'
  { roleName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServiceLinkedRole' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the service-linked role to be deleted.
mkDeleteServiceLinkedRole ::
  -- | 'roleName'
  Lude.Text ->
  DeleteServiceLinkedRole
mkDeleteServiceLinkedRole pRoleName_ =
  DeleteServiceLinkedRole' {roleName = pRoleName_}

-- | The name of the service-linked role to be deleted.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrRoleName :: Lens.Lens' DeleteServiceLinkedRole Lude.Text
dslrRoleName = Lens.lens (roleName :: DeleteServiceLinkedRole -> Lude.Text) (\s a -> s {roleName = a} :: DeleteServiceLinkedRole)
{-# DEPRECATED dslrRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest DeleteServiceLinkedRole where
  type Rs DeleteServiceLinkedRole = DeleteServiceLinkedRoleResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "DeleteServiceLinkedRoleResult"
      ( \s h x ->
          DeleteServiceLinkedRoleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "DeletionTaskId")
      )

instance Lude.ToHeaders DeleteServiceLinkedRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteServiceLinkedRole where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteServiceLinkedRole where
  toQuery DeleteServiceLinkedRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteServiceLinkedRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName
      ]

-- | /See:/ 'mkDeleteServiceLinkedRoleResponse' smart constructor.
data DeleteServiceLinkedRoleResponse = DeleteServiceLinkedRoleResponse'
  { responseStatus ::
      Lude.Int,
    deletionTaskId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteServiceLinkedRoleResponse' with the minimum fields required to make a request.
--
-- * 'deletionTaskId' - The deletion task identifier that you can use to check the status of the deletion. This identifier is returned in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
-- * 'responseStatus' - The response status code.
mkDeleteServiceLinkedRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'deletionTaskId'
  Lude.Text ->
  DeleteServiceLinkedRoleResponse
mkDeleteServiceLinkedRoleResponse pResponseStatus_ pDeletionTaskId_ =
  DeleteServiceLinkedRoleResponse'
    { responseStatus =
        pResponseStatus_,
      deletionTaskId = pDeletionTaskId_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrrsResponseStatus :: Lens.Lens' DeleteServiceLinkedRoleResponse Lude.Int
dslrrsResponseStatus = Lens.lens (responseStatus :: DeleteServiceLinkedRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteServiceLinkedRoleResponse)
{-# DEPRECATED dslrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The deletion task identifier that you can use to check the status of the deletion. This identifier is returned in the format @task/aws-service-role/<service-principal-name>/<role-name>/<task-uuid>@ .
--
-- /Note:/ Consider using 'deletionTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dslrrsDeletionTaskId :: Lens.Lens' DeleteServiceLinkedRoleResponse Lude.Text
dslrrsDeletionTaskId = Lens.lens (deletionTaskId :: DeleteServiceLinkedRoleResponse -> Lude.Text) (\s a -> s {deletionTaskId = a} :: DeleteServiceLinkedRoleResponse)
{-# DEPRECATED dslrrsDeletionTaskId "Use generic-lens or generic-optics with 'deletionTaskId' instead." #-}
