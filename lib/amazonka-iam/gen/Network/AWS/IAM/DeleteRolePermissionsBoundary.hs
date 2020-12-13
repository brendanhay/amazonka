{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteRolePermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM role.
--
-- /Important:/ Deleting the permissions boundary for a role might increase its permissions. For example, it might allow anyone who assumes the role to perform all the actions granted in its permissions policies.
module Network.AWS.IAM.DeleteRolePermissionsBoundary
  ( -- * Creating a request
    DeleteRolePermissionsBoundary (..),
    mkDeleteRolePermissionsBoundary,

    -- ** Request lenses
    drpbRoleName,

    -- * Destructuring the response
    DeleteRolePermissionsBoundaryResponse (..),
    mkDeleteRolePermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRolePermissionsBoundary' smart constructor.
newtype DeleteRolePermissionsBoundary = DeleteRolePermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM role from which you want to remove the permissions boundary.
    roleName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRolePermissionsBoundary' with the minimum fields required to make a request.
--
-- * 'roleName' - The name (friendly name, not ARN) of the IAM role from which you want to remove the permissions boundary.
mkDeleteRolePermissionsBoundary ::
  -- | 'roleName'
  Lude.Text ->
  DeleteRolePermissionsBoundary
mkDeleteRolePermissionsBoundary pRoleName_ =
  DeleteRolePermissionsBoundary' {roleName = pRoleName_}

-- | The name (friendly name, not ARN) of the IAM role from which you want to remove the permissions boundary.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpbRoleName :: Lens.Lens' DeleteRolePermissionsBoundary Lude.Text
drpbRoleName = Lens.lens (roleName :: DeleteRolePermissionsBoundary -> Lude.Text) (\s a -> s {roleName = a} :: DeleteRolePermissionsBoundary)
{-# DEPRECATED drpbRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest DeleteRolePermissionsBoundary where
  type
    Rs DeleteRolePermissionsBoundary =
      DeleteRolePermissionsBoundaryResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteRolePermissionsBoundaryResponse'

instance Lude.ToHeaders DeleteRolePermissionsBoundary where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRolePermissionsBoundary where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRolePermissionsBoundary where
  toQuery DeleteRolePermissionsBoundary' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteRolePermissionsBoundary" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName
      ]

-- | /See:/ 'mkDeleteRolePermissionsBoundaryResponse' smart constructor.
data DeleteRolePermissionsBoundaryResponse = DeleteRolePermissionsBoundaryResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRolePermissionsBoundaryResponse' with the minimum fields required to make a request.
mkDeleteRolePermissionsBoundaryResponse ::
  DeleteRolePermissionsBoundaryResponse
mkDeleteRolePermissionsBoundaryResponse =
  DeleteRolePermissionsBoundaryResponse'
