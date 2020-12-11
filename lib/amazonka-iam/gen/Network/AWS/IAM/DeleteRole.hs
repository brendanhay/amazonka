{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified role. The role must not have any policies attached. For more information about roles, go to <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> .
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the role you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.
module Network.AWS.IAM.DeleteRole
  ( -- * Creating a request
    DeleteRole (..),
    mkDeleteRole,

    -- ** Request lenses
    drRoleName,

    -- * Destructuring the response
    DeleteRoleResponse (..),
    mkDeleteRoleResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRole' smart constructor.
newtype DeleteRole = DeleteRole' {roleName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRole' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the role to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkDeleteRole ::
  -- | 'roleName'
  Lude.Text ->
  DeleteRole
mkDeleteRole pRoleName_ = DeleteRole' {roleName = pRoleName_}

-- | The name of the role to delete.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRoleName :: Lens.Lens' DeleteRole Lude.Text
drRoleName = Lens.lens (roleName :: DeleteRole -> Lude.Text) (\s a -> s {roleName = a} :: DeleteRole)
{-# DEPRECATED drRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest DeleteRole where
  type Rs DeleteRole = DeleteRoleResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteRoleResponse'

instance Lude.ToHeaders DeleteRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRole where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRole where
  toQuery DeleteRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName
      ]

-- | /See:/ 'mkDeleteRoleResponse' smart constructor.
data DeleteRoleResponse = DeleteRoleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRoleResponse' with the minimum fields required to make a request.
mkDeleteRoleResponse ::
  DeleteRoleResponse
mkDeleteRoleResponse = DeleteRoleResponse'
