{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified role, including the role's path, GUID, ARN, and the role's trust policy that grants permission to assume the role. For more information about roles, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> .
module Network.AWS.IAM.GetRole
  ( -- * Creating a request
    GetRole (..),
    mkGetRole,

    -- ** Request lenses
    grRoleName,

    -- * Destructuring the response
    GetRoleResponse (..),
    mkGetRoleResponse,

    -- ** Response lenses
    grrsResponseStatus,
    grrsRole,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetRole' smart constructor.
newtype GetRole = GetRole' {roleName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRole' with the minimum fields required to make a request.
--
-- * 'roleName' - The name of the IAM role to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkGetRole ::
  -- | 'roleName'
  Lude.Text ->
  GetRole
mkGetRole pRoleName_ = GetRole' {roleName = pRoleName_}

-- | The name of the IAM role to get information about.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRoleName :: Lens.Lens' GetRole Lude.Text
grRoleName = Lens.lens (roleName :: GetRole -> Lude.Text) (\s a -> s {roleName = a} :: GetRole)
{-# DEPRECATED grRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

instance Lude.AWSRequest GetRole where
  type Rs GetRole = GetRoleResponse
  request = Req.postQuery iamService
  response =
    Res.receiveXMLWrapper
      "GetRoleResult"
      ( \s h x ->
          GetRoleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..@ "Role")
      )

instance Lude.ToHeaders GetRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetRole where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRole where
  toQuery GetRole' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName
      ]

-- | Contains the response to a successful 'GetRole' request.
--
-- /See:/ 'mkGetRoleResponse' smart constructor.
data GetRoleResponse = GetRoleResponse'
  { responseStatus :: Lude.Int,
    role' :: Role
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRoleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'role'' - A structure containing details about the IAM role.
mkGetRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'role''
  Role ->
  GetRoleResponse
mkGetRoleResponse pResponseStatus_ pRole_ =
  GetRoleResponse'
    { responseStatus = pResponseStatus_,
      role' = pRole_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRoleResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRoleResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A structure containing details about the IAM role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRole :: Lens.Lens' GetRoleResponse Role
grrsRole = Lens.lens (role' :: GetRoleResponse -> Role) (\s a -> s {role' = a} :: GetRoleResponse)
{-# DEPRECATED grrsRole "Use generic-lens or generic-optics with 'role'' instead." #-}
