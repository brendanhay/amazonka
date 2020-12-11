{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutRolePermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM role's permissions boundary. You can use an AWS managed policy or a customer managed policy to set the boundary for a role. Use the boundary to control the maximum permissions that the role can have. Setting a permissions boundary is an advanced feature that can affect the permissions for the role.
--
-- You cannot set the boundary for a service-linked role.
-- /Important:/ Policies used as permissions boundaries do not provide permissions. You must also attach a permissions policy to the role. To learn how the effective permissions for a role are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON Policy Evaluation Logic> in the IAM User Guide.
module Network.AWS.IAM.PutRolePermissionsBoundary
  ( -- * Creating a request
    PutRolePermissionsBoundary (..),
    mkPutRolePermissionsBoundary,

    -- ** Request lenses
    prpbRoleName,
    prpbPermissionsBoundary,

    -- * Destructuring the response
    PutRolePermissionsBoundaryResponse (..),
    mkPutRolePermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutRolePermissionsBoundary' smart constructor.
data PutRolePermissionsBoundary = PutRolePermissionsBoundary'
  { roleName ::
      Lude.Text,
    permissionsBoundary :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRolePermissionsBoundary' with the minimum fields required to make a request.
--
-- * 'permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for the role.
-- * 'roleName' - The name (friendly name, not ARN) of the IAM role for which you want to set the permissions boundary.
mkPutRolePermissionsBoundary ::
  -- | 'roleName'
  Lude.Text ->
  -- | 'permissionsBoundary'
  Lude.Text ->
  PutRolePermissionsBoundary
mkPutRolePermissionsBoundary pRoleName_ pPermissionsBoundary_ =
  PutRolePermissionsBoundary'
    { roleName = pRoleName_,
      permissionsBoundary = pPermissionsBoundary_
    }

-- | The name (friendly name, not ARN) of the IAM role for which you want to set the permissions boundary.
--
-- /Note:/ Consider using 'roleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpbRoleName :: Lens.Lens' PutRolePermissionsBoundary Lude.Text
prpbRoleName = Lens.lens (roleName :: PutRolePermissionsBoundary -> Lude.Text) (\s a -> s {roleName = a} :: PutRolePermissionsBoundary)
{-# DEPRECATED prpbRoleName "Use generic-lens or generic-optics with 'roleName' instead." #-}

-- | The ARN of the policy that is used to set the permissions boundary for the role.
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prpbPermissionsBoundary :: Lens.Lens' PutRolePermissionsBoundary Lude.Text
prpbPermissionsBoundary = Lens.lens (permissionsBoundary :: PutRolePermissionsBoundary -> Lude.Text) (\s a -> s {permissionsBoundary = a} :: PutRolePermissionsBoundary)
{-# DEPRECATED prpbPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

instance Lude.AWSRequest PutRolePermissionsBoundary where
  type
    Rs PutRolePermissionsBoundary =
      PutRolePermissionsBoundaryResponse
  request = Req.postQuery iamService
  response = Res.receiveNull PutRolePermissionsBoundaryResponse'

instance Lude.ToHeaders PutRolePermissionsBoundary where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutRolePermissionsBoundary where
  toPath = Lude.const "/"

instance Lude.ToQuery PutRolePermissionsBoundary where
  toQuery PutRolePermissionsBoundary' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PutRolePermissionsBoundary" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "RoleName" Lude.=: roleName,
        "PermissionsBoundary" Lude.=: permissionsBoundary
      ]

-- | /See:/ 'mkPutRolePermissionsBoundaryResponse' smart constructor.
data PutRolePermissionsBoundaryResponse = PutRolePermissionsBoundaryResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutRolePermissionsBoundaryResponse' with the minimum fields required to make a request.
mkPutRolePermissionsBoundaryResponse ::
  PutRolePermissionsBoundaryResponse
mkPutRolePermissionsBoundaryResponse =
  PutRolePermissionsBoundaryResponse'
