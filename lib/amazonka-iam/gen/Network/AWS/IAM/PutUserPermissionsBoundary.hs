{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.PutUserPermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates the policy that is specified as the IAM user's permissions boundary. You can use an AWS managed policy or a customer managed policy to set the boundary for a user. Use the boundary to control the maximum permissions that the user can have. Setting a permissions boundary is an advanced feature that can affect the permissions for the user.
--
-- /Important:/ Policies that are used as permissions boundaries do not provide permissions. You must also attach a permissions policy to the user. To learn how the effective permissions for a user are evaluated, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_evaluation-logic.html IAM JSON Policy Evaluation Logic> in the IAM User Guide.
module Network.AWS.IAM.PutUserPermissionsBoundary
  ( -- * Creating a request
    PutUserPermissionsBoundary (..),
    mkPutUserPermissionsBoundary,

    -- ** Request lenses
    pupbUserName,
    pupbPermissionsBoundary,

    -- * Destructuring the response
    PutUserPermissionsBoundaryResponse (..),
    mkPutUserPermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutUserPermissionsBoundary' smart constructor.
data PutUserPermissionsBoundary = PutUserPermissionsBoundary'
  { -- | The name (friendly name, not ARN) of the IAM user for which you want to set the permissions boundary.
    userName :: Lude.Text,
    -- | The ARN of the policy that is used to set the permissions boundary for the user.
    permissionsBoundary :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutUserPermissionsBoundary' with the minimum fields required to make a request.
--
-- * 'userName' - The name (friendly name, not ARN) of the IAM user for which you want to set the permissions boundary.
-- * 'permissionsBoundary' - The ARN of the policy that is used to set the permissions boundary for the user.
mkPutUserPermissionsBoundary ::
  -- | 'userName'
  Lude.Text ->
  -- | 'permissionsBoundary'
  Lude.Text ->
  PutUserPermissionsBoundary
mkPutUserPermissionsBoundary pUserName_ pPermissionsBoundary_ =
  PutUserPermissionsBoundary'
    { userName = pUserName_,
      permissionsBoundary = pPermissionsBoundary_
    }

-- | The name (friendly name, not ARN) of the IAM user for which you want to set the permissions boundary.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupbUserName :: Lens.Lens' PutUserPermissionsBoundary Lude.Text
pupbUserName = Lens.lens (userName :: PutUserPermissionsBoundary -> Lude.Text) (\s a -> s {userName = a} :: PutUserPermissionsBoundary)
{-# DEPRECATED pupbUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | The ARN of the policy that is used to set the permissions boundary for the user.
--
-- /Note:/ Consider using 'permissionsBoundary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pupbPermissionsBoundary :: Lens.Lens' PutUserPermissionsBoundary Lude.Text
pupbPermissionsBoundary = Lens.lens (permissionsBoundary :: PutUserPermissionsBoundary -> Lude.Text) (\s a -> s {permissionsBoundary = a} :: PutUserPermissionsBoundary)
{-# DEPRECATED pupbPermissionsBoundary "Use generic-lens or generic-optics with 'permissionsBoundary' instead." #-}

instance Lude.AWSRequest PutUserPermissionsBoundary where
  type
    Rs PutUserPermissionsBoundary =
      PutUserPermissionsBoundaryResponse
  request = Req.postQuery iamService
  response = Res.receiveNull PutUserPermissionsBoundaryResponse'

instance Lude.ToHeaders PutUserPermissionsBoundary where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutUserPermissionsBoundary where
  toPath = Lude.const "/"

instance Lude.ToQuery PutUserPermissionsBoundary where
  toQuery PutUserPermissionsBoundary' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("PutUserPermissionsBoundary" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName,
        "PermissionsBoundary" Lude.=: permissionsBoundary
      ]

-- | /See:/ 'mkPutUserPermissionsBoundaryResponse' smart constructor.
data PutUserPermissionsBoundaryResponse = PutUserPermissionsBoundaryResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutUserPermissionsBoundaryResponse' with the minimum fields required to make a request.
mkPutUserPermissionsBoundaryResponse ::
  PutUserPermissionsBoundaryResponse
mkPutUserPermissionsBoundaryResponse =
  PutUserPermissionsBoundaryResponse'
