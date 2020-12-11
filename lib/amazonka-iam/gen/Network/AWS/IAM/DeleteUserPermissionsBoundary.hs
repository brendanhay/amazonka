{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteUserPermissionsBoundary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM user.
--
-- /Important:/ Deleting the permissions boundary for a user might increase its permissions by allowing the user to perform all the actions granted in its permissions policies.
module Network.AWS.IAM.DeleteUserPermissionsBoundary
  ( -- * Creating a request
    DeleteUserPermissionsBoundary (..),
    mkDeleteUserPermissionsBoundary,

    -- ** Request lenses
    dupbUserName,

    -- * Destructuring the response
    DeleteUserPermissionsBoundaryResponse (..),
    mkDeleteUserPermissionsBoundaryResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserPermissionsBoundary' smart constructor.
newtype DeleteUserPermissionsBoundary = DeleteUserPermissionsBoundary'
  { userName ::
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

-- | Creates a value of 'DeleteUserPermissionsBoundary' with the minimum fields required to make a request.
--
-- * 'userName' - The name (friendly name, not ARN) of the IAM user from which you want to remove the permissions boundary.
mkDeleteUserPermissionsBoundary ::
  -- | 'userName'
  Lude.Text ->
  DeleteUserPermissionsBoundary
mkDeleteUserPermissionsBoundary pUserName_ =
  DeleteUserPermissionsBoundary' {userName = pUserName_}

-- | The name (friendly name, not ARN) of the IAM user from which you want to remove the permissions boundary.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupbUserName :: Lens.Lens' DeleteUserPermissionsBoundary Lude.Text
dupbUserName = Lens.lens (userName :: DeleteUserPermissionsBoundary -> Lude.Text) (\s a -> s {userName = a} :: DeleteUserPermissionsBoundary)
{-# DEPRECATED dupbUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest DeleteUserPermissionsBoundary where
  type
    Rs DeleteUserPermissionsBoundary =
      DeleteUserPermissionsBoundaryResponse
  request = Req.postQuery iamService
  response = Res.receiveNull DeleteUserPermissionsBoundaryResponse'

instance Lude.ToHeaders DeleteUserPermissionsBoundary where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteUserPermissionsBoundary where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserPermissionsBoundary where
  toQuery DeleteUserPermissionsBoundary' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteUserPermissionsBoundary" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkDeleteUserPermissionsBoundaryResponse' smart constructor.
data DeleteUserPermissionsBoundaryResponse = DeleteUserPermissionsBoundaryResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserPermissionsBoundaryResponse' with the minimum fields required to make a request.
mkDeleteUserPermissionsBoundaryResponse ::
  DeleteUserPermissionsBoundaryResponse
mkDeleteUserPermissionsBoundaryResponse =
  DeleteUserPermissionsBoundaryResponse'
