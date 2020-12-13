{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.DeleteUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a user profile.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.DeleteUserProfile
  ( -- * Creating a request
    DeleteUserProfile (..),
    mkDeleteUserProfile,

    -- ** Request lenses
    dupIAMUserARN,

    -- * Destructuring the response
    DeleteUserProfileResponse (..),
    mkDeleteUserProfileResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteUserProfile' smart constructor.
newtype DeleteUserProfile = DeleteUserProfile'
  { -- | The user's IAM ARN. This can also be a federated user's ARN.
    iamUserARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserProfile' with the minimum fields required to make a request.
--
-- * 'iamUserARN' - The user's IAM ARN. This can also be a federated user's ARN.
mkDeleteUserProfile ::
  -- | 'iamUserARN'
  Lude.Text ->
  DeleteUserProfile
mkDeleteUserProfile pIAMUserARN_ =
  DeleteUserProfile' {iamUserARN = pIAMUserARN_}

-- | The user's IAM ARN. This can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupIAMUserARN :: Lens.Lens' DeleteUserProfile Lude.Text
dupIAMUserARN = Lens.lens (iamUserARN :: DeleteUserProfile -> Lude.Text) (\s a -> s {iamUserARN = a} :: DeleteUserProfile)
{-# DEPRECATED dupIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

instance Lude.AWSRequest DeleteUserProfile where
  type Rs DeleteUserProfile = DeleteUserProfileResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull DeleteUserProfileResponse'

instance Lude.ToHeaders DeleteUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.DeleteUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteUserProfile where
  toJSON DeleteUserProfile' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("IamUserArn" Lude..= iamUserARN)])

instance Lude.ToPath DeleteUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteUserProfileResponse' smart constructor.
data DeleteUserProfileResponse = DeleteUserProfileResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteUserProfileResponse' with the minimum fields required to make a request.
mkDeleteUserProfileResponse ::
  DeleteUserProfileResponse
mkDeleteUserProfileResponse = DeleteUserProfileResponse'
