{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateMyUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's SSH public key.
--
-- __Required Permissions__ : To use this action, an IAM user must have self-management enabled or an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateMyUserProfile
  ( -- * Creating a request
    UpdateMyUserProfile (..),
    mkUpdateMyUserProfile,

    -- ** Request lenses
    umupSSHPublicKey,

    -- * Destructuring the response
    UpdateMyUserProfileResponse (..),
    mkUpdateMyUserProfileResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateMyUserProfile' smart constructor.
newtype UpdateMyUserProfile = UpdateMyUserProfile'
  { -- | The user's SSH public key.
    sshPublicKey :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMyUserProfile' with the minimum fields required to make a request.
--
-- * 'sshPublicKey' - The user's SSH public key.
mkUpdateMyUserProfile ::
  UpdateMyUserProfile
mkUpdateMyUserProfile =
  UpdateMyUserProfile' {sshPublicKey = Lude.Nothing}

-- | The user's SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umupSSHPublicKey :: Lens.Lens' UpdateMyUserProfile (Lude.Maybe Lude.Text)
umupSSHPublicKey = Lens.lens (sshPublicKey :: UpdateMyUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: UpdateMyUserProfile)
{-# DEPRECATED umupSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

instance Lude.AWSRequest UpdateMyUserProfile where
  type Rs UpdateMyUserProfile = UpdateMyUserProfileResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateMyUserProfileResponse'

instance Lude.ToHeaders UpdateMyUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateMyUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMyUserProfile where
  toJSON UpdateMyUserProfile' {..} =
    Lude.object
      (Lude.catMaybes [("SshPublicKey" Lude..=) Lude.<$> sshPublicKey])

instance Lude.ToPath UpdateMyUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateMyUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMyUserProfileResponse' smart constructor.
data UpdateMyUserProfileResponse = UpdateMyUserProfileResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMyUserProfileResponse' with the minimum fields required to make a request.
mkUpdateMyUserProfileResponse ::
  UpdateMyUserProfileResponse
mkUpdateMyUserProfileResponse = UpdateMyUserProfileResponse'
