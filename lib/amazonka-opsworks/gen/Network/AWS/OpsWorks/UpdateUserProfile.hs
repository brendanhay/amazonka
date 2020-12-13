{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified user profile.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateUserProfile
  ( -- * Creating a request
    UpdateUserProfile (..),
    mkUpdateUserProfile,

    -- ** Request lenses
    uupAllowSelfManagement,
    uupSSHPublicKey,
    uupSSHUsername,
    uupIAMUserARN,

    -- * Destructuring the response
    UpdateUserProfileResponse (..),
    mkUpdateUserProfileResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
    allowSelfManagement :: Lude.Maybe Lude.Bool,
    -- | The user's new SSH public key.
    sshPublicKey :: Lude.Maybe Lude.Text,
    -- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
    sshUsername :: Lude.Maybe Lude.Text,
    -- | The user IAM ARN. This can also be a federated user's ARN.
    iamUserARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserProfile' with the minimum fields required to make a request.
--
-- * 'allowSelfManagement' - Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
-- * 'sshPublicKey' - The user's new SSH public key.
-- * 'sshUsername' - The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
-- * 'iamUserARN' - The user IAM ARN. This can also be a federated user's ARN.
mkUpdateUserProfile ::
  -- | 'iamUserARN'
  Lude.Text ->
  UpdateUserProfile
mkUpdateUserProfile pIAMUserARN_ =
  UpdateUserProfile'
    { allowSelfManagement = Lude.Nothing,
      sshPublicKey = Lude.Nothing,
      sshUsername = Lude.Nothing,
      iamUserARN = pIAMUserARN_
    }

-- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Managing User Permissions> .
--
-- /Note:/ Consider using 'allowSelfManagement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupAllowSelfManagement :: Lens.Lens' UpdateUserProfile (Lude.Maybe Lude.Bool)
uupAllowSelfManagement = Lens.lens (allowSelfManagement :: UpdateUserProfile -> Lude.Maybe Lude.Bool) (\s a -> s {allowSelfManagement = a} :: UpdateUserProfile)
{-# DEPRECATED uupAllowSelfManagement "Use generic-lens or generic-optics with 'allowSelfManagement' instead." #-}

-- | The user's new SSH public key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSSHPublicKey :: Lens.Lens' UpdateUserProfile (Lude.Maybe Lude.Text)
uupSSHPublicKey = Lens.lens (sshPublicKey :: UpdateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: UpdateUserProfile)
{-# DEPRECATED uupSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSSHUsername :: Lens.Lens' UpdateUserProfile (Lude.Maybe Lude.Text)
uupSSHUsername = Lens.lens (sshUsername :: UpdateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshUsername = a} :: UpdateUserProfile)
{-# DEPRECATED uupSSHUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

-- | The user IAM ARN. This can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupIAMUserARN :: Lens.Lens' UpdateUserProfile Lude.Text
uupIAMUserARN = Lens.lens (iamUserARN :: UpdateUserProfile -> Lude.Text) (\s a -> s {iamUserARN = a} :: UpdateUserProfile)
{-# DEPRECATED uupIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

instance Lude.AWSRequest UpdateUserProfile where
  type Rs UpdateUserProfile = UpdateUserProfileResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateUserProfileResponse'

instance Lude.ToHeaders UpdateUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AllowSelfManagement" Lude..=) Lude.<$> allowSelfManagement,
            ("SshPublicKey" Lude..=) Lude.<$> sshPublicKey,
            ("SshUsername" Lude..=) Lude.<$> sshUsername,
            Lude.Just ("IamUserArn" Lude..= iamUserARN)
          ]
      )

instance Lude.ToPath UpdateUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserProfileResponse' with the minimum fields required to make a request.
mkUpdateUserProfileResponse ::
  UpdateUserProfileResponse
mkUpdateUserProfileResponse = UpdateUserProfileResponse'
