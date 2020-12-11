{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.CreateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new user profile.
--
-- __Required Permissions__ : To use this action, an IAM user must have an attached policy that explicitly grants permissions. For more information about user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.CreateUserProfile
  ( -- * Creating a request
    CreateUserProfile (..),
    mkCreateUserProfile,

    -- ** Request lenses
    cupAllowSelfManagement,
    cupSSHPublicKey,
    cupSSHUsername,
    cupIAMUserARN,

    -- * Destructuring the response
    CreateUserProfileResponse (..),
    mkCreateUserProfileResponse,

    -- ** Response lenses
    cuprsIAMUserARN,
    cuprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { allowSelfManagement ::
      Lude.Maybe Lude.Bool,
    sshPublicKey :: Lude.Maybe Lude.Text,
    sshUsername :: Lude.Maybe Lude.Text,
    iamUserARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserProfile' with the minimum fields required to make a request.
--
-- * 'allowSelfManagement' - Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User's Public SSH Key> .
-- * 'iamUserARN' - The user's IAM ARN; this can also be a federated user's ARN.
-- * 'sshPublicKey' - The user's public SSH key.
-- * 'sshUsername' - The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
mkCreateUserProfile ::
  -- | 'iamUserARN'
  Lude.Text ->
  CreateUserProfile
mkCreateUserProfile pIAMUserARN_ =
  CreateUserProfile'
    { allowSelfManagement = Lude.Nothing,
      sshPublicKey = Lude.Nothing,
      sshUsername = Lude.Nothing,
      iamUserARN = pIAMUserARN_
    }

-- | Whether users can specify their own SSH public key through the My Settings page. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/security-settingsshkey.html Setting an IAM User's Public SSH Key> .
--
-- /Note:/ Consider using 'allowSelfManagement' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAllowSelfManagement :: Lens.Lens' CreateUserProfile (Lude.Maybe Lude.Bool)
cupAllowSelfManagement = Lens.lens (allowSelfManagement :: CreateUserProfile -> Lude.Maybe Lude.Bool) (\s a -> s {allowSelfManagement = a} :: CreateUserProfile)
{-# DEPRECATED cupAllowSelfManagement "Use generic-lens or generic-optics with 'allowSelfManagement' instead." #-}

-- | The user's public SSH key.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSSHPublicKey :: Lens.Lens' CreateUserProfile (Lude.Maybe Lude.Text)
cupSSHPublicKey = Lens.lens (sshPublicKey :: CreateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: CreateUserProfile)
{-# DEPRECATED cupSSHPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The user's SSH user name. The allowable characters are [a-z], [A-Z], [0-9], '-', and '_'. If the specified name includes other punctuation marks, AWS OpsWorks Stacks removes them. For example, @my.name@ will be changed to @myname@ . If you do not specify an SSH user name, AWS OpsWorks Stacks generates one from the IAM user name.
--
-- /Note:/ Consider using 'sshUsername' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSSHUsername :: Lens.Lens' CreateUserProfile (Lude.Maybe Lude.Text)
cupSSHUsername = Lens.lens (sshUsername :: CreateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshUsername = a} :: CreateUserProfile)
{-# DEPRECATED cupSSHUsername "Use generic-lens or generic-optics with 'sshUsername' instead." #-}

-- | The user's IAM ARN; this can also be a federated user's ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupIAMUserARN :: Lens.Lens' CreateUserProfile Lude.Text
cupIAMUserARN = Lens.lens (iamUserARN :: CreateUserProfile -> Lude.Text) (\s a -> s {iamUserARN = a} :: CreateUserProfile)
{-# DEPRECATED cupIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

instance Lude.AWSRequest CreateUserProfile where
  type Rs CreateUserProfile = CreateUserProfileResponse
  request = Req.postJSON opsWorksService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Lude.<$> (x Lude..?> "IamUserArn") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.CreateUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AllowSelfManagement" Lude..=) Lude.<$> allowSelfManagement,
            ("SshPublicKey" Lude..=) Lude.<$> sshPublicKey,
            ("SshUsername" Lude..=) Lude.<$> sshUsername,
            Lude.Just ("IamUserArn" Lude..= iamUserARN)
          ]
      )

instance Lude.ToPath CreateUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserProfile where
  toQuery = Lude.const Lude.mempty

-- | Contains the response to a @CreateUserProfile@ request.
--
-- /See:/ 'mkCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { iamUserARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'iamUserARN' - The user's IAM ARN.
-- * 'responseStatus' - The response status code.
mkCreateUserProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserProfileResponse
mkCreateUserProfileResponse pResponseStatus_ =
  CreateUserProfileResponse'
    { iamUserARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user's IAM ARN.
--
-- /Note:/ Consider using 'iamUserARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsIAMUserARN :: Lens.Lens' CreateUserProfileResponse (Lude.Maybe Lude.Text)
cuprsIAMUserARN = Lens.lens (iamUserARN :: CreateUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {iamUserARN = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsIAMUserARN "Use generic-lens or generic-optics with 'iamUserARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsResponseStatus :: Lens.Lens' CreateUserProfileResponse Lude.Int
cuprsResponseStatus = Lens.lens (responseStatus :: CreateUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
