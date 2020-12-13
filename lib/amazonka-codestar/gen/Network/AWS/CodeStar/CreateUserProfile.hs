{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.CreateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile for a user that includes user preferences, such as the display name and email address assocciated with the user, in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar.
module Network.AWS.CodeStar.CreateUserProfile
  ( -- * Creating a request
    CreateUserProfile (..),
    mkCreateUserProfile,

    -- ** Request lenses
    cupSshPublicKey,
    cupUserARN,
    cupEmailAddress,
    cupDisplayName,

    -- * Destructuring the response
    CreateUserProfileResponse (..),
    mkCreateUserProfileResponse,

    -- ** Response lenses
    cuprsLastModifiedTimestamp,
    cuprsSshPublicKey,
    cuprsUserARN,
    cuprsEmailAddress,
    cuprsDisplayName,
    cuprsCreatedTimestamp,
    cuprsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
    sshPublicKey :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userARN :: Lude.Text,
    -- | The email address that will be displayed as part of the user's profile in AWS CodeStar.
    emailAddress :: Lude.Sensitive Lude.Text,
    -- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
    displayName :: Lude.Sensitive Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserProfile' with the minimum fields required to make a request.
--
-- * 'sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user in IAM.
-- * 'emailAddress' - The email address that will be displayed as part of the user's profile in AWS CodeStar.
-- * 'displayName' - The name that will be displayed as the friendly name for the user in AWS CodeStar.
mkCreateUserProfile ::
  -- | 'userARN'
  Lude.Text ->
  -- | 'emailAddress'
  Lude.Sensitive Lude.Text ->
  -- | 'displayName'
  Lude.Sensitive Lude.Text ->
  CreateUserProfile
mkCreateUserProfile pUserARN_ pEmailAddress_ pDisplayName_ =
  CreateUserProfile'
    { sshPublicKey = Lude.Nothing,
      userARN = pUserARN_,
      emailAddress = pEmailAddress_,
      displayName = pDisplayName_
    }

-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSshPublicKey :: Lens.Lens' CreateUserProfile (Lude.Maybe Lude.Text)
cupSshPublicKey = Lens.lens (sshPublicKey :: CreateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: CreateUserProfile)
{-# DEPRECATED cupSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserARN :: Lens.Lens' CreateUserProfile Lude.Text
cupUserARN = Lens.lens (userARN :: CreateUserProfile -> Lude.Text) (\s a -> s {userARN = a} :: CreateUserProfile)
{-# DEPRECATED cupUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The email address that will be displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailAddress :: Lens.Lens' CreateUserProfile (Lude.Sensitive Lude.Text)
cupEmailAddress = Lens.lens (emailAddress :: CreateUserProfile -> Lude.Sensitive Lude.Text) (\s a -> s {emailAddress = a} :: CreateUserProfile)
{-# DEPRECATED cupEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDisplayName :: Lens.Lens' CreateUserProfile (Lude.Sensitive Lude.Text)
cupDisplayName = Lens.lens (displayName :: CreateUserProfile -> Lude.Sensitive Lude.Text) (\s a -> s {displayName = a} :: CreateUserProfile)
{-# DEPRECATED cupDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

instance Lude.AWSRequest CreateUserProfile where
  type Rs CreateUserProfile = CreateUserProfileResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Lude.<$> (x Lude..?> "lastModifiedTimestamp")
            Lude.<*> (x Lude..?> "sshPublicKey")
            Lude.<*> (x Lude..:> "userArn")
            Lude.<*> (x Lude..?> "emailAddress")
            Lude.<*> (x Lude..?> "displayName")
            Lude.<*> (x Lude..?> "createdTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.CreateUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sshPublicKey" Lude..=) Lude.<$> sshPublicKey,
            Lude.Just ("userArn" Lude..= userARN),
            Lude.Just ("emailAddress" Lude..= emailAddress),
            Lude.Just ("displayName" Lude..= displayName)
          ]
      )

instance Lude.ToPath CreateUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The date the user profile was last modified, in timestamp format.
    lastModifiedTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
    sshPublicKey :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the user in IAM.
    userARN :: Lude.Text,
    -- | The email address that is displayed as part of the user's profile in AWS CodeStar.
    emailAddress :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The name that is displayed as the friendly name for the user in AWS CodeStar.
    displayName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The date the user profile was created, in timestamp format.
    createdTimestamp :: Lude.Maybe Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedTimestamp' - The date the user profile was last modified, in timestamp format.
-- * 'sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user in IAM.
-- * 'emailAddress' - The email address that is displayed as part of the user's profile in AWS CodeStar.
-- * 'displayName' - The name that is displayed as the friendly name for the user in AWS CodeStar.
-- * 'createdTimestamp' - The date the user profile was created, in timestamp format.
-- * 'responseStatus' - The response status code.
mkCreateUserProfileResponse ::
  -- | 'userARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserProfileResponse
mkCreateUserProfileResponse pUserARN_ pResponseStatus_ =
  CreateUserProfileResponse'
    { lastModifiedTimestamp = Lude.Nothing,
      sshPublicKey = Lude.Nothing,
      userARN = pUserARN_,
      emailAddress = Lude.Nothing,
      displayName = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date the user profile was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsLastModifiedTimestamp :: Lens.Lens' CreateUserProfileResponse (Lude.Maybe Lude.Timestamp)
cuprsLastModifiedTimestamp = Lens.lens (lastModifiedTimestamp :: CreateUserProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimestamp = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsSshPublicKey :: Lens.Lens' CreateUserProfileResponse (Lude.Maybe Lude.Text)
cuprsSshPublicKey = Lens.lens (sshPublicKey :: CreateUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsUserARN :: Lens.Lens' CreateUserProfileResponse Lude.Text
cuprsUserARN = Lens.lens (userARN :: CreateUserProfileResponse -> Lude.Text) (\s a -> s {userARN = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsEmailAddress :: Lens.Lens' CreateUserProfileResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
cuprsEmailAddress = Lens.lens (emailAddress :: CreateUserProfileResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {emailAddress = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsDisplayName :: Lens.Lens' CreateUserProfileResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
cuprsDisplayName = Lens.lens (displayName :: CreateUserProfileResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {displayName = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The date the user profile was created, in timestamp format.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsCreatedTimestamp :: Lens.Lens' CreateUserProfileResponse (Lude.Maybe Lude.Timestamp)
cuprsCreatedTimestamp = Lens.lens (createdTimestamp :: CreateUserProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsResponseStatus :: Lens.Lens' CreateUserProfileResponse Lude.Int
cuprsResponseStatus = Lens.lens (responseStatus :: CreateUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
