{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.UpdateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user's profile in AWS CodeStar. The user profile is not project-specific. Information in the user profile is displayed wherever the user's information appears to other users in AWS CodeStar.
module Network.AWS.CodeStar.UpdateUserProfile
  ( -- * Creating a request
    UpdateUserProfile (..),
    mkUpdateUserProfile,

    -- ** Request lenses
    uupSshPublicKey,
    uupEmailAddress,
    uupDisplayName,
    uupUserARN,

    -- * Destructuring the response
    UpdateUserProfileResponse (..),
    mkUpdateUserProfileResponse,

    -- ** Response lenses
    uuprsLastModifiedTimestamp,
    uuprsSshPublicKey,
    uuprsEmailAddress,
    uuprsDisplayName,
    uuprsCreatedTimestamp,
    uuprsResponseStatus,
    uuprsUserARN,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { sshPublicKey ::
      Lude.Maybe Lude.Text,
    emailAddress :: Lude.Maybe (Lude.Sensitive Lude.Text),
    displayName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    userARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserProfile' with the minimum fields required to make a request.
--
-- * 'displayName' - The name that is displayed as the friendly name for the user in AWS CodeStar.
-- * 'emailAddress' - The email address that is displayed as part of the user's profile in AWS CodeStar.
-- * 'sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
-- * 'userARN' - The name that will be displayed as the friendly name for the user in AWS CodeStar.
mkUpdateUserProfile ::
  -- | 'userARN'
  Lude.Text ->
  UpdateUserProfile
mkUpdateUserProfile pUserARN_ =
  UpdateUserProfile'
    { sshPublicKey = Lude.Nothing,
      emailAddress = Lude.Nothing,
      displayName = Lude.Nothing,
      userARN = pUserARN_
    }

-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSshPublicKey :: Lens.Lens' UpdateUserProfile (Lude.Maybe Lude.Text)
uupSshPublicKey = Lens.lens (sshPublicKey :: UpdateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: UpdateUserProfile)
{-# DEPRECATED uupSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupEmailAddress :: Lens.Lens' UpdateUserProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
uupEmailAddress = Lens.lens (emailAddress :: UpdateUserProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {emailAddress = a} :: UpdateUserProfile)
{-# DEPRECATED uupEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupDisplayName :: Lens.Lens' UpdateUserProfile (Lude.Maybe (Lude.Sensitive Lude.Text))
uupDisplayName = Lens.lens (displayName :: UpdateUserProfile -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {displayName = a} :: UpdateUserProfile)
{-# DEPRECATED uupDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserARN :: Lens.Lens' UpdateUserProfile Lude.Text
uupUserARN = Lens.lens (userARN :: UpdateUserProfile -> Lude.Text) (\s a -> s {userARN = a} :: UpdateUserProfile)
{-# DEPRECATED uupUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.AWSRequest UpdateUserProfile where
  type Rs UpdateUserProfile = UpdateUserProfileResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateUserProfileResponse'
            Lude.<$> (x Lude..?> "lastModifiedTimestamp")
            Lude.<*> (x Lude..?> "sshPublicKey")
            Lude.<*> (x Lude..?> "emailAddress")
            Lude.<*> (x Lude..?> "displayName")
            Lude.<*> (x Lude..?> "createdTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "userArn")
      )

instance Lude.ToHeaders UpdateUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.UpdateUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sshPublicKey" Lude..=) Lude.<$> sshPublicKey,
            ("emailAddress" Lude..=) Lude.<$> emailAddress,
            ("displayName" Lude..=) Lude.<$> displayName,
            Lude.Just ("userArn" Lude..= userARN)
          ]
      )

instance Lude.ToPath UpdateUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { lastModifiedTimestamp ::
      Lude.Maybe Lude.Timestamp,
    sshPublicKey :: Lude.Maybe Lude.Text,
    emailAddress ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    displayName ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    createdTimestamp ::
      Lude.Maybe Lude.Timestamp,
    responseStatus :: Lude.Int,
    userARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'createdTimestamp' - The date the user profile was created, in timestamp format.
-- * 'displayName' - The name that is displayed as the friendly name for the user in AWS CodeStar.
-- * 'emailAddress' - The email address that is displayed as part of the user's profile in AWS CodeStar.
-- * 'lastModifiedTimestamp' - The date the user profile was last modified, in timestamp format.
-- * 'responseStatus' - The response status code.
-- * 'sshPublicKey' - The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user in IAM.
mkUpdateUserProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'userARN'
  Lude.Text ->
  UpdateUserProfileResponse
mkUpdateUserProfileResponse pResponseStatus_ pUserARN_ =
  UpdateUserProfileResponse'
    { lastModifiedTimestamp = Lude.Nothing,
      sshPublicKey = Lude.Nothing,
      emailAddress = Lude.Nothing,
      displayName = Lude.Nothing,
      createdTimestamp = Lude.Nothing,
      responseStatus = pResponseStatus_,
      userARN = pUserARN_
    }

-- | The date the user profile was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsLastModifiedTimestamp :: Lens.Lens' UpdateUserProfileResponse (Lude.Maybe Lude.Timestamp)
uuprsLastModifiedTimestamp = Lens.lens (lastModifiedTimestamp :: UpdateUserProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTimestamp = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsSshPublicKey :: Lens.Lens' UpdateUserProfileResponse (Lude.Maybe Lude.Text)
uuprsSshPublicKey = Lens.lens (sshPublicKey :: UpdateUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsEmailAddress :: Lens.Lens' UpdateUserProfileResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
uuprsEmailAddress = Lens.lens (emailAddress :: UpdateUserProfileResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {emailAddress = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsDisplayName :: Lens.Lens' UpdateUserProfileResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
uuprsDisplayName = Lens.lens (displayName :: UpdateUserProfileResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {displayName = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The date the user profile was created, in timestamp format.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsCreatedTimestamp :: Lens.Lens' UpdateUserProfileResponse (Lude.Maybe Lude.Timestamp)
uuprsCreatedTimestamp = Lens.lens (createdTimestamp :: UpdateUserProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsResponseStatus :: Lens.Lens' UpdateUserProfileResponse Lude.Int
uuprsResponseStatus = Lens.lens (responseStatus :: UpdateUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsUserARN :: Lens.Lens' UpdateUserProfileResponse Lude.Text
uuprsUserARN = Lens.lens (userARN :: UpdateUserProfileResponse -> Lude.Text) (\s a -> s {userARN = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}
