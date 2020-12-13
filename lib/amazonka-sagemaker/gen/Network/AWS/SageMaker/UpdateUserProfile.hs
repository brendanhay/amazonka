{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a user profile.
module Network.AWS.SageMaker.UpdateUserProfile
  ( -- * Creating a request
    UpdateUserProfile (..),
    mkUpdateUserProfile,

    -- ** Request lenses
    uupUserSettings,
    uupUserProfileName,
    uupDomainId,

    -- * Destructuring the response
    UpdateUserProfileResponse (..),
    mkUpdateUserProfileResponse,

    -- ** Response lenses
    uuprsUserProfileARN,
    uuprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { -- | A collection of settings.
    userSettings :: Lude.Maybe UserSettings,
    -- | The user profile name.
    userProfileName :: Lude.Text,
    -- | The domain ID.
    domainId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserProfile' with the minimum fields required to make a request.
--
-- * 'userSettings' - A collection of settings.
-- * 'userProfileName' - The user profile name.
-- * 'domainId' - The domain ID.
mkUpdateUserProfile ::
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'domainId'
  Lude.Text ->
  UpdateUserProfile
mkUpdateUserProfile pUserProfileName_ pDomainId_ =
  UpdateUserProfile'
    { userSettings = Lude.Nothing,
      userProfileName = pUserProfileName_,
      domainId = pDomainId_
    }

-- | A collection of settings.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserSettings :: Lens.Lens' UpdateUserProfile (Lude.Maybe UserSettings)
uupUserSettings = Lens.lens (userSettings :: UpdateUserProfile -> Lude.Maybe UserSettings) (\s a -> s {userSettings = a} :: UpdateUserProfile)
{-# DEPRECATED uupUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserProfileName :: Lens.Lens' UpdateUserProfile Lude.Text
uupUserProfileName = Lens.lens (userProfileName :: UpdateUserProfile -> Lude.Text) (\s a -> s {userProfileName = a} :: UpdateUserProfile)
{-# DEPRECATED uupUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupDomainId :: Lens.Lens' UpdateUserProfile Lude.Text
uupDomainId = Lens.lens (domainId :: UpdateUserProfile -> Lude.Text) (\s a -> s {domainId = a} :: UpdateUserProfile)
{-# DEPRECATED uupDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.AWSRequest UpdateUserProfile where
  type Rs UpdateUserProfile = UpdateUserProfileResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateUserProfileResponse'
            Lude.<$> (x Lude..?> "UserProfileArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserProfile where
  toJSON UpdateUserProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserSettings" Lude..=) Lude.<$> userSettings,
            Lude.Just ("UserProfileName" Lude..= userProfileName),
            Lude.Just ("DomainId" Lude..= domainId)
          ]
      )

instance Lude.ToPath UpdateUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { -- | The user profile Amazon Resource Name (ARN).
    userProfileARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'userProfileARN' - The user profile Amazon Resource Name (ARN).
-- * 'responseStatus' - The response status code.
mkUpdateUserProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserProfileResponse
mkUpdateUserProfileResponse pResponseStatus_ =
  UpdateUserProfileResponse'
    { userProfileARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user profile Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'userProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsUserProfileARN :: Lens.Lens' UpdateUserProfileResponse (Lude.Maybe Lude.Text)
uuprsUserProfileARN = Lens.lens (userProfileARN :: UpdateUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {userProfileARN = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsUserProfileARN "Use generic-lens or generic-optics with 'userProfileARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsResponseStatus :: Lens.Lens' UpdateUserProfileResponse Lude.Int
uuprsResponseStatus = Lens.lens (responseStatus :: UpdateUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserProfileResponse)
{-# DEPRECATED uuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
