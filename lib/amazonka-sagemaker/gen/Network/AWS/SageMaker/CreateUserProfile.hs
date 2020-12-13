{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a user profile. A user profile represents a single user within a domain, and is the main way to reference a "person" for the purposes of sharing, reporting, and other user-oriented features. This entity is created when a user onboards to Amazon SageMaker Studio. If an administrator invites a person by email or imports them from SSO, a user profile is automatically created. A user profile is the primary holder of settings for an individual user and has a reference to the user's private Amazon Elastic File System (EFS) home directory.
module Network.AWS.SageMaker.CreateUserProfile
  ( -- * Creating a request
    CreateUserProfile (..),
    mkCreateUserProfile,

    -- ** Request lenses
    cupUserSettings,
    cupSingleSignOnUserValue,
    cupUserProfileName,
    cupSingleSignOnUserIdentifier,
    cupDomainId,
    cupTags,

    -- * Destructuring the response
    CreateUserProfileResponse (..),
    mkCreateUserProfileResponse,

    -- ** Response lenses
    cuprsUserProfileARN,
    cuprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | A collection of settings.
    userSettings :: Lude.Maybe UserSettings,
    -- | The username of the associated AWS Single Sign-On User for this UserProfile. If the Domain's AuthMode is SSO, this field is required, and must match a valid username of a user in your directory. If the Domain's AuthMode is not SSO, this field cannot be specified.
    singleSignOnUserValue :: Lude.Maybe Lude.Text,
    -- | A name for the UserProfile.
    userProfileName :: Lude.Text,
    -- | A specifier for the type of value specified in SingleSignOnUserValue. Currently, the only supported value is "UserName". If the Domain's AuthMode is SSO, this field is required. If the Domain's AuthMode is not SSO, this field cannot be specified.
    singleSignOnUserIdentifier :: Lude.Maybe Lude.Text,
    -- | The ID of the associated Domain.
    domainId :: Lude.Text,
    -- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserProfile' with the minimum fields required to make a request.
--
-- * 'userSettings' - A collection of settings.
-- * 'singleSignOnUserValue' - The username of the associated AWS Single Sign-On User for this UserProfile. If the Domain's AuthMode is SSO, this field is required, and must match a valid username of a user in your directory. If the Domain's AuthMode is not SSO, this field cannot be specified.
-- * 'userProfileName' - A name for the UserProfile.
-- * 'singleSignOnUserIdentifier' - A specifier for the type of value specified in SingleSignOnUserValue. Currently, the only supported value is "UserName". If the Domain's AuthMode is SSO, this field is required. If the Domain's AuthMode is not SSO, this field cannot be specified.
-- * 'domainId' - The ID of the associated Domain.
-- * 'tags' - Each tag consists of a key and an optional value. Tag keys must be unique per resource.
mkCreateUserProfile ::
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'domainId'
  Lude.Text ->
  CreateUserProfile
mkCreateUserProfile pUserProfileName_ pDomainId_ =
  CreateUserProfile'
    { userSettings = Lude.Nothing,
      singleSignOnUserValue = Lude.Nothing,
      userProfileName = pUserProfileName_,
      singleSignOnUserIdentifier = Lude.Nothing,
      domainId = pDomainId_,
      tags = Lude.Nothing
    }

-- | A collection of settings.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserSettings :: Lens.Lens' CreateUserProfile (Lude.Maybe UserSettings)
cupUserSettings = Lens.lens (userSettings :: CreateUserProfile -> Lude.Maybe UserSettings) (\s a -> s {userSettings = a} :: CreateUserProfile)
{-# DEPRECATED cupUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

-- | The username of the associated AWS Single Sign-On User for this UserProfile. If the Domain's AuthMode is SSO, this field is required, and must match a valid username of a user in your directory. If the Domain's AuthMode is not SSO, this field cannot be specified.
--
-- /Note:/ Consider using 'singleSignOnUserValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSingleSignOnUserValue :: Lens.Lens' CreateUserProfile (Lude.Maybe Lude.Text)
cupSingleSignOnUserValue = Lens.lens (singleSignOnUserValue :: CreateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {singleSignOnUserValue = a} :: CreateUserProfile)
{-# DEPRECATED cupSingleSignOnUserValue "Use generic-lens or generic-optics with 'singleSignOnUserValue' instead." #-}

-- | A name for the UserProfile.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserProfileName :: Lens.Lens' CreateUserProfile Lude.Text
cupUserProfileName = Lens.lens (userProfileName :: CreateUserProfile -> Lude.Text) (\s a -> s {userProfileName = a} :: CreateUserProfile)
{-# DEPRECATED cupUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | A specifier for the type of value specified in SingleSignOnUserValue. Currently, the only supported value is "UserName". If the Domain's AuthMode is SSO, this field is required. If the Domain's AuthMode is not SSO, this field cannot be specified.
--
-- /Note:/ Consider using 'singleSignOnUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSingleSignOnUserIdentifier :: Lens.Lens' CreateUserProfile (Lude.Maybe Lude.Text)
cupSingleSignOnUserIdentifier = Lens.lens (singleSignOnUserIdentifier :: CreateUserProfile -> Lude.Maybe Lude.Text) (\s a -> s {singleSignOnUserIdentifier = a} :: CreateUserProfile)
{-# DEPRECATED cupSingleSignOnUserIdentifier "Use generic-lens or generic-optics with 'singleSignOnUserIdentifier' instead." #-}

-- | The ID of the associated Domain.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDomainId :: Lens.Lens' CreateUserProfile Lude.Text
cupDomainId = Lens.lens (domainId :: CreateUserProfile -> Lude.Text) (\s a -> s {domainId = a} :: CreateUserProfile)
{-# DEPRECATED cupDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupTags :: Lens.Lens' CreateUserProfile (Lude.Maybe [Tag])
cupTags = Lens.lens (tags :: CreateUserProfile -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateUserProfile)
{-# DEPRECATED cupTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateUserProfile where
  type Rs CreateUserProfile = CreateUserProfileResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Lude.<$> (x Lude..?> "UserProfileArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserProfile where
  toJSON CreateUserProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserSettings" Lude..=) Lude.<$> userSettings,
            ("SingleSignOnUserValue" Lude..=) Lude.<$> singleSignOnUserValue,
            Lude.Just ("UserProfileName" Lude..= userProfileName),
            ("SingleSignOnUserIdentifier" Lude..=)
              Lude.<$> singleSignOnUserIdentifier,
            Lude.Just ("DomainId" Lude..= domainId),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The user profile Amazon Resource Name (ARN).
    userProfileARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'userProfileARN' - The user profile Amazon Resource Name (ARN).
-- * 'responseStatus' - The response status code.
mkCreateUserProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserProfileResponse
mkCreateUserProfileResponse pResponseStatus_ =
  CreateUserProfileResponse'
    { userProfileARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user profile Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'userProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsUserProfileARN :: Lens.Lens' CreateUserProfileResponse (Lude.Maybe Lude.Text)
cuprsUserProfileARN = Lens.lens (userProfileARN :: CreateUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {userProfileARN = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsUserProfileARN "Use generic-lens or generic-optics with 'userProfileARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsResponseStatus :: Lens.Lens' CreateUserProfileResponse Lude.Int
cuprsResponseStatus = Lens.lens (responseStatus :: CreateUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserProfileResponse)
{-# DEPRECATED cuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
