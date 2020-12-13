{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user profile. For more information, see @CreateUserProfile@ .
module Network.AWS.SageMaker.DescribeUserProfile
  ( -- * Creating a request
    DescribeUserProfile (..),
    mkDescribeUserProfile,

    -- ** Request lenses
    dupUserProfileName,
    dupDomainId,

    -- * Destructuring the response
    DescribeUserProfileResponse (..),
    mkDescribeUserProfileResponse,

    -- ** Response lenses
    duprsCreationTime,
    duprsUserSettings,
    duprsStatus,
    duprsFailureReason,
    duprsSingleSignOnUserValue,
    duprsUserProfileName,
    duprsLastModifiedTime,
    duprsHomeEfsFileSystemUid,
    duprsUserProfileARN,
    duprsSingleSignOnUserIdentifier,
    duprsDomainId,
    duprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { -- | The user profile name.
    userProfileName :: Lude.Text,
    -- | The domain ID.
    domainId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserProfile' with the minimum fields required to make a request.
--
-- * 'userProfileName' - The user profile name.
-- * 'domainId' - The domain ID.
mkDescribeUserProfile ::
  -- | 'userProfileName'
  Lude.Text ->
  -- | 'domainId'
  Lude.Text ->
  DescribeUserProfile
mkDescribeUserProfile pUserProfileName_ pDomainId_ =
  DescribeUserProfile'
    { userProfileName = pUserProfileName_,
      domainId = pDomainId_
    }

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserProfileName :: Lens.Lens' DescribeUserProfile Lude.Text
dupUserProfileName = Lens.lens (userProfileName :: DescribeUserProfile -> Lude.Text) (\s a -> s {userProfileName = a} :: DescribeUserProfile)
{-# DEPRECATED dupUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupDomainId :: Lens.Lens' DescribeUserProfile Lude.Text
dupDomainId = Lens.lens (domainId :: DescribeUserProfile -> Lude.Text) (\s a -> s {domainId = a} :: DescribeUserProfile)
{-# DEPRECATED dupDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.AWSRequest DescribeUserProfile where
  type Rs DescribeUserProfile = DescribeUserProfileResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Lude.<$> (x Lude..?> "CreationTime")
            Lude.<*> (x Lude..?> "UserSettings")
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "FailureReason")
            Lude.<*> (x Lude..?> "SingleSignOnUserValue")
            Lude.<*> (x Lude..?> "UserProfileName")
            Lude.<*> (x Lude..?> "LastModifiedTime")
            Lude.<*> (x Lude..?> "HomeEfsFileSystemUid")
            Lude.<*> (x Lude..?> "UserProfileArn")
            Lude.<*> (x Lude..?> "SingleSignOnUserIdentifier")
            Lude.<*> (x Lude..?> "DomainId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("UserProfileName" Lude..= userProfileName),
            Lude.Just ("DomainId" Lude..= domainId)
          ]
      )

instance Lude.ToPath DescribeUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The creation time.
    creationTime :: Lude.Maybe Lude.Timestamp,
    -- | A collection of settings.
    userSettings :: Lude.Maybe UserSettings,
    -- | The status.
    status :: Lude.Maybe UserProfileStatus,
    -- | The failure reason.
    failureReason :: Lude.Maybe Lude.Text,
    -- | The SSO user value.
    singleSignOnUserValue :: Lude.Maybe Lude.Text,
    -- | The user profile name.
    userProfileName :: Lude.Maybe Lude.Text,
    -- | The last modified time.
    lastModifiedTime :: Lude.Maybe Lude.Timestamp,
    -- | The ID of the user's profile in the Amazon Elastic File System (EFS) volume.
    homeEfsFileSystemUid :: Lude.Maybe Lude.Text,
    -- | The user profile Amazon Resource Name (ARN).
    userProfileARN :: Lude.Maybe Lude.Text,
    -- | The SSO user identifier.
    singleSignOnUserIdentifier :: Lude.Maybe Lude.Text,
    -- | The ID of the domain that contains the profile.
    domainId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The creation time.
-- * 'userSettings' - A collection of settings.
-- * 'status' - The status.
-- * 'failureReason' - The failure reason.
-- * 'singleSignOnUserValue' - The SSO user value.
-- * 'userProfileName' - The user profile name.
-- * 'lastModifiedTime' - The last modified time.
-- * 'homeEfsFileSystemUid' - The ID of the user's profile in the Amazon Elastic File System (EFS) volume.
-- * 'userProfileARN' - The user profile Amazon Resource Name (ARN).
-- * 'singleSignOnUserIdentifier' - The SSO user identifier.
-- * 'domainId' - The ID of the domain that contains the profile.
-- * 'responseStatus' - The response status code.
mkDescribeUserProfileResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserProfileResponse
mkDescribeUserProfileResponse pResponseStatus_ =
  DescribeUserProfileResponse'
    { creationTime = Lude.Nothing,
      userSettings = Lude.Nothing,
      status = Lude.Nothing,
      failureReason = Lude.Nothing,
      singleSignOnUserValue = Lude.Nothing,
      userProfileName = Lude.Nothing,
      lastModifiedTime = Lude.Nothing,
      homeEfsFileSystemUid = Lude.Nothing,
      userProfileARN = Lude.Nothing,
      singleSignOnUserIdentifier = Lude.Nothing,
      domainId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsCreationTime :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Timestamp)
duprsCreationTime = Lens.lens (creationTime :: DescribeUserProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A collection of settings.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsUserSettings :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe UserSettings)
duprsUserSettings = Lens.lens (userSettings :: DescribeUserProfileResponse -> Lude.Maybe UserSettings) (\s a -> s {userSettings = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsUserSettings "Use generic-lens or generic-optics with 'userSettings' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsStatus :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe UserProfileStatus)
duprsStatus = Lens.lens (status :: DescribeUserProfileResponse -> Lude.Maybe UserProfileStatus) (\s a -> s {status = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsFailureReason :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsFailureReason = Lens.lens (failureReason :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {failureReason = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The SSO user value.
--
-- /Note:/ Consider using 'singleSignOnUserValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsSingleSignOnUserValue :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsSingleSignOnUserValue = Lens.lens (singleSignOnUserValue :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {singleSignOnUserValue = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsSingleSignOnUserValue "Use generic-lens or generic-optics with 'singleSignOnUserValue' instead." #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsUserProfileName :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsUserProfileName = Lens.lens (userProfileName :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {userProfileName = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsLastModifiedTime :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Timestamp)
duprsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeUserProfileResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}

-- | The ID of the user's profile in the Amazon Elastic File System (EFS) volume.
--
-- /Note:/ Consider using 'homeEfsFileSystemUid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsHomeEfsFileSystemUid :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsHomeEfsFileSystemUid = Lens.lens (homeEfsFileSystemUid :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {homeEfsFileSystemUid = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsHomeEfsFileSystemUid "Use generic-lens or generic-optics with 'homeEfsFileSystemUid' instead." #-}

-- | The user profile Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'userProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsUserProfileARN :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsUserProfileARN = Lens.lens (userProfileARN :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {userProfileARN = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsUserProfileARN "Use generic-lens or generic-optics with 'userProfileARN' instead." #-}

-- | The SSO user identifier.
--
-- /Note:/ Consider using 'singleSignOnUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsSingleSignOnUserIdentifier :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsSingleSignOnUserIdentifier = Lens.lens (singleSignOnUserIdentifier :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {singleSignOnUserIdentifier = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsSingleSignOnUserIdentifier "Use generic-lens or generic-optics with 'singleSignOnUserIdentifier' instead." #-}

-- | The ID of the domain that contains the profile.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsDomainId :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsDomainId = Lens.lens (domainId :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainId = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsResponseStatus :: Lens.Lens' DescribeUserProfileResponse Lude.Int
duprsResponseStatus = Lens.lens (responseStatus :: DescribeUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
