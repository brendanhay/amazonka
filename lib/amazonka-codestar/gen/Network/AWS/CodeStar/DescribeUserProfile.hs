{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeStar.DescribeUserProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user in AWS CodeStar and the user attributes across all projects.
module Network.AWS.CodeStar.DescribeUserProfile
  ( -- * Creating a request
    DescribeUserProfile (..),
    mkDescribeUserProfile,

    -- ** Request lenses
    dupUserARN,

    -- * Destructuring the response
    DescribeUserProfileResponse (..),
    mkDescribeUserProfileResponse,

    -- ** Response lenses
    duprsLastModifiedTimestamp,
    duprsSshPublicKey,
    duprsUserARN,
    duprsEmailAddress,
    duprsDisplayName,
    duprsCreatedTimestamp,
    duprsResponseStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeUserProfile' smart constructor.
newtype DescribeUserProfile = DescribeUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user.
    userARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserProfile' with the minimum fields required to make a request.
--
-- * 'userARN' - The Amazon Resource Name (ARN) of the user.
mkDescribeUserProfile ::
  -- | 'userARN'
  Lude.Text ->
  DescribeUserProfile
mkDescribeUserProfile pUserARN_ =
  DescribeUserProfile' {userARN = pUserARN_}

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserARN :: Lens.Lens' DescribeUserProfile Lude.Text
dupUserARN = Lens.lens (userARN :: DescribeUserProfile -> Lude.Text) (\s a -> s {userARN = a} :: DescribeUserProfile)
{-# DEPRECATED dupUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.AWSRequest DescribeUserProfile where
  type Rs DescribeUserProfile = DescribeUserProfileResponse
  request = Req.postJSON codeStarService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Lude.<$> (x Lude..:> "lastModifiedTimestamp")
            Lude.<*> (x Lude..?> "sshPublicKey")
            Lude.<*> (x Lude..:> "userArn")
            Lude.<*> (x Lude..?> "emailAddress")
            Lude.<*> (x Lude..?> "displayName")
            Lude.<*> (x Lude..:> "createdTimestamp")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeUserProfile where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeStar_20170419.DescribeUserProfile" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("userArn" Lude..= userARN)])

instance Lude.ToPath DescribeUserProfile where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeUserProfile where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The date and time when the user profile was last modified, in timestamp format.
    lastModifiedTimestamp :: Lude.Timestamp,
    -- | The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
    sshPublicKey :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the user.
    userARN :: Lude.Text,
    -- | The email address for the user. Optional.
    emailAddress :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
    displayName :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The date and time when the user profile was created in AWS CodeStar, in timestamp format.
    createdTimestamp :: Lude.Timestamp,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeUserProfileResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedTimestamp' - The date and time when the user profile was last modified, in timestamp format.
-- * 'sshPublicKey' - The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user.
-- * 'emailAddress' - The email address for the user. Optional.
-- * 'displayName' - The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
-- * 'createdTimestamp' - The date and time when the user profile was created in AWS CodeStar, in timestamp format.
-- * 'responseStatus' - The response status code.
mkDescribeUserProfileResponse ::
  -- | 'lastModifiedTimestamp'
  Lude.Timestamp ->
  -- | 'userARN'
  Lude.Text ->
  -- | 'createdTimestamp'
  Lude.Timestamp ->
  -- | 'responseStatus'
  Lude.Int ->
  DescribeUserProfileResponse
mkDescribeUserProfileResponse
  pLastModifiedTimestamp_
  pUserARN_
  pCreatedTimestamp_
  pResponseStatus_ =
    DescribeUserProfileResponse'
      { lastModifiedTimestamp =
          pLastModifiedTimestamp_,
        sshPublicKey = Lude.Nothing,
        userARN = pUserARN_,
        emailAddress = Lude.Nothing,
        displayName = Lude.Nothing,
        createdTimestamp = pCreatedTimestamp_,
        responseStatus = pResponseStatus_
      }

-- | The date and time when the user profile was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsLastModifiedTimestamp :: Lens.Lens' DescribeUserProfileResponse Lude.Timestamp
duprsLastModifiedTimestamp = Lens.lens (lastModifiedTimestamp :: DescribeUserProfileResponse -> Lude.Timestamp) (\s a -> s {lastModifiedTimestamp = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsSshPublicKey :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe Lude.Text)
duprsSshPublicKey = Lens.lens (sshPublicKey :: DescribeUserProfileResponse -> Lude.Maybe Lude.Text) (\s a -> s {sshPublicKey = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsUserARN :: Lens.Lens' DescribeUserProfileResponse Lude.Text
duprsUserARN = Lens.lens (userARN :: DescribeUserProfileResponse -> Lude.Text) (\s a -> s {userARN = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

-- | The email address for the user. Optional.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsEmailAddress :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
duprsEmailAddress = Lens.lens (emailAddress :: DescribeUserProfileResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {emailAddress = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsDisplayName :: Lens.Lens' DescribeUserProfileResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
duprsDisplayName = Lens.lens (displayName :: DescribeUserProfileResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {displayName = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The date and time when the user profile was created in AWS CodeStar, in timestamp format.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsCreatedTimestamp :: Lens.Lens' DescribeUserProfileResponse Lude.Timestamp
duprsCreatedTimestamp = Lens.lens (createdTimestamp :: DescribeUserProfileResponse -> Lude.Timestamp) (\s a -> s {createdTimestamp = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprsResponseStatus :: Lens.Lens' DescribeUserProfileResponse Lude.Int
duprsResponseStatus = Lens.lens (responseStatus :: DescribeUserProfileResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeUserProfileResponse)
{-# DEPRECATED duprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
