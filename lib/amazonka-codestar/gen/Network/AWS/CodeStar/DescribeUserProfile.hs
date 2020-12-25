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
    dupUserArn,

    -- * Destructuring the response
    DescribeUserProfileResponse (..),
    mkDescribeUserProfileResponse,

    -- ** Response lenses
    duprrsUserArn,
    duprrsCreatedTimestamp,
    duprrsLastModifiedTimestamp,
    duprrsDisplayName,
    duprrsEmailAddress,
    duprrsSshPublicKey,
    duprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUserProfile' smart constructor.
newtype DescribeUserProfile = DescribeUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Types.UserArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserProfile' value with any optional fields omitted.
mkDescribeUserProfile ::
  -- | 'userArn'
  Types.UserArn ->
  DescribeUserProfile
mkDescribeUserProfile userArn = DescribeUserProfile' {userArn}

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserArn :: Lens.Lens' DescribeUserProfile Types.UserArn
dupUserArn = Lens.field @"userArn"
{-# DEPRECATED dupUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

instance Core.FromJSON DescribeUserProfile where
  toJSON DescribeUserProfile {..} =
    Core.object
      (Core.catMaybes [Core.Just ("userArn" Core..= userArn)])

instance Core.AWSRequest DescribeUserProfile where
  type Rs DescribeUserProfile = DescribeUserProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.DescribeUserProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Core.<$> (x Core..: "userArn")
            Core.<*> (x Core..: "createdTimestamp")
            Core.<*> (x Core..: "lastModifiedTimestamp")
            Core.<*> (x Core..:? "displayName")
            Core.<*> (x Core..:? "emailAddress")
            Core.<*> (x Core..:? "sshPublicKey")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Types.UserArn,
    -- | The date and time when the user profile was created in AWS CodeStar, in timestamp format.
    createdTimestamp :: Core.NominalDiffTime,
    -- | The date and time when the user profile was last modified, in timestamp format.
    lastModifiedTimestamp :: Core.NominalDiffTime,
    -- | The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
    displayName :: Core.Maybe Types.UserProfileDisplayName,
    -- | The email address for the user. Optional.
    emailAddress :: Core.Maybe Types.Email,
    -- | The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
    sshPublicKey :: Core.Maybe Types.SshPublicKey,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeUserProfileResponse' value with any optional fields omitted.
mkDescribeUserProfileResponse ::
  -- | 'userArn'
  Types.UserArn ->
  -- | 'createdTimestamp'
  Core.NominalDiffTime ->
  -- | 'lastModifiedTimestamp'
  Core.NominalDiffTime ->
  -- | 'responseStatus'
  Core.Int ->
  DescribeUserProfileResponse
mkDescribeUserProfileResponse
  userArn
  createdTimestamp
  lastModifiedTimestamp
  responseStatus =
    DescribeUserProfileResponse'
      { userArn,
        createdTimestamp,
        lastModifiedTimestamp,
        displayName = Core.Nothing,
        emailAddress = Core.Nothing,
        sshPublicKey = Core.Nothing,
        responseStatus
      }

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserArn :: Lens.Lens' DescribeUserProfileResponse Types.UserArn
duprrsUserArn = Lens.field @"userArn"
{-# DEPRECATED duprrsUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The date and time when the user profile was created in AWS CodeStar, in timestamp format.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsCreatedTimestamp :: Lens.Lens' DescribeUserProfileResponse Core.NominalDiffTime
duprrsCreatedTimestamp = Lens.field @"createdTimestamp"
{-# DEPRECATED duprrsCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The date and time when the user profile was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsLastModifiedTimestamp :: Lens.Lens' DescribeUserProfileResponse Core.NominalDiffTime
duprrsLastModifiedTimestamp = Lens.field @"lastModifiedTimestamp"
{-# DEPRECATED duprrsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsDisplayName :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.UserProfileDisplayName)
duprrsDisplayName = Lens.field @"displayName"
{-# DEPRECATED duprrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The email address for the user. Optional.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsEmailAddress :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.Email)
duprrsEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED duprrsEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsSshPublicKey :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.SshPublicKey)
duprrsSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED duprrsSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsResponseStatus :: Lens.Lens' DescribeUserProfileResponse Core.Int
duprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED duprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
