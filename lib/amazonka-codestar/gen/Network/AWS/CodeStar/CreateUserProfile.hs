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
    cupUserArn,
    cupDisplayName,
    cupEmailAddress,
    cupSshPublicKey,

    -- * Destructuring the response
    CreateUserProfileResponse (..),
    mkCreateUserProfileResponse,

    -- ** Response lenses
    cuprrsUserArn,
    cuprrsCreatedTimestamp,
    cuprrsDisplayName,
    cuprrsEmailAddress,
    cuprrsLastModifiedTimestamp,
    cuprrsSshPublicKey,
    cuprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Types.UserArn,
    -- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
    displayName :: Types.UserProfileDisplayName,
    -- | The email address that will be displayed as part of the user's profile in AWS CodeStar.
    emailAddress :: Types.Email,
    -- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
    sshPublicKey :: Core.Maybe Types.SshPublicKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserProfile' value with any optional fields omitted.
mkCreateUserProfile ::
  -- | 'userArn'
  Types.UserArn ->
  -- | 'displayName'
  Types.UserProfileDisplayName ->
  -- | 'emailAddress'
  Types.Email ->
  CreateUserProfile
mkCreateUserProfile userArn displayName emailAddress =
  CreateUserProfile'
    { userArn,
      displayName,
      emailAddress,
      sshPublicKey = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserArn :: Lens.Lens' CreateUserProfile Types.UserArn
cupUserArn = Lens.field @"userArn"
{-# DEPRECATED cupUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDisplayName :: Lens.Lens' CreateUserProfile Types.UserProfileDisplayName
cupDisplayName = Lens.field @"displayName"
{-# DEPRECATED cupDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The email address that will be displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailAddress :: Lens.Lens' CreateUserProfile Types.Email
cupEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED cupEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSshPublicKey :: Lens.Lens' CreateUserProfile (Core.Maybe Types.SshPublicKey)
cupSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED cupSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

instance Core.FromJSON CreateUserProfile where
  toJSON CreateUserProfile {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("userArn" Core..= userArn),
            Core.Just ("displayName" Core..= displayName),
            Core.Just ("emailAddress" Core..= emailAddress),
            ("sshPublicKey" Core..=) Core.<$> sshPublicKey
          ]
      )

instance Core.AWSRequest CreateUserProfile where
  type Rs CreateUserProfile = CreateUserProfileResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeStar_20170419.CreateUserProfile")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserProfileResponse'
            Core.<$> (x Core..: "userArn")
            Core.<*> (x Core..:? "createdTimestamp")
            Core.<*> (x Core..:? "displayName")
            Core.<*> (x Core..:? "emailAddress")
            Core.<*> (x Core..:? "lastModifiedTimestamp")
            Core.<*> (x Core..:? "sshPublicKey")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { -- | The Amazon Resource Name (ARN) of the user in IAM.
    userArn :: Types.UserArn,
    -- | The date the user profile was created, in timestamp format.
    createdTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The name that is displayed as the friendly name for the user in AWS CodeStar.
    displayName :: Core.Maybe Types.DisplayName,
    -- | The email address that is displayed as part of the user's profile in AWS CodeStar.
    emailAddress :: Core.Maybe Types.Email,
    -- | The date the user profile was last modified, in timestamp format.
    lastModifiedTimestamp :: Core.Maybe Core.NominalDiffTime,
    -- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
    sshPublicKey :: Core.Maybe Types.SshPublicKey,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateUserProfileResponse' value with any optional fields omitted.
mkCreateUserProfileResponse ::
  -- | 'userArn'
  Types.UserArn ->
  -- | 'responseStatus'
  Core.Int ->
  CreateUserProfileResponse
mkCreateUserProfileResponse userArn responseStatus =
  CreateUserProfileResponse'
    { userArn,
      createdTimestamp = Core.Nothing,
      displayName = Core.Nothing,
      emailAddress = Core.Nothing,
      lastModifiedTimestamp = Core.Nothing,
      sshPublicKey = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsUserArn :: Lens.Lens' CreateUserProfileResponse Types.UserArn
cuprrsUserArn = Lens.field @"userArn"
{-# DEPRECATED cuprrsUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

-- | The date the user profile was created, in timestamp format.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsCreatedTimestamp :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.NominalDiffTime)
cuprrsCreatedTimestamp = Lens.field @"createdTimestamp"
{-# DEPRECATED cuprrsCreatedTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead." #-}

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsDisplayName :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Types.DisplayName)
cuprrsDisplayName = Lens.field @"displayName"
{-# DEPRECATED cuprrsDisplayName "Use generic-lens or generic-optics with 'displayName' instead." #-}

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsEmailAddress :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Types.Email)
cuprrsEmailAddress = Lens.field @"emailAddress"
{-# DEPRECATED cuprrsEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | The date the user profile was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsLastModifiedTimestamp :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Core.NominalDiffTime)
cuprrsLastModifiedTimestamp = Lens.field @"lastModifiedTimestamp"
{-# DEPRECATED cuprrsLastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead." #-}

-- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsSshPublicKey :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Types.SshPublicKey)
cuprrsSshPublicKey = Lens.field @"sshPublicKey"
{-# DEPRECATED cuprrsSshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsResponseStatus :: Lens.Lens' CreateUserProfileResponse Core.Int
cuprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cuprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
