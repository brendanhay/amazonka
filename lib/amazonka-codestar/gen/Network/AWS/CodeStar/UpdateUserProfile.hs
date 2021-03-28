{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateUserProfile (..)
    , mkUpdateUserProfile
    -- ** Request lenses
    , uupUserArn
    , uupDisplayName
    , uupEmailAddress
    , uupSshPublicKey

    -- * Destructuring the response
    , UpdateUserProfileResponse (..)
    , mkUpdateUserProfileResponse
    -- ** Response lenses
    , uuprrsUserArn
    , uuprrsCreatedTimestamp
    , uuprrsDisplayName
    , uuprrsEmailAddress
    , uuprrsLastModifiedTimestamp
    , uuprrsSshPublicKey
    , uuprrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { userArn :: Types.UserArn
    -- ^ The name that will be displayed as the friendly name for the user in AWS CodeStar.
  , displayName :: Core.Maybe Types.UserProfileDisplayName
    -- ^ The name that is displayed as the friendly name for the user in AWS CodeStar.
  , emailAddress :: Core.Maybe Types.Email
    -- ^ The email address that is displayed as part of the user's profile in AWS CodeStar.
  , sshPublicKey :: Core.Maybe Types.SshPublicKey
    -- ^ The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserProfile' value with any optional fields omitted.
mkUpdateUserProfile
    :: Types.UserArn -- ^ 'userArn'
    -> UpdateUserProfile
mkUpdateUserProfile userArn
  = UpdateUserProfile'{userArn, displayName = Core.Nothing,
                       emailAddress = Core.Nothing, sshPublicKey = Core.Nothing}

-- | The name that will be displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserArn :: Lens.Lens' UpdateUserProfile Types.UserArn
uupUserArn = Lens.field @"userArn"
{-# INLINEABLE uupUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupDisplayName :: Lens.Lens' UpdateUserProfile (Core.Maybe Types.UserProfileDisplayName)
uupDisplayName = Lens.field @"displayName"
{-# INLINEABLE uupDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupEmailAddress :: Lens.Lens' UpdateUserProfile (Core.Maybe Types.Email)
uupEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE uupEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The SSH public key associated with the user in AWS CodeStar. If a project owner allows the user remote access to project resources, this public key will be used along with the user's private key for SSH access.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSshPublicKey :: Lens.Lens' UpdateUserProfile (Core.Maybe Types.SshPublicKey)
uupSshPublicKey = Lens.field @"sshPublicKey"
{-# INLINEABLE uupSshPublicKey #-}
{-# DEPRECATED sshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead"  #-}

instance Core.ToQuery UpdateUserProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUserProfile where
        toHeaders UpdateUserProfile{..}
          = Core.pure ("X-Amz-Target", "CodeStar_20170419.UpdateUserProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUserProfile where
        toJSON UpdateUserProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("userArn" Core..= userArn),
                  ("displayName" Core..=) Core.<$> displayName,
                  ("emailAddress" Core..=) Core.<$> emailAddress,
                  ("sshPublicKey" Core..=) Core.<$> sshPublicKey])

instance Core.AWSRequest UpdateUserProfile where
        type Rs UpdateUserProfile = UpdateUserProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateUserProfileResponse' Core.<$>
                   (x Core..: "userArn") Core.<*> x Core..:? "createdTimestamp"
                     Core.<*> x Core..:? "displayName"
                     Core.<*> x Core..:? "emailAddress"
                     Core.<*> x Core..:? "lastModifiedTimestamp"
                     Core.<*> x Core..:? "sshPublicKey"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user in IAM.
  , createdTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the user profile was created, in timestamp format.
  , displayName :: Core.Maybe Types.UserProfileDisplayName
    -- ^ The name that is displayed as the friendly name for the user in AWS CodeStar.
  , emailAddress :: Core.Maybe Types.Email
    -- ^ The email address that is displayed as part of the user's profile in AWS CodeStar.
  , lastModifiedTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the user profile was last modified, in timestamp format.
  , sshPublicKey :: Core.Maybe Types.SshPublicKey
    -- ^ The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdateUserProfileResponse' value with any optional fields omitted.
mkUpdateUserProfileResponse
    :: Types.UserArn -- ^ 'userArn'
    -> Core.Int -- ^ 'responseStatus'
    -> UpdateUserProfileResponse
mkUpdateUserProfileResponse userArn responseStatus
  = UpdateUserProfileResponse'{userArn,
                               createdTimestamp = Core.Nothing, displayName = Core.Nothing,
                               emailAddress = Core.Nothing, lastModifiedTimestamp = Core.Nothing,
                               sshPublicKey = Core.Nothing, responseStatus}

-- | The Amazon Resource Name (ARN) of the user in IAM.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsUserArn :: Lens.Lens' UpdateUserProfileResponse Types.UserArn
uuprrsUserArn = Lens.field @"userArn"
{-# INLINEABLE uuprrsUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The date the user profile was created, in timestamp format.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsCreatedTimestamp :: Lens.Lens' UpdateUserProfileResponse (Core.Maybe Core.NominalDiffTime)
uuprrsCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE uuprrsCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The name that is displayed as the friendly name for the user in AWS CodeStar.
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsDisplayName :: Lens.Lens' UpdateUserProfileResponse (Core.Maybe Types.UserProfileDisplayName)
uuprrsDisplayName = Lens.field @"displayName"
{-# INLINEABLE uuprrsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The email address that is displayed as part of the user's profile in AWS CodeStar.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsEmailAddress :: Lens.Lens' UpdateUserProfileResponse (Core.Maybe Types.Email)
uuprrsEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE uuprrsEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The date the user profile was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsLastModifiedTimestamp :: Lens.Lens' UpdateUserProfileResponse (Core.Maybe Core.NominalDiffTime)
uuprrsLastModifiedTimestamp = Lens.field @"lastModifiedTimestamp"
{-# INLINEABLE uuprrsLastModifiedTimestamp #-}
{-# DEPRECATED lastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead"  #-}

-- | The SSH public key associated with the user in AWS CodeStar. This is the public portion of the public/private keypair the user can use to access project resources if a project owner allows the user remote access to those resources.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsSshPublicKey :: Lens.Lens' UpdateUserProfileResponse (Core.Maybe Types.SshPublicKey)
uuprrsSshPublicKey = Lens.field @"sshPublicKey"
{-# INLINEABLE uuprrsSshPublicKey #-}
{-# DEPRECATED sshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsResponseStatus :: Lens.Lens' UpdateUserProfileResponse Core.Int
uuprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uuprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
