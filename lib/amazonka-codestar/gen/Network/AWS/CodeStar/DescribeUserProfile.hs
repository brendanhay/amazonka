{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeUserProfile (..)
    , mkDescribeUserProfile
    -- ** Request lenses
    , dupUserArn

    -- * Destructuring the response
    , DescribeUserProfileResponse (..)
    , mkDescribeUserProfileResponse
    -- ** Response lenses
    , duprrsUserArn
    , duprrsCreatedTimestamp
    , duprrsLastModifiedTimestamp
    , duprrsDisplayName
    , duprrsEmailAddress
    , duprrsSshPublicKey
    , duprrsResponseStatus
    ) where

import qualified Network.AWS.CodeStar.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeUserProfile' smart constructor.
newtype DescribeUserProfile = DescribeUserProfile'
  { userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserProfile' value with any optional fields omitted.
mkDescribeUserProfile
    :: Types.UserArn -- ^ 'userArn'
    -> DescribeUserProfile
mkDescribeUserProfile userArn = DescribeUserProfile'{userArn}

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserArn :: Lens.Lens' DescribeUserProfile Types.UserArn
dupUserArn = Lens.field @"userArn"
{-# INLINEABLE dupUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

instance Core.ToQuery DescribeUserProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeUserProfile where
        toHeaders DescribeUserProfile{..}
          = Core.pure
              ("X-Amz-Target", "CodeStar_20170419.DescribeUserProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeUserProfile where
        toJSON DescribeUserProfile{..}
          = Core.object
              (Core.catMaybes [Core.Just ("userArn" Core..= userArn)])

instance Core.AWSRequest DescribeUserProfile where
        type Rs DescribeUserProfile = DescribeUserProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeUserProfileResponse' Core.<$>
                   (x Core..: "userArn") Core.<*> x Core..: "createdTimestamp"
                     Core.<*> x Core..: "lastModifiedTimestamp"
                     Core.<*> x Core..:? "displayName"
                     Core.<*> x Core..:? "emailAddress"
                     Core.<*> x Core..:? "sshPublicKey"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { userArn :: Types.UserArn
    -- ^ The Amazon Resource Name (ARN) of the user.
  , createdTimestamp :: Core.NominalDiffTime
    -- ^ The date and time when the user profile was created in AWS CodeStar, in timestamp format.
  , lastModifiedTimestamp :: Core.NominalDiffTime
    -- ^ The date and time when the user profile was last modified, in timestamp format.
  , displayName :: Core.Maybe Types.UserProfileDisplayName
    -- ^ The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
  , emailAddress :: Core.Maybe Types.Email
    -- ^ The email address for the user. Optional.
  , sshPublicKey :: Core.Maybe Types.SshPublicKey
    -- ^ The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUserProfileResponse' value with any optional fields omitted.
mkDescribeUserProfileResponse
    :: Types.UserArn -- ^ 'userArn'
    -> Core.NominalDiffTime -- ^ 'createdTimestamp'
    -> Core.NominalDiffTime -- ^ 'lastModifiedTimestamp'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeUserProfileResponse
mkDescribeUserProfileResponse userArn createdTimestamp
  lastModifiedTimestamp responseStatus
  = DescribeUserProfileResponse'{userArn, createdTimestamp,
                                 lastModifiedTimestamp, displayName = Core.Nothing,
                                 emailAddress = Core.Nothing, sshPublicKey = Core.Nothing,
                                 responseStatus}

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserArn :: Lens.Lens' DescribeUserProfileResponse Types.UserArn
duprrsUserArn = Lens.field @"userArn"
{-# INLINEABLE duprrsUserArn #-}
{-# DEPRECATED userArn "Use generic-lens or generic-optics with 'userArn' instead"  #-}

-- | The date and time when the user profile was created in AWS CodeStar, in timestamp format.
--
-- /Note:/ Consider using 'createdTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsCreatedTimestamp :: Lens.Lens' DescribeUserProfileResponse Core.NominalDiffTime
duprrsCreatedTimestamp = Lens.field @"createdTimestamp"
{-# INLINEABLE duprrsCreatedTimestamp #-}
{-# DEPRECATED createdTimestamp "Use generic-lens or generic-optics with 'createdTimestamp' instead"  #-}

-- | The date and time when the user profile was last modified, in timestamp format.
--
-- /Note:/ Consider using 'lastModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsLastModifiedTimestamp :: Lens.Lens' DescribeUserProfileResponse Core.NominalDiffTime
duprrsLastModifiedTimestamp = Lens.field @"lastModifiedTimestamp"
{-# INLINEABLE duprrsLastModifiedTimestamp #-}
{-# DEPRECATED lastModifiedTimestamp "Use generic-lens or generic-optics with 'lastModifiedTimestamp' instead"  #-}

-- | The display name shown for the user in AWS CodeStar projects. For example, this could be set to both first and last name ("Mary Major") or a single name ("Mary"). The display name is also used to generate the initial icon associated with the user in AWS CodeStar projects. If spaces are included in the display name, the first character that appears after the space will be used as the second character in the user initial icon. The initial icon displays a maximum of two characters, so a display name with more than one space (for example "Mary Jane Major") would generate an initial icon using the first character and the first character after the space ("MJ", not "MM").
--
-- /Note:/ Consider using 'displayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsDisplayName :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.UserProfileDisplayName)
duprrsDisplayName = Lens.field @"displayName"
{-# INLINEABLE duprrsDisplayName #-}
{-# DEPRECATED displayName "Use generic-lens or generic-optics with 'displayName' instead"  #-}

-- | The email address for the user. Optional.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsEmailAddress :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.Email)
duprrsEmailAddress = Lens.field @"emailAddress"
{-# INLINEABLE duprrsEmailAddress #-}
{-# DEPRECATED emailAddress "Use generic-lens or generic-optics with 'emailAddress' instead"  #-}

-- | The SSH public key associated with the user. This SSH public key is associated with the user profile, and can be used in conjunction with the associated private key for access to project resources, such as Amazon EC2 instances, if a project owner grants remote access to those resources.
--
-- /Note:/ Consider using 'sshPublicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsSshPublicKey :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.SshPublicKey)
duprrsSshPublicKey = Lens.field @"sshPublicKey"
{-# INLINEABLE duprrsSshPublicKey #-}
{-# DEPRECATED sshPublicKey "Use generic-lens or generic-optics with 'sshPublicKey' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsResponseStatus :: Lens.Lens' DescribeUserProfileResponse Core.Int
duprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE duprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
