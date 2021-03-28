{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeUserProfile (..)
    , mkDescribeUserProfile
    -- ** Request lenses
    , dupDomainId
    , dupUserProfileName

    -- * Destructuring the response
    , DescribeUserProfileResponse (..)
    , mkDescribeUserProfileResponse
    -- ** Response lenses
    , duprrsCreationTime
    , duprrsDomainId
    , duprrsFailureReason
    , duprrsHomeEfsFileSystemUid
    , duprrsLastModifiedTime
    , duprrsSingleSignOnUserIdentifier
    , duprrsSingleSignOnUserValue
    , duprrsStatus
    , duprrsUserProfileArn
    , duprrsUserProfileName
    , duprrsUserSettings
    , duprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  , userProfileName :: Types.UserProfileName
    -- ^ The user profile name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeUserProfile' value with any optional fields omitted.
mkDescribeUserProfile
    :: Types.DomainId -- ^ 'domainId'
    -> Types.UserProfileName -- ^ 'userProfileName'
    -> DescribeUserProfile
mkDescribeUserProfile domainId userProfileName
  = DescribeUserProfile'{domainId, userProfileName}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupDomainId :: Lens.Lens' DescribeUserProfile Types.DomainId
dupDomainId = Lens.field @"domainId"
{-# INLINEABLE dupDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dupUserProfileName :: Lens.Lens' DescribeUserProfile Types.UserProfileName
dupUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE dupUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

instance Core.ToQuery DescribeUserProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeUserProfile where
        toHeaders DescribeUserProfile{..}
          = Core.pure ("X-Amz-Target", "SageMaker.DescribeUserProfile")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeUserProfile where
        toJSON DescribeUserProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  Core.Just ("UserProfileName" Core..= userProfileName)])

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
                   (x Core..:? "CreationTime") Core.<*> x Core..:? "DomainId" Core.<*>
                     x Core..:? "FailureReason"
                     Core.<*> x Core..:? "HomeEfsFileSystemUid"
                     Core.<*> x Core..:? "LastModifiedTime"
                     Core.<*> x Core..:? "SingleSignOnUserIdentifier"
                     Core.<*> x Core..:? "SingleSignOnUserValue"
                     Core.<*> x Core..:? "Status"
                     Core.<*> x Core..:? "UserProfileArn"
                     Core.<*> x Core..:? "UserProfileName"
                     Core.<*> x Core..:? "UserSettings"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { creationTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation time.
  , domainId :: Core.Maybe Types.DomainId
    -- ^ The ID of the domain that contains the profile.
  , failureReason :: Core.Maybe Types.FailureReason
    -- ^ The failure reason.
  , homeEfsFileSystemUid :: Core.Maybe Types.EfsUid
    -- ^ The ID of the user's profile in the Amazon Elastic File System (EFS) volume.
  , lastModifiedTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The last modified time.
  , singleSignOnUserIdentifier :: Core.Maybe Types.SingleSignOnUserIdentifier
    -- ^ The SSO user identifier.
  , singleSignOnUserValue :: Core.Maybe Types.String256
    -- ^ The SSO user value.
  , status :: Core.Maybe Types.UserProfileStatus
    -- ^ The status.
  , userProfileArn :: Core.Maybe Types.UserProfileArn
    -- ^ The user profile Amazon Resource Name (ARN).
  , userProfileName :: Core.Maybe Types.UserProfileName
    -- ^ The user profile name.
  , userSettings :: Core.Maybe Types.UserSettings
    -- ^ A collection of settings.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeUserProfileResponse' value with any optional fields omitted.
mkDescribeUserProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeUserProfileResponse
mkDescribeUserProfileResponse responseStatus
  = DescribeUserProfileResponse'{creationTime = Core.Nothing,
                                 domainId = Core.Nothing, failureReason = Core.Nothing,
                                 homeEfsFileSystemUid = Core.Nothing,
                                 lastModifiedTime = Core.Nothing,
                                 singleSignOnUserIdentifier = Core.Nothing,
                                 singleSignOnUserValue = Core.Nothing, status = Core.Nothing,
                                 userProfileArn = Core.Nothing, userProfileName = Core.Nothing,
                                 userSettings = Core.Nothing, responseStatus}

-- | The creation time.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsCreationTime :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.NominalDiffTime)
duprrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE duprrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The ID of the domain that contains the profile.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsDomainId :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.DomainId)
duprrsDomainId = Lens.field @"domainId"
{-# INLINEABLE duprrsDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The failure reason.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsFailureReason :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.FailureReason)
duprrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE duprrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The ID of the user's profile in the Amazon Elastic File System (EFS) volume.
--
-- /Note:/ Consider using 'homeEfsFileSystemUid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsHomeEfsFileSystemUid :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.EfsUid)
duprrsHomeEfsFileSystemUid = Lens.field @"homeEfsFileSystemUid"
{-# INLINEABLE duprrsHomeEfsFileSystemUid #-}
{-# DEPRECATED homeEfsFileSystemUid "Use generic-lens or generic-optics with 'homeEfsFileSystemUid' instead"  #-}

-- | The last modified time.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsLastModifiedTime :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.NominalDiffTime)
duprrsLastModifiedTime = Lens.field @"lastModifiedTime"
{-# INLINEABLE duprrsLastModifiedTime #-}
{-# DEPRECATED lastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead"  #-}

-- | The SSO user identifier.
--
-- /Note:/ Consider using 'singleSignOnUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsSingleSignOnUserIdentifier :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.SingleSignOnUserIdentifier)
duprrsSingleSignOnUserIdentifier = Lens.field @"singleSignOnUserIdentifier"
{-# INLINEABLE duprrsSingleSignOnUserIdentifier #-}
{-# DEPRECATED singleSignOnUserIdentifier "Use generic-lens or generic-optics with 'singleSignOnUserIdentifier' instead"  #-}

-- | The SSO user value.
--
-- /Note:/ Consider using 'singleSignOnUserValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsSingleSignOnUserValue :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.String256)
duprrsSingleSignOnUserValue = Lens.field @"singleSignOnUserValue"
{-# INLINEABLE duprrsSingleSignOnUserValue #-}
{-# DEPRECATED singleSignOnUserValue "Use generic-lens or generic-optics with 'singleSignOnUserValue' instead"  #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsStatus :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.UserProfileStatus)
duprrsStatus = Lens.field @"status"
{-# INLINEABLE duprrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The user profile Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'userProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserProfileArn :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.UserProfileArn)
duprrsUserProfileArn = Lens.field @"userProfileArn"
{-# INLINEABLE duprrsUserProfileArn #-}
{-# DEPRECATED userProfileArn "Use generic-lens or generic-optics with 'userProfileArn' instead"  #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserProfileName :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.UserProfileName)
duprrsUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE duprrsUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | A collection of settings.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsUserSettings :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Types.UserSettings)
duprrsUserSettings = Lens.field @"userSettings"
{-# INLINEABLE duprrsUserSettings #-}
{-# DEPRECATED userSettings "Use generic-lens or generic-optics with 'userSettings' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
duprrsResponseStatus :: Lens.Lens' DescribeUserProfileResponse Core.Int
duprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE duprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
