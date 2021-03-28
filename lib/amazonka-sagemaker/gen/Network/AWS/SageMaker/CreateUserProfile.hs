{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateUserProfile (..)
    , mkCreateUserProfile
    -- ** Request lenses
    , cupDomainId
    , cupUserProfileName
    , cupSingleSignOnUserIdentifier
    , cupSingleSignOnUserValue
    , cupTags
    , cupUserSettings

    -- * Destructuring the response
    , CreateUserProfileResponse (..)
    , mkCreateUserProfileResponse
    -- ** Response lenses
    , cuprrsUserProfileArn
    , cuprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateUserProfile' smart constructor.
data CreateUserProfile = CreateUserProfile'
  { domainId :: Types.DomainId
    -- ^ The ID of the associated Domain.
  , userProfileName :: Types.UserProfileName
    -- ^ A name for the UserProfile.
  , singleSignOnUserIdentifier :: Core.Maybe Types.SingleSignOnUserIdentifier
    -- ^ A specifier for the type of value specified in SingleSignOnUserValue. Currently, the only supported value is "UserName". If the Domain's AuthMode is SSO, this field is required. If the Domain's AuthMode is not SSO, this field cannot be specified. 
  , singleSignOnUserValue :: Core.Maybe Types.String256
    -- ^ The username of the associated AWS Single Sign-On User for this UserProfile. If the Domain's AuthMode is SSO, this field is required, and must match a valid username of a user in your directory. If the Domain's AuthMode is not SSO, this field cannot be specified. 
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Each tag consists of a key and an optional value. Tag keys must be unique per resource.
  , userSettings :: Core.Maybe Types.UserSettings
    -- ^ A collection of settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserProfile' value with any optional fields omitted.
mkCreateUserProfile
    :: Types.DomainId -- ^ 'domainId'
    -> Types.UserProfileName -- ^ 'userProfileName'
    -> CreateUserProfile
mkCreateUserProfile domainId userProfileName
  = CreateUserProfile'{domainId, userProfileName,
                       singleSignOnUserIdentifier = Core.Nothing,
                       singleSignOnUserValue = Core.Nothing, tags = Core.Nothing,
                       userSettings = Core.Nothing}

-- | The ID of the associated Domain.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDomainId :: Lens.Lens' CreateUserProfile Types.DomainId
cupDomainId = Lens.field @"domainId"
{-# INLINEABLE cupDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | A name for the UserProfile.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserProfileName :: Lens.Lens' CreateUserProfile Types.UserProfileName
cupUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE cupUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | A specifier for the type of value specified in SingleSignOnUserValue. Currently, the only supported value is "UserName". If the Domain's AuthMode is SSO, this field is required. If the Domain's AuthMode is not SSO, this field cannot be specified. 
--
-- /Note:/ Consider using 'singleSignOnUserIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSingleSignOnUserIdentifier :: Lens.Lens' CreateUserProfile (Core.Maybe Types.SingleSignOnUserIdentifier)
cupSingleSignOnUserIdentifier = Lens.field @"singleSignOnUserIdentifier"
{-# INLINEABLE cupSingleSignOnUserIdentifier #-}
{-# DEPRECATED singleSignOnUserIdentifier "Use generic-lens or generic-optics with 'singleSignOnUserIdentifier' instead"  #-}

-- | The username of the associated AWS Single Sign-On User for this UserProfile. If the Domain's AuthMode is SSO, this field is required, and must match a valid username of a user in your directory. If the Domain's AuthMode is not SSO, this field cannot be specified. 
--
-- /Note:/ Consider using 'singleSignOnUserValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSingleSignOnUserValue :: Lens.Lens' CreateUserProfile (Core.Maybe Types.String256)
cupSingleSignOnUserValue = Lens.field @"singleSignOnUserValue"
{-# INLINEABLE cupSingleSignOnUserValue #-}
{-# DEPRECATED singleSignOnUserValue "Use generic-lens or generic-optics with 'singleSignOnUserValue' instead"  #-}

-- | Each tag consists of a key and an optional value. Tag keys must be unique per resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupTags :: Lens.Lens' CreateUserProfile (Core.Maybe [Types.Tag])
cupTags = Lens.field @"tags"
{-# INLINEABLE cupTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A collection of settings.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserSettings :: Lens.Lens' CreateUserProfile (Core.Maybe Types.UserSettings)
cupUserSettings = Lens.field @"userSettings"
{-# INLINEABLE cupUserSettings #-}
{-# DEPRECATED userSettings "Use generic-lens or generic-optics with 'userSettings' instead"  #-}

instance Core.ToQuery CreateUserProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUserProfile where
        toHeaders CreateUserProfile{..}
          = Core.pure ("X-Amz-Target", "SageMaker.CreateUserProfile") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUserProfile where
        toJSON CreateUserProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  Core.Just ("UserProfileName" Core..= userProfileName),
                  ("SingleSignOnUserIdentifier" Core..=) Core.<$>
                    singleSignOnUserIdentifier,
                  ("SingleSignOnUserValue" Core..=) Core.<$> singleSignOnUserValue,
                  ("Tags" Core..=) Core.<$> tags,
                  ("UserSettings" Core..=) Core.<$> userSettings])

instance Core.AWSRequest CreateUserProfile where
        type Rs CreateUserProfile = CreateUserProfileResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateUserProfileResponse' Core.<$>
                   (x Core..:? "UserProfileArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateUserProfileResponse' smart constructor.
data CreateUserProfileResponse = CreateUserProfileResponse'
  { userProfileArn :: Core.Maybe Types.UserProfileArn
    -- ^ The user profile Amazon Resource Name (ARN).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserProfileResponse' value with any optional fields omitted.
mkCreateUserProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateUserProfileResponse
mkCreateUserProfileResponse responseStatus
  = CreateUserProfileResponse'{userProfileArn = Core.Nothing,
                               responseStatus}

-- | The user profile Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'userProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsUserProfileArn :: Lens.Lens' CreateUserProfileResponse (Core.Maybe Types.UserProfileArn)
cuprrsUserProfileArn = Lens.field @"userProfileArn"
{-# INLINEABLE cuprrsUserProfileArn #-}
{-# DEPRECATED userProfileArn "Use generic-lens or generic-optics with 'userProfileArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsResponseStatus :: Lens.Lens' CreateUserProfileResponse Core.Int
cuprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cuprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
