{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateUserProfile (..)
    , mkUpdateUserProfile
    -- ** Request lenses
    , uupDomainId
    , uupUserProfileName
    , uupUserSettings

    -- * Destructuring the response
    , UpdateUserProfileResponse (..)
    , mkUpdateUserProfileResponse
    -- ** Response lenses
    , uuprrsUserProfileArn
    , uuprrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkUpdateUserProfile' smart constructor.
data UpdateUserProfile = UpdateUserProfile'
  { domainId :: Types.DomainId
    -- ^ The domain ID.
  , userProfileName :: Types.UserProfileName
    -- ^ The user profile name.
  , userSettings :: Core.Maybe Types.UserSettings
    -- ^ A collection of settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserProfile' value with any optional fields omitted.
mkUpdateUserProfile
    :: Types.DomainId -- ^ 'domainId'
    -> Types.UserProfileName -- ^ 'userProfileName'
    -> UpdateUserProfile
mkUpdateUserProfile domainId userProfileName
  = UpdateUserProfile'{domainId, userProfileName,
                       userSettings = Core.Nothing}

-- | The domain ID.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupDomainId :: Lens.Lens' UpdateUserProfile Types.DomainId
uupDomainId = Lens.field @"domainId"
{-# INLINEABLE uupDomainId #-}
{-# DEPRECATED domainId "Use generic-lens or generic-optics with 'domainId' instead"  #-}

-- | The user profile name.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserProfileName :: Lens.Lens' UpdateUserProfile Types.UserProfileName
uupUserProfileName = Lens.field @"userProfileName"
{-# INLINEABLE uupUserProfileName #-}
{-# DEPRECATED userProfileName "Use generic-lens or generic-optics with 'userProfileName' instead"  #-}

-- | A collection of settings.
--
-- /Note:/ Consider using 'userSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserSettings :: Lens.Lens' UpdateUserProfile (Core.Maybe Types.UserSettings)
uupUserSettings = Lens.field @"userSettings"
{-# INLINEABLE uupUserSettings #-}
{-# DEPRECATED userSettings "Use generic-lens or generic-optics with 'userSettings' instead"  #-}

instance Core.ToQuery UpdateUserProfile where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateUserProfile where
        toHeaders UpdateUserProfile{..}
          = Core.pure ("X-Amz-Target", "SageMaker.UpdateUserProfile") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateUserProfile where
        toJSON UpdateUserProfile{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DomainId" Core..= domainId),
                  Core.Just ("UserProfileName" Core..= userProfileName),
                  ("UserSettings" Core..=) Core.<$> userSettings])

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
                   (x Core..:? "UserProfileArn") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateUserProfileResponse' smart constructor.
data UpdateUserProfileResponse = UpdateUserProfileResponse'
  { userProfileArn :: Core.Maybe Types.UserProfileArn
    -- ^ The user profile Amazon Resource Name (ARN).
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserProfileResponse' value with any optional fields omitted.
mkUpdateUserProfileResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateUserProfileResponse
mkUpdateUserProfileResponse responseStatus
  = UpdateUserProfileResponse'{userProfileArn = Core.Nothing,
                               responseStatus}

-- | The user profile Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'userProfileArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsUserProfileArn :: Lens.Lens' UpdateUserProfileResponse (Core.Maybe Types.UserProfileArn)
uuprrsUserProfileArn = Lens.field @"userProfileArn"
{-# INLINEABLE uuprrsUserProfileArn #-}
{-# DEPRECATED userProfileArn "Use generic-lens or generic-optics with 'userProfileArn' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprrsResponseStatus :: Lens.Lens' UpdateUserProfileResponse Core.Int
uuprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE uuprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
