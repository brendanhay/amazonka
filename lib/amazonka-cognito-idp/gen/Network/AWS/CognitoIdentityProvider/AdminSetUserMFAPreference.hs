{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the user's multi-factor authentication (MFA) preference, including which MFA options are enabled and if any are preferred. Only one factor can be set as preferred. The preferred MFA factor will be used to authenticate a user if multiple factors are enabled. If multiple options are enabled and no preference is set, a challenge to choose an MFA option will be returned during sign in.
module Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
    (
    -- * Creating a request
      AdminSetUserMFAPreference (..)
    , mkAdminSetUserMFAPreference
    -- ** Request lenses
    , asumfapUsername
    , asumfapUserPoolId
    , asumfapSMSMfaSettings
    , asumfapSoftwareTokenMfaSettings

    -- * Destructuring the response
    , AdminSetUserMFAPreferenceResponse (..)
    , mkAdminSetUserMFAPreferenceResponse
    -- ** Response lenses
    , asumfaprrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAdminSetUserMFAPreference' smart constructor.
data AdminSetUserMFAPreference = AdminSetUserMFAPreference'
  { username :: Types.Username
    -- ^ The user pool username or alias.
  , userPoolId :: Types.UserPoolId
    -- ^ The user pool ID.
  , sMSMfaSettings :: Core.Maybe Types.SMSMfaSettingsType
    -- ^ The SMS text message MFA settings.
  , softwareTokenMfaSettings :: Core.Maybe Types.SoftwareTokenMfaSettingsType
    -- ^ The time-based one-time password software token MFA settings.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserMFAPreference' value with any optional fields omitted.
mkAdminSetUserMFAPreference
    :: Types.Username -- ^ 'username'
    -> Types.UserPoolId -- ^ 'userPoolId'
    -> AdminSetUserMFAPreference
mkAdminSetUserMFAPreference username userPoolId
  = AdminSetUserMFAPreference'{username, userPoolId,
                               sMSMfaSettings = Core.Nothing,
                               softwareTokenMfaSettings = Core.Nothing}

-- | The user pool username or alias.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumfapUsername :: Lens.Lens' AdminSetUserMFAPreference Types.Username
asumfapUsername = Lens.field @"username"
{-# INLINEABLE asumfapUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

-- | The user pool ID.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumfapUserPoolId :: Lens.Lens' AdminSetUserMFAPreference Types.UserPoolId
asumfapUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE asumfapUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The SMS text message MFA settings.
--
-- /Note:/ Consider using 'sMSMfaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumfapSMSMfaSettings :: Lens.Lens' AdminSetUserMFAPreference (Core.Maybe Types.SMSMfaSettingsType)
asumfapSMSMfaSettings = Lens.field @"sMSMfaSettings"
{-# INLINEABLE asumfapSMSMfaSettings #-}
{-# DEPRECATED sMSMfaSettings "Use generic-lens or generic-optics with 'sMSMfaSettings' instead"  #-}

-- | The time-based one-time password software token MFA settings.
--
-- /Note:/ Consider using 'softwareTokenMfaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumfapSoftwareTokenMfaSettings :: Lens.Lens' AdminSetUserMFAPreference (Core.Maybe Types.SoftwareTokenMfaSettingsType)
asumfapSoftwareTokenMfaSettings = Lens.field @"softwareTokenMfaSettings"
{-# INLINEABLE asumfapSoftwareTokenMfaSettings #-}
{-# DEPRECATED softwareTokenMfaSettings "Use generic-lens or generic-optics with 'softwareTokenMfaSettings' instead"  #-}

instance Core.ToQuery AdminSetUserMFAPreference where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AdminSetUserMFAPreference where
        toHeaders AdminSetUserMFAPreference{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AdminSetUserMFAPreference")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AdminSetUserMFAPreference where
        toJSON AdminSetUserMFAPreference{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Username" Core..= username),
                  Core.Just ("UserPoolId" Core..= userPoolId),
                  ("SMSMfaSettings" Core..=) Core.<$> sMSMfaSettings,
                  ("SoftwareTokenMfaSettings" Core..=) Core.<$>
                    softwareTokenMfaSettings])

instance Core.AWSRequest AdminSetUserMFAPreference where
        type Rs AdminSetUserMFAPreference =
             AdminSetUserMFAPreferenceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 AdminSetUserMFAPreferenceResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAdminSetUserMFAPreferenceResponse' smart constructor.
newtype AdminSetUserMFAPreferenceResponse = AdminSetUserMFAPreferenceResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AdminSetUserMFAPreferenceResponse' value with any optional fields omitted.
mkAdminSetUserMFAPreferenceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AdminSetUserMFAPreferenceResponse
mkAdminSetUserMFAPreferenceResponse responseStatus
  = AdminSetUserMFAPreferenceResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asumfaprrsResponseStatus :: Lens.Lens' AdminSetUserMFAPreferenceResponse Core.Int
asumfaprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE asumfaprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
