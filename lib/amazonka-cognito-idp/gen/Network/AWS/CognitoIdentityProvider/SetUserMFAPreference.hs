{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user's multi-factor authentication (MFA) method preference, including which MFA factors are enabled and if any are preferred. Only one factor can be set as preferred. The preferred MFA factor will be used to authenticate a user if multiple factors are enabled. If multiple options are enabled and no preference is set, a challenge to choose an MFA option will be returned during sign in. If an MFA type is enabled for a user, the user will be prompted for MFA during all sign in attempts, unless device tracking is turned on and the device has been trusted. If you would like MFA to be applied selectively based on the assessed risk level of sign in attempts, disable MFA for users and turn on Adaptive Authentication for the user pool.
module Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
  ( -- * Creating a request
    SetUserMFAPreference (..),
    mkSetUserMFAPreference,

    -- ** Request lenses
    sumfapAccessToken,
    sumfapSMSMfaSettings,
    sumfapSoftwareTokenMfaSettings,

    -- * Destructuring the response
    SetUserMFAPreferenceResponse (..),
    mkSetUserMFAPreferenceResponse,

    -- ** Response lenses
    sumfaprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetUserMFAPreference' smart constructor.
data SetUserMFAPreference = SetUserMFAPreference'
  { -- | The access token for the user.
    accessToken :: Types.AccessToken,
    -- | The SMS text message multi-factor authentication (MFA) settings.
    sMSMfaSettings :: Core.Maybe Types.SMSMfaSettingsType,
    -- | The time-based one-time password software token MFA settings.
    softwareTokenMfaSettings :: Core.Maybe Types.SoftwareTokenMfaSettingsType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetUserMFAPreference' value with any optional fields omitted.
mkSetUserMFAPreference ::
  -- | 'accessToken'
  Types.AccessToken ->
  SetUserMFAPreference
mkSetUserMFAPreference accessToken =
  SetUserMFAPreference'
    { accessToken,
      sMSMfaSettings = Core.Nothing,
      softwareTokenMfaSettings = Core.Nothing
    }

-- | The access token for the user.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumfapAccessToken :: Lens.Lens' SetUserMFAPreference Types.AccessToken
sumfapAccessToken = Lens.field @"accessToken"
{-# DEPRECATED sumfapAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The SMS text message multi-factor authentication (MFA) settings.
--
-- /Note:/ Consider using 'sMSMfaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumfapSMSMfaSettings :: Lens.Lens' SetUserMFAPreference (Core.Maybe Types.SMSMfaSettingsType)
sumfapSMSMfaSettings = Lens.field @"sMSMfaSettings"
{-# DEPRECATED sumfapSMSMfaSettings "Use generic-lens or generic-optics with 'sMSMfaSettings' instead." #-}

-- | The time-based one-time password software token MFA settings.
--
-- /Note:/ Consider using 'softwareTokenMfaSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumfapSoftwareTokenMfaSettings :: Lens.Lens' SetUserMFAPreference (Core.Maybe Types.SoftwareTokenMfaSettingsType)
sumfapSoftwareTokenMfaSettings = Lens.field @"softwareTokenMfaSettings"
{-# DEPRECATED sumfapSoftwareTokenMfaSettings "Use generic-lens or generic-optics with 'softwareTokenMfaSettings' instead." #-}

instance Core.FromJSON SetUserMFAPreference where
  toJSON SetUserMFAPreference {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccessToken" Core..= accessToken),
            ("SMSMfaSettings" Core..=) Core.<$> sMSMfaSettings,
            ("SoftwareTokenMfaSettings" Core..=)
              Core.<$> softwareTokenMfaSettings
          ]
      )

instance Core.AWSRequest SetUserMFAPreference where
  type Rs SetUserMFAPreference = SetUserMFAPreferenceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.SetUserMFAPreference"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetUserMFAPreferenceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSetUserMFAPreferenceResponse' smart constructor.
newtype SetUserMFAPreferenceResponse = SetUserMFAPreferenceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetUserMFAPreferenceResponse' value with any optional fields omitted.
mkSetUserMFAPreferenceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetUserMFAPreferenceResponse
mkSetUserMFAPreferenceResponse responseStatus =
  SetUserMFAPreferenceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sumfaprrsResponseStatus :: Lens.Lens' SetUserMFAPreferenceResponse Core.Int
sumfaprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sumfaprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
