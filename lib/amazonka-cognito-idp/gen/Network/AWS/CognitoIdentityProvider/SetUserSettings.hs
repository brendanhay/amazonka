{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only SMS MFA. You can't use it to configure TOTP software token MFA. To configure either type of MFA, use <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SetUserMFAPreference.html SetUserMFAPreference> instead.
module Network.AWS.CognitoIdentityProvider.SetUserSettings
  ( -- * Creating a request
    SetUserSettings (..),
    mkSetUserSettings,

    -- ** Request lenses
    susAccessToken,
    susMFAOptions,

    -- * Destructuring the response
    SetUserSettingsResponse (..),
    mkSetUserSettingsResponse,

    -- ** Response lenses
    susrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to set user settings.
--
-- /See:/ 'mkSetUserSettings' smart constructor.
data SetUserSettings = SetUserSettings'
  { -- | The access token for the set user settings request.
    accessToken :: Types.TokenModelType,
    -- | You can use this parameter only to set an SMS configuration that uses SMS for delivery.
    mFAOptions :: [Types.MFAOptionType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetUserSettings' value with any optional fields omitted.
mkSetUserSettings ::
  -- | 'accessToken'
  Types.TokenModelType ->
  SetUserSettings
mkSetUserSettings accessToken =
  SetUserSettings' {accessToken, mFAOptions = Core.mempty}

-- | The access token for the set user settings request.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
susAccessToken :: Lens.Lens' SetUserSettings Types.TokenModelType
susAccessToken = Lens.field @"accessToken"
{-# DEPRECATED susAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | You can use this parameter only to set an SMS configuration that uses SMS for delivery.
--
-- /Note:/ Consider using 'mFAOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
susMFAOptions :: Lens.Lens' SetUserSettings [Types.MFAOptionType]
susMFAOptions = Lens.field @"mFAOptions"
{-# DEPRECATED susMFAOptions "Use generic-lens or generic-optics with 'mFAOptions' instead." #-}

instance Core.FromJSON SetUserSettings where
  toJSON SetUserSettings {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AccessToken" Core..= accessToken),
            Core.Just ("MFAOptions" Core..= mFAOptions)
          ]
      )

instance Core.AWSRequest SetUserSettings where
  type Rs SetUserSettings = SetUserSettingsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.SetUserSettings"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          SetUserSettingsResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | The response from the server for a set user settings request.
--
-- /See:/ 'mkSetUserSettingsResponse' smart constructor.
newtype SetUserSettingsResponse = SetUserSettingsResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetUserSettingsResponse' value with any optional fields omitted.
mkSetUserSettingsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetUserSettingsResponse
mkSetUserSettingsResponse responseStatus =
  SetUserSettingsResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
susrrsResponseStatus :: Lens.Lens' SetUserSettingsResponse Core.Int
susrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED susrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
