{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a unique generated shared secret key code for the user account. The request takes an access token or a session string, but not both.
module Network.AWS.CognitoIdentityProvider.AssociateSoftwareToken
  ( -- * Creating a request
    AssociateSoftwareToken (..),
    mkAssociateSoftwareToken,

    -- ** Request lenses
    astAccessToken,
    astSession,

    -- * Destructuring the response
    AssociateSoftwareTokenResponse (..),
    mkAssociateSoftwareTokenResponse,

    -- ** Response lenses
    astrrsSecretCode,
    astrrsSession,
    astrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { -- | The access token.
    accessToken :: Core.Maybe Types.TokenModelType,
    -- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
    session :: Core.Maybe Types.SessionType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSoftwareToken' value with any optional fields omitted.
mkAssociateSoftwareToken ::
  AssociateSoftwareToken
mkAssociateSoftwareToken =
  AssociateSoftwareToken'
    { accessToken = Core.Nothing,
      session = Core.Nothing
    }

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astAccessToken :: Lens.Lens' AssociateSoftwareToken (Core.Maybe Types.TokenModelType)
astAccessToken = Lens.field @"accessToken"
{-# DEPRECATED astAccessToken "Use generic-lens or generic-optics with 'accessToken' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astSession :: Lens.Lens' AssociateSoftwareToken (Core.Maybe Types.SessionType)
astSession = Lens.field @"session"
{-# DEPRECATED astSession "Use generic-lens or generic-optics with 'session' instead." #-}

instance Core.FromJSON AssociateSoftwareToken where
  toJSON AssociateSoftwareToken {..} =
    Core.object
      ( Core.catMaybes
          [ ("AccessToken" Core..=) Core.<$> accessToken,
            ("Session" Core..=) Core.<$> session
          ]
      )

instance Core.AWSRequest AssociateSoftwareToken where
  type Rs AssociateSoftwareToken = AssociateSoftwareTokenResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.AssociateSoftwareToken"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateSoftwareTokenResponse'
            Core.<$> (x Core..:? "SecretCode")
            Core.<*> (x Core..:? "Session")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAssociateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { -- | A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
    secretCode :: Core.Maybe Types.SecretCode,
    -- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
    session :: Core.Maybe Types.Session,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSoftwareTokenResponse' value with any optional fields omitted.
mkAssociateSoftwareTokenResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AssociateSoftwareTokenResponse
mkAssociateSoftwareTokenResponse responseStatus =
  AssociateSoftwareTokenResponse'
    { secretCode = Core.Nothing,
      session = Core.Nothing,
      responseStatus
    }

-- | A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
--
-- /Note:/ Consider using 'secretCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrrsSecretCode :: Lens.Lens' AssociateSoftwareTokenResponse (Core.Maybe Types.SecretCode)
astrrsSecretCode = Lens.field @"secretCode"
{-# DEPRECATED astrrsSecretCode "Use generic-lens or generic-optics with 'secretCode' instead." #-}

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrrsSession :: Lens.Lens' AssociateSoftwareTokenResponse (Core.Maybe Types.Session)
astrrsSession = Lens.field @"session"
{-# DEPRECATED astrrsSession "Use generic-lens or generic-optics with 'session' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrrsResponseStatus :: Lens.Lens' AssociateSoftwareTokenResponse Core.Int
astrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED astrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
