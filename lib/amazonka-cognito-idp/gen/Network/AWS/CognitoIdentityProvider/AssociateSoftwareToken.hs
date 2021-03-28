{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      AssociateSoftwareToken (..)
    , mkAssociateSoftwareToken
    -- ** Request lenses
    , astAccessToken
    , astSession

    -- * Destructuring the response
    , AssociateSoftwareTokenResponse (..)
    , mkAssociateSoftwareTokenResponse
    -- ** Response lenses
    , astrrsSecretCode
    , astrrsSession
    , astrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkAssociateSoftwareToken' smart constructor.
data AssociateSoftwareToken = AssociateSoftwareToken'
  { accessToken :: Core.Maybe Types.TokenModelType
    -- ^ The access token.
  , session :: Core.Maybe Types.SessionType
    -- ^ The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSoftwareToken' value with any optional fields omitted.
mkAssociateSoftwareToken
    :: AssociateSoftwareToken
mkAssociateSoftwareToken
  = AssociateSoftwareToken'{accessToken = Core.Nothing,
                            session = Core.Nothing}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astAccessToken :: Lens.Lens' AssociateSoftwareToken (Core.Maybe Types.TokenModelType)
astAccessToken = Lens.field @"accessToken"
{-# INLINEABLE astAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astSession :: Lens.Lens' AssociateSoftwareToken (Core.Maybe Types.SessionType)
astSession = Lens.field @"session"
{-# INLINEABLE astSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

instance Core.ToQuery AssociateSoftwareToken where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AssociateSoftwareToken where
        toHeaders AssociateSoftwareToken{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.AssociateSoftwareToken")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON AssociateSoftwareToken where
        toJSON AssociateSoftwareToken{..}
          = Core.object
              (Core.catMaybes
                 [("AccessToken" Core..=) Core.<$> accessToken,
                  ("Session" Core..=) Core.<$> session])

instance Core.AWSRequest AssociateSoftwareToken where
        type Rs AssociateSoftwareToken = AssociateSoftwareTokenResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 AssociateSoftwareTokenResponse' Core.<$>
                   (x Core..:? "SecretCode") Core.<*> x Core..:? "Session" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAssociateSoftwareTokenResponse' smart constructor.
data AssociateSoftwareTokenResponse = AssociateSoftwareTokenResponse'
  { secretCode :: Core.Maybe Types.SecretCode
    -- ^ A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
  , session :: Core.Maybe Types.Session
    -- ^ The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AssociateSoftwareTokenResponse' value with any optional fields omitted.
mkAssociateSoftwareTokenResponse
    :: Core.Int -- ^ 'responseStatus'
    -> AssociateSoftwareTokenResponse
mkAssociateSoftwareTokenResponse responseStatus
  = AssociateSoftwareTokenResponse'{secretCode = Core.Nothing,
                                    session = Core.Nothing, responseStatus}

-- | A unique generated shared secret code that is used in the TOTP algorithm to generate a one time code.
--
-- /Note:/ Consider using 'secretCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrrsSecretCode :: Lens.Lens' AssociateSoftwareTokenResponse (Core.Maybe Types.SecretCode)
astrrsSecretCode = Lens.field @"secretCode"
{-# INLINEABLE astrrsSecretCode #-}
{-# DEPRECATED secretCode "Use generic-lens or generic-optics with 'secretCode' instead"  #-}

-- | The session which should be passed both ways in challenge-response calls to the service. This allows authentication of the user as part of the MFA setup process.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrrsSession :: Lens.Lens' AssociateSoftwareTokenResponse (Core.Maybe Types.Session)
astrrsSession = Lens.field @"session"
{-# INLINEABLE astrrsSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
astrrsResponseStatus :: Lens.Lens' AssociateSoftwareTokenResponse Core.Int
astrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE astrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
