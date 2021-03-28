{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to register a user's entered TOTP code and mark the user's software token MFA status as "verified" if successful. The request takes an access token or a session string, but not both.
module Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
    (
    -- * Creating a request
      VerifySoftwareToken (..)
    , mkVerifySoftwareToken
    -- ** Request lenses
    , vstUserCode
    , vstAccessToken
    , vstFriendlyDeviceName
    , vstSession

    -- * Destructuring the response
    , VerifySoftwareTokenResponse (..)
    , mkVerifySoftwareTokenResponse
    -- ** Response lenses
    , vstrrsSession
    , vstrrsStatus
    , vstrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkVerifySoftwareToken' smart constructor.
data VerifySoftwareToken = VerifySoftwareToken'
  { userCode :: Types.SoftwareTokenMFAUserCodeType
    -- ^ The one time password computed using the secret code returned by <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken"> .
  , accessToken :: Core.Maybe Types.TokenModelType
    -- ^ The access token.
  , friendlyDeviceName :: Core.Maybe Types.FriendlyDeviceName
    -- ^ The friendly device name.
  , session :: Core.Maybe Types.SessionType
    -- ^ The session which should be passed both ways in challenge-response calls to the service.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerifySoftwareToken' value with any optional fields omitted.
mkVerifySoftwareToken
    :: Types.SoftwareTokenMFAUserCodeType -- ^ 'userCode'
    -> VerifySoftwareToken
mkVerifySoftwareToken userCode
  = VerifySoftwareToken'{userCode, accessToken = Core.Nothing,
                         friendlyDeviceName = Core.Nothing, session = Core.Nothing}

-- | The one time password computed using the secret code returned by <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AssociateSoftwareToken.html AssociateSoftwareToken"> .
--
-- /Note:/ Consider using 'userCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstUserCode :: Lens.Lens' VerifySoftwareToken Types.SoftwareTokenMFAUserCodeType
vstUserCode = Lens.field @"userCode"
{-# INLINEABLE vstUserCode #-}
{-# DEPRECATED userCode "Use generic-lens or generic-optics with 'userCode' instead"  #-}

-- | The access token.
--
-- /Note:/ Consider using 'accessToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstAccessToken :: Lens.Lens' VerifySoftwareToken (Core.Maybe Types.TokenModelType)
vstAccessToken = Lens.field @"accessToken"
{-# INLINEABLE vstAccessToken #-}
{-# DEPRECATED accessToken "Use generic-lens or generic-optics with 'accessToken' instead"  #-}

-- | The friendly device name.
--
-- /Note:/ Consider using 'friendlyDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstFriendlyDeviceName :: Lens.Lens' VerifySoftwareToken (Core.Maybe Types.FriendlyDeviceName)
vstFriendlyDeviceName = Lens.field @"friendlyDeviceName"
{-# INLINEABLE vstFriendlyDeviceName #-}
{-# DEPRECATED friendlyDeviceName "Use generic-lens or generic-optics with 'friendlyDeviceName' instead"  #-}

-- | The session which should be passed both ways in challenge-response calls to the service.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstSession :: Lens.Lens' VerifySoftwareToken (Core.Maybe Types.SessionType)
vstSession = Lens.field @"session"
{-# INLINEABLE vstSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

instance Core.ToQuery VerifySoftwareToken where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders VerifySoftwareToken where
        toHeaders VerifySoftwareToken{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.VerifySoftwareToken")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON VerifySoftwareToken where
        toJSON VerifySoftwareToken{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserCode" Core..= userCode),
                  ("AccessToken" Core..=) Core.<$> accessToken,
                  ("FriendlyDeviceName" Core..=) Core.<$> friendlyDeviceName,
                  ("Session" Core..=) Core.<$> session])

instance Core.AWSRequest VerifySoftwareToken where
        type Rs VerifySoftwareToken = VerifySoftwareTokenResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 VerifySoftwareTokenResponse' Core.<$>
                   (x Core..:? "Session") Core.<*> x Core..:? "Status" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkVerifySoftwareTokenResponse' smart constructor.
data VerifySoftwareTokenResponse = VerifySoftwareTokenResponse'
  { session :: Core.Maybe Types.SessionType
    -- ^ The session which should be passed both ways in challenge-response calls to the service.
  , status :: Core.Maybe Types.VerifySoftwareTokenResponseType
    -- ^ The status of the verify software token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerifySoftwareTokenResponse' value with any optional fields omitted.
mkVerifySoftwareTokenResponse
    :: Core.Int -- ^ 'responseStatus'
    -> VerifySoftwareTokenResponse
mkVerifySoftwareTokenResponse responseStatus
  = VerifySoftwareTokenResponse'{session = Core.Nothing,
                                 status = Core.Nothing, responseStatus}

-- | The session which should be passed both ways in challenge-response calls to the service.
--
-- /Note:/ Consider using 'session' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstrrsSession :: Lens.Lens' VerifySoftwareTokenResponse (Core.Maybe Types.SessionType)
vstrrsSession = Lens.field @"session"
{-# INLINEABLE vstrrsSession #-}
{-# DEPRECATED session "Use generic-lens or generic-optics with 'session' instead"  #-}

-- | The status of the verify software token.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstrrsStatus :: Lens.Lens' VerifySoftwareTokenResponse (Core.Maybe Types.VerifySoftwareTokenResponseType)
vstrrsStatus = Lens.field @"status"
{-# INLINEABLE vstrrsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vstrrsResponseStatus :: Lens.Lens' VerifySoftwareTokenResponse Core.Int
vstrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE vstrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
