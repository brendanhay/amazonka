{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdToken
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an OpenID token, using a known Cognito ID. This known Cognito ID is returned by 'GetId' . You can optionally add additional logins for the identity. Supplying multiple logins creates an implicit link.
--
-- The OpenId token is valid for 10 minutes.
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetOpenIdToken
    (
    -- * Creating a request
      GetOpenIdToken (..)
    , mkGetOpenIdToken
    -- ** Request lenses
    , goitIdentityId
    , goitLogins

    -- * Destructuring the response
    , GetOpenIdTokenResponse (..)
    , mkGetOpenIdTokenResponse
    -- ** Response lenses
    , goitrrsIdentityId
    , goitrrsToken
    , goitrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the GetOpenIdToken action.
--
-- /See:/ 'mkGetOpenIdToken' smart constructor.
data GetOpenIdToken = GetOpenIdToken'
  { identityId :: Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , logins :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken)
    -- ^ A set of optional name-value pairs that map provider names to provider tokens. When using graph.facebook.com and www.amazon.com, supply the access_token returned from the provider's authflow. For accounts.google.com, an Amazon Cognito user pool provider, or any other OpenId Connect provider, always include the @id_token@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpenIdToken' value with any optional fields omitted.
mkGetOpenIdToken
    :: Types.IdentityId -- ^ 'identityId'
    -> GetOpenIdToken
mkGetOpenIdToken identityId
  = GetOpenIdToken'{identityId, logins = Core.Nothing}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitIdentityId :: Lens.Lens' GetOpenIdToken Types.IdentityId
goitIdentityId = Lens.field @"identityId"
{-# INLINEABLE goitIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. When using graph.facebook.com and www.amazon.com, supply the access_token returned from the provider's authflow. For accounts.google.com, an Amazon Cognito user pool provider, or any other OpenId Connect provider, always include the @id_token@ .
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitLogins :: Lens.Lens' GetOpenIdToken (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken))
goitLogins = Lens.field @"logins"
{-# INLINEABLE goitLogins #-}
{-# DEPRECATED logins "Use generic-lens or generic-optics with 'logins' instead"  #-}

instance Core.ToQuery GetOpenIdToken where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOpenIdToken where
        toHeaders GetOpenIdToken{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityService.GetOpenIdToken")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOpenIdToken where
        toJSON GetOpenIdToken{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityId" Core..= identityId),
                  ("Logins" Core..=) Core.<$> logins])

instance Core.AWSRequest GetOpenIdToken where
        type Rs GetOpenIdToken = GetOpenIdTokenResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOpenIdTokenResponse' Core.<$>
                   (x Core..:? "IdentityId") Core.<*> x Core..:? "Token" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned in response to a successful GetOpenIdToken request.
--
-- /See:/ 'mkGetOpenIdTokenResponse' smart constructor.
data GetOpenIdTokenResponse = GetOpenIdTokenResponse'
  { identityId :: Core.Maybe Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID. Note that the IdentityId returned may not match the one passed on input.
  , token :: Core.Maybe Types.Token
    -- ^ An OpenID token, valid for 10 minutes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpenIdTokenResponse' value with any optional fields omitted.
mkGetOpenIdTokenResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOpenIdTokenResponse
mkGetOpenIdTokenResponse responseStatus
  = GetOpenIdTokenResponse'{identityId = Core.Nothing,
                            token = Core.Nothing, responseStatus}

-- | A unique identifier in the format REGION:GUID. Note that the IdentityId returned may not match the one passed on input.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitrrsIdentityId :: Lens.Lens' GetOpenIdTokenResponse (Core.Maybe Types.IdentityId)
goitrrsIdentityId = Lens.field @"identityId"
{-# INLINEABLE goitrrsIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | An OpenID token, valid for 10 minutes.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitrrsToken :: Lens.Lens' GetOpenIdTokenResponse (Core.Maybe Types.Token)
goitrrsToken = Lens.field @"token"
{-# INLINEABLE goitrrsToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitrrsResponseStatus :: Lens.Lens' GetOpenIdTokenResponse Core.Int
goitrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE goitrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
