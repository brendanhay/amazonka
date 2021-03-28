{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers (or retrieves) a Cognito @IdentityId@ and an OpenID Connect token for a user authenticated by your backend authentication process. Supplying multiple logins will create an implicit linked account. You can only specify one developer provider as part of the @Logins@ map, which is linked to the identity pool. The developer provider is the "domain" by which Cognito will refer to your users.
--
-- You can use @GetOpenIdTokenForDeveloperIdentity@ to create a new identity and to link new logins (that is, user credentials issued by a public provider or developer provider) to an existing identity. When you want to create a new identity, the @IdentityId@ should be null. When you want to associate a new login with an existing authenticated/unauthenticated identity, you can do so by providing the existing @IdentityId@ . This API will create the identity in the specified @IdentityPoolId@ .
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.GetOpenIdTokenForDeveloperIdentity
    (
    -- * Creating a request
      GetOpenIdTokenForDeveloperIdentity (..)
    , mkGetOpenIdTokenForDeveloperIdentity
    -- ** Request lenses
    , goitfdiIdentityPoolId
    , goitfdiLogins
    , goitfdiIdentityId
    , goitfdiTokenDuration

    -- * Destructuring the response
    , GetOpenIdTokenForDeveloperIdentityResponse (..)
    , mkGetOpenIdTokenForDeveloperIdentityResponse
    -- ** Response lenses
    , goitfdirrsIdentityId
    , goitfdirrsToken
    , goitfdirrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetOpenIdTokenForDeveloperIdentity@ action.
--
-- /See:/ 'mkGetOpenIdTokenForDeveloperIdentity' smart constructor.
data GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentity'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ An identity pool ID in the format REGION:GUID.
  , logins :: Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken
    -- ^ A set of optional name-value pairs that map provider names to provider tokens. Each name-value pair represents a user from a public provider or developer provider. If the user is from a developer provider, the name-value pair will follow the syntax @"developer_provider_name": "developer_user_identifier"@ . The developer provider is the "domain" by which Cognito will refer to your users; you provided this domain while creating/updating the identity pool. The developer user identifier is an identifier from your backend that uniquely identifies a user. When you create an identity pool, you can specify the supported logins.
  , identityId :: Core.Maybe Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , tokenDuration :: Core.Maybe Core.Natural
    -- ^ The expiration time of the token, in seconds. You can specify a custom expiration time for the token so that you can cache it. If you don't provide an expiration time, the token is valid for 15 minutes. You can exchange the token with Amazon STS for temporary AWS credentials, which are valid for a maximum of one hour. The maximum token duration you can set is 24 hours. You should take care in setting the expiration time for a token, as there are significant security implications: an attacker could use a leaked token to access your AWS resources for the token's duration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpenIdTokenForDeveloperIdentity' value with any optional fields omitted.
mkGetOpenIdTokenForDeveloperIdentity
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> GetOpenIdTokenForDeveloperIdentity
mkGetOpenIdTokenForDeveloperIdentity identityPoolId
  = GetOpenIdTokenForDeveloperIdentity'{identityPoolId,
                                        logins = Core.mempty, identityId = Core.Nothing,
                                        tokenDuration = Core.Nothing}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiIdentityPoolId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity Types.IdentityPoolId
goitfdiIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE goitfdiIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. Each name-value pair represents a user from a public provider or developer provider. If the user is from a developer provider, the name-value pair will follow the syntax @"developer_provider_name": "developer_user_identifier"@ . The developer provider is the "domain" by which Cognito will refer to your users; you provided this domain while creating/updating the identity pool. The developer user identifier is an identifier from your backend that uniquely identifies a user. When you create an identity pool, you can specify the supported logins.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiLogins :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken)
goitfdiLogins = Lens.field @"logins"
{-# INLINEABLE goitfdiLogins #-}
{-# DEPRECATED logins "Use generic-lens or generic-optics with 'logins' instead"  #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiIdentityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.Maybe Types.IdentityId)
goitfdiIdentityId = Lens.field @"identityId"
{-# INLINEABLE goitfdiIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The expiration time of the token, in seconds. You can specify a custom expiration time for the token so that you can cache it. If you don't provide an expiration time, the token is valid for 15 minutes. You can exchange the token with Amazon STS for temporary AWS credentials, which are valid for a maximum of one hour. The maximum token duration you can set is 24 hours. You should take care in setting the expiration time for a token, as there are significant security implications: an attacker could use a leaked token to access your AWS resources for the token's duration.
--
-- /Note:/ Consider using 'tokenDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiTokenDuration :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.Maybe Core.Natural)
goitfdiTokenDuration = Lens.field @"tokenDuration"
{-# INLINEABLE goitfdiTokenDuration #-}
{-# DEPRECATED tokenDuration "Use generic-lens or generic-optics with 'tokenDuration' instead"  #-}

instance Core.ToQuery GetOpenIdTokenForDeveloperIdentity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetOpenIdTokenForDeveloperIdentity where
        toHeaders GetOpenIdTokenForDeveloperIdentity{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityService.GetOpenIdTokenForDeveloperIdentity")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetOpenIdTokenForDeveloperIdentity where
        toJSON GetOpenIdTokenForDeveloperIdentity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityPoolId" Core..= identityPoolId),
                  Core.Just ("Logins" Core..= logins),
                  ("IdentityId" Core..=) Core.<$> identityId,
                  ("TokenDuration" Core..=) Core.<$> tokenDuration])

instance Core.AWSRequest GetOpenIdTokenForDeveloperIdentity where
        type Rs GetOpenIdTokenForDeveloperIdentity =
             GetOpenIdTokenForDeveloperIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetOpenIdTokenForDeveloperIdentityResponse' Core.<$>
                   (x Core..:? "IdentityId") Core.<*> x Core..:? "Token" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned in response to a successful @GetOpenIdTokenForDeveloperIdentity@ request.
--
-- /See:/ 'mkGetOpenIdTokenForDeveloperIdentityResponse' smart constructor.
data GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse'
  { identityId :: Core.Maybe Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , token :: Core.Maybe Types.Token
    -- ^ An OpenID token.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpenIdTokenForDeveloperIdentityResponse' value with any optional fields omitted.
mkGetOpenIdTokenForDeveloperIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetOpenIdTokenForDeveloperIdentityResponse
mkGetOpenIdTokenForDeveloperIdentityResponse responseStatus
  = GetOpenIdTokenForDeveloperIdentityResponse'{identityId =
                                                  Core.Nothing,
                                                token = Core.Nothing, responseStatus}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirrsIdentityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Core.Maybe Types.IdentityId)
goitfdirrsIdentityId = Lens.field @"identityId"
{-# INLINEABLE goitfdirrsIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | An OpenID token.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirrsToken :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Core.Maybe Types.Token)
goitfdirrsToken = Lens.field @"token"
{-# INLINEABLE goitfdirrsToken #-}
{-# DEPRECATED token "Use generic-lens or generic-optics with 'token' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirrsResponseStatus :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse Core.Int
goitfdirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE goitfdirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
