{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetOpenIdTokenForDeveloperIdentity (..),
    mkGetOpenIdTokenForDeveloperIdentity,

    -- ** Request lenses
    goitfdiIdentityPoolId,
    goitfdiLogins,
    goitfdiIdentityId,
    goitfdiTokenDuration,

    -- * Destructuring the response
    GetOpenIdTokenForDeveloperIdentityResponse (..),
    mkGetOpenIdTokenForDeveloperIdentityResponse,

    -- ** Response lenses
    goitfdirrsIdentityId,
    goitfdirrsToken,
    goitfdirrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetOpenIdTokenForDeveloperIdentity@ action.
--
-- /See:/ 'mkGetOpenIdTokenForDeveloperIdentity' smart constructor.
data GetOpenIdTokenForDeveloperIdentity = GetOpenIdTokenForDeveloperIdentity'
  { -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Types.IdentityPoolId,
    -- | A set of optional name-value pairs that map provider names to provider tokens. Each name-value pair represents a user from a public provider or developer provider. If the user is from a developer provider, the name-value pair will follow the syntax @"developer_provider_name": "developer_user_identifier"@ . The developer provider is the "domain" by which Cognito will refer to your users; you provided this domain while creating/updating the identity pool. The developer user identifier is an identifier from your backend that uniquely identifies a user. When you create an identity pool, you can specify the supported logins.
    logins :: Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken,
    -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Types.IdentityId,
    -- | The expiration time of the token, in seconds. You can specify a custom expiration time for the token so that you can cache it. If you don't provide an expiration time, the token is valid for 15 minutes. You can exchange the token with Amazon STS for temporary AWS credentials, which are valid for a maximum of one hour. The maximum token duration you can set is 24 hours. You should take care in setting the expiration time for a token, as there are significant security implications: an attacker could use a leaked token to access your AWS resources for the token's duration.
    tokenDuration :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpenIdTokenForDeveloperIdentity' value with any optional fields omitted.
mkGetOpenIdTokenForDeveloperIdentity ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  GetOpenIdTokenForDeveloperIdentity
mkGetOpenIdTokenForDeveloperIdentity identityPoolId =
  GetOpenIdTokenForDeveloperIdentity'
    { identityPoolId,
      logins = Core.mempty,
      identityId = Core.Nothing,
      tokenDuration = Core.Nothing
    }

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiIdentityPoolId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity Types.IdentityPoolId
goitfdiIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED goitfdiIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. Each name-value pair represents a user from a public provider or developer provider. If the user is from a developer provider, the name-value pair will follow the syntax @"developer_provider_name": "developer_user_identifier"@ . The developer provider is the "domain" by which Cognito will refer to your users; you provided this domain while creating/updating the identity pool. The developer user identifier is an identifier from your backend that uniquely identifies a user. When you create an identity pool, you can specify the supported logins.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiLogins :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken)
goitfdiLogins = Lens.field @"logins"
{-# DEPRECATED goitfdiLogins "Use generic-lens or generic-optics with 'logins' instead." #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiIdentityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.Maybe Types.IdentityId)
goitfdiIdentityId = Lens.field @"identityId"
{-# DEPRECATED goitfdiIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | The expiration time of the token, in seconds. You can specify a custom expiration time for the token so that you can cache it. If you don't provide an expiration time, the token is valid for 15 minutes. You can exchange the token with Amazon STS for temporary AWS credentials, which are valid for a maximum of one hour. The maximum token duration you can set is 24 hours. You should take care in setting the expiration time for a token, as there are significant security implications: an attacker could use a leaked token to access your AWS resources for the token's duration.
--
-- /Note:/ Consider using 'tokenDuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdiTokenDuration :: Lens.Lens' GetOpenIdTokenForDeveloperIdentity (Core.Maybe Core.Natural)
goitfdiTokenDuration = Lens.field @"tokenDuration"
{-# DEPRECATED goitfdiTokenDuration "Use generic-lens or generic-optics with 'tokenDuration' instead." #-}

instance Core.FromJSON GetOpenIdTokenForDeveloperIdentity where
  toJSON GetOpenIdTokenForDeveloperIdentity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IdentityPoolId" Core..= identityPoolId),
            Core.Just ("Logins" Core..= logins),
            ("IdentityId" Core..=) Core.<$> identityId,
            ("TokenDuration" Core..=) Core.<$> tokenDuration
          ]
      )

instance Core.AWSRequest GetOpenIdTokenForDeveloperIdentity where
  type
    Rs GetOpenIdTokenForDeveloperIdentity =
      GetOpenIdTokenForDeveloperIdentityResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityService.GetOpenIdTokenForDeveloperIdentity"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpenIdTokenForDeveloperIdentityResponse'
            Core.<$> (x Core..:? "IdentityId")
            Core.<*> (x Core..:? "Token")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Returned in response to a successful @GetOpenIdTokenForDeveloperIdentity@ request.
--
-- /See:/ 'mkGetOpenIdTokenForDeveloperIdentityResponse' smart constructor.
data GetOpenIdTokenForDeveloperIdentityResponse = GetOpenIdTokenForDeveloperIdentityResponse'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Maybe Types.IdentityId,
    -- | An OpenID token.
    token :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpenIdTokenForDeveloperIdentityResponse' value with any optional fields omitted.
mkGetOpenIdTokenForDeveloperIdentityResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOpenIdTokenForDeveloperIdentityResponse
mkGetOpenIdTokenForDeveloperIdentityResponse responseStatus =
  GetOpenIdTokenForDeveloperIdentityResponse'
    { identityId =
        Core.Nothing,
      token = Core.Nothing,
      responseStatus
    }

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirrsIdentityId :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Core.Maybe Types.IdentityId)
goitfdirrsIdentityId = Lens.field @"identityId"
{-# DEPRECATED goitfdirrsIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | An OpenID token.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirrsToken :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse (Core.Maybe Types.Token)
goitfdirrsToken = Lens.field @"token"
{-# DEPRECATED goitfdirrsToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goitfdirrsResponseStatus :: Lens.Lens' GetOpenIdTokenForDeveloperIdentityResponse Core.Int
goitfdirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED goitfdirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
