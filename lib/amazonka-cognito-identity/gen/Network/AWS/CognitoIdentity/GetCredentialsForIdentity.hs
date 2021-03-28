{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.GetCredentialsForIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns credentials for the provided identity ID. Any provided logins will be validated against supported login providers. If the token is for cognito-identity.amazonaws.com, it will be passed through to AWS Security Token Service with the appropriate role for the token.
--
-- This is a public API. You do not need any credentials to call this API.
module Network.AWS.CognitoIdentity.GetCredentialsForIdentity
    (
    -- * Creating a request
      GetCredentialsForIdentity (..)
    , mkGetCredentialsForIdentity
    -- ** Request lenses
    , gcfiIdentityId
    , gcfiCustomRoleArn
    , gcfiLogins

    -- * Destructuring the response
    , GetCredentialsForIdentityResponse (..)
    , mkGetCredentialsForIdentityResponse
    -- ** Response lenses
    , gcfirrsCredentials
    , gcfirrsIdentityId
    , gcfirrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @GetCredentialsForIdentity@ action.
--
-- /See:/ 'mkGetCredentialsForIdentity' smart constructor.
data GetCredentialsForIdentity = GetCredentialsForIdentity'
  { identityId :: Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , customRoleArn :: Core.Maybe Types.CustomRoleArn
    -- ^ The Amazon Resource Name (ARN) of the role to be assumed when multiple roles were received in the token from the identity provider. For example, a SAML-based identity provider. This parameter is optional for identity providers that do not support role customization.
  , logins :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken)
    -- ^ A set of optional name-value pairs that map provider names to provider tokens. The name-value pair will follow the syntax "provider_name": "provider_user_identifier".
--
-- Logins should not be specified when trying to get credentials for an unauthenticated identity.
-- The Logins parameter is required when using identities associated with external identity providers such as FaceBook. For examples of @Logins@ maps, see the code examples in the <http://docs.aws.amazon.com/cognito/latest/developerguide/external-identity-providers.html External Identity Providers> section of the Amazon Cognito Developer Guide.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCredentialsForIdentity' value with any optional fields omitted.
mkGetCredentialsForIdentity
    :: Types.IdentityId -- ^ 'identityId'
    -> GetCredentialsForIdentity
mkGetCredentialsForIdentity identityId
  = GetCredentialsForIdentity'{identityId,
                               customRoleArn = Core.Nothing, logins = Core.Nothing}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfiIdentityId :: Lens.Lens' GetCredentialsForIdentity Types.IdentityId
gcfiIdentityId = Lens.field @"identityId"
{-# INLINEABLE gcfiIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The Amazon Resource Name (ARN) of the role to be assumed when multiple roles were received in the token from the identity provider. For example, a SAML-based identity provider. This parameter is optional for identity providers that do not support role customization.
--
-- /Note:/ Consider using 'customRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfiCustomRoleArn :: Lens.Lens' GetCredentialsForIdentity (Core.Maybe Types.CustomRoleArn)
gcfiCustomRoleArn = Lens.field @"customRoleArn"
{-# INLINEABLE gcfiCustomRoleArn #-}
{-# DEPRECATED customRoleArn "Use generic-lens or generic-optics with 'customRoleArn' instead"  #-}

-- | A set of optional name-value pairs that map provider names to provider tokens. The name-value pair will follow the syntax "provider_name": "provider_user_identifier".
--
-- Logins should not be specified when trying to get credentials for an unauthenticated identity.
-- The Logins parameter is required when using identities associated with external identity providers such as FaceBook. For examples of @Logins@ maps, see the code examples in the <http://docs.aws.amazon.com/cognito/latest/developerguide/external-identity-providers.html External Identity Providers> section of the Amazon Cognito Developer Guide.
--
-- /Note:/ Consider using 'logins' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfiLogins :: Lens.Lens' GetCredentialsForIdentity (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderToken))
gcfiLogins = Lens.field @"logins"
{-# INLINEABLE gcfiLogins #-}
{-# DEPRECATED logins "Use generic-lens or generic-optics with 'logins' instead"  #-}

instance Core.ToQuery GetCredentialsForIdentity where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCredentialsForIdentity where
        toHeaders GetCredentialsForIdentity{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityService.GetCredentialsForIdentity")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCredentialsForIdentity where
        toJSON GetCredentialsForIdentity{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityId" Core..= identityId),
                  ("CustomRoleArn" Core..=) Core.<$> customRoleArn,
                  ("Logins" Core..=) Core.<$> logins])

instance Core.AWSRequest GetCredentialsForIdentity where
        type Rs GetCredentialsForIdentity =
             GetCredentialsForIdentityResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCredentialsForIdentityResponse' Core.<$>
                   (x Core..:? "Credentials") Core.<*> x Core..:? "IdentityId"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Returned in response to a successful @GetCredentialsForIdentity@ operation.
--
-- /See:/ 'mkGetCredentialsForIdentityResponse' smart constructor.
data GetCredentialsForIdentityResponse = GetCredentialsForIdentityResponse'
  { credentials :: Core.Maybe Types.Credentials
    -- ^ Credentials for the provided identity ID.
  , identityId :: Core.Maybe Types.IdentityId
    -- ^ A unique identifier in the format REGION:GUID.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCredentialsForIdentityResponse' value with any optional fields omitted.
mkGetCredentialsForIdentityResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCredentialsForIdentityResponse
mkGetCredentialsForIdentityResponse responseStatus
  = GetCredentialsForIdentityResponse'{credentials = Core.Nothing,
                                       identityId = Core.Nothing, responseStatus}

-- | Credentials for the provided identity ID.
--
-- /Note:/ Consider using 'credentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfirrsCredentials :: Lens.Lens' GetCredentialsForIdentityResponse (Core.Maybe Types.Credentials)
gcfirrsCredentials = Lens.field @"credentials"
{-# INLINEABLE gcfirrsCredentials #-}
{-# DEPRECATED credentials "Use generic-lens or generic-optics with 'credentials' instead"  #-}

-- | A unique identifier in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfirrsIdentityId :: Lens.Lens' GetCredentialsForIdentityResponse (Core.Maybe Types.IdentityId)
gcfirrsIdentityId = Lens.field @"identityId"
{-# INLINEABLE gcfirrsIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcfirrsResponseStatus :: Lens.Lens' GetCredentialsForIdentityResponse Core.Int
gcfirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcfirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
