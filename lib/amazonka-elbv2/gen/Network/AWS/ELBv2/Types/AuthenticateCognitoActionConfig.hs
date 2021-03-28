{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.AuthenticateCognitoActionConfig
  ( AuthenticateCognitoActionConfig (..)
  -- * Smart constructor
  , mkAuthenticateCognitoActionConfig
  -- * Lenses
  , acacUserPoolArn
  , acacUserPoolClientId
  , acacUserPoolDomain
  , acacAuthenticationRequestExtraParams
  , acacOnUnauthenticatedRequest
  , acacScope
  , acacSessionCookieName
  , acacSessionTimeout
  ) where

import qualified Network.AWS.ELBv2.Types.AuthenticateCognitoActionAuthenticationRequestParamName as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateCognitoActionAuthenticationRequestParamValue as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateCognitoActionConditionalBehaviorEnum as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateCognitoActionScope as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateCognitoActionUserPoolArn as Types
import qualified Network.AWS.ELBv2.Types.AuthenticateCognitoActionUserPoolClientId as Types
import qualified Network.AWS.ELBv2.Types.SessionCookieName as Types
import qualified Network.AWS.ELBv2.Types.UserPoolDomain as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Request parameters to use when integrating with Amazon Cognito to authenticate users.
--
-- /See:/ 'mkAuthenticateCognitoActionConfig' smart constructor.
data AuthenticateCognitoActionConfig = AuthenticateCognitoActionConfig'
  { userPoolArn :: Types.AuthenticateCognitoActionUserPoolArn
    -- ^ The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
  , userPoolClientId :: Types.AuthenticateCognitoActionUserPoolClientId
    -- ^ The ID of the Amazon Cognito user pool client.
  , userPoolDomain :: Types.UserPoolDomain
    -- ^ The domain prefix or fully-qualified domain name of the Amazon Cognito user pool.
  , authenticationRequestExtraParams :: Core.Maybe (Core.HashMap Types.AuthenticateCognitoActionAuthenticationRequestParamName Types.AuthenticateCognitoActionAuthenticationRequestParamValue)
    -- ^ The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
  , onUnauthenticatedRequest :: Core.Maybe Types.AuthenticateCognitoActionConditionalBehaviorEnum
    -- ^ The behavior if the user is not authenticated. The following are possible values:
--
--
--     * deny- Return an HTTP 401 Unauthorized error.
--
--
--     * allow- Allow the request to be forwarded to the target.
--
--
--     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
--
--
  , scope :: Core.Maybe Types.AuthenticateCognitoActionScope
    -- ^ The set of user claims to be requested from the IdP. The default is @openid@ .
--
-- To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
  , sessionCookieName :: Core.Maybe Types.SessionCookieName
    -- ^ The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
  , sessionTimeout :: Core.Maybe Core.Integer
    -- ^ The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthenticateCognitoActionConfig' value with any optional fields omitted.
mkAuthenticateCognitoActionConfig
    :: Types.AuthenticateCognitoActionUserPoolArn -- ^ 'userPoolArn'
    -> Types.AuthenticateCognitoActionUserPoolClientId -- ^ 'userPoolClientId'
    -> Types.UserPoolDomain -- ^ 'userPoolDomain'
    -> AuthenticateCognitoActionConfig
mkAuthenticateCognitoActionConfig userPoolArn userPoolClientId
  userPoolDomain
  = AuthenticateCognitoActionConfig'{userPoolArn, userPoolClientId,
                                     userPoolDomain,
                                     authenticationRequestExtraParams = Core.Nothing,
                                     onUnauthenticatedRequest = Core.Nothing, scope = Core.Nothing,
                                     sessionCookieName = Core.Nothing,
                                     sessionTimeout = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'userPoolArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacUserPoolArn :: Lens.Lens' AuthenticateCognitoActionConfig Types.AuthenticateCognitoActionUserPoolArn
acacUserPoolArn = Lens.field @"userPoolArn"
{-# INLINEABLE acacUserPoolArn #-}
{-# DEPRECATED userPoolArn "Use generic-lens or generic-optics with 'userPoolArn' instead"  #-}

-- | The ID of the Amazon Cognito user pool client.
--
-- /Note:/ Consider using 'userPoolClientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacUserPoolClientId :: Lens.Lens' AuthenticateCognitoActionConfig Types.AuthenticateCognitoActionUserPoolClientId
acacUserPoolClientId = Lens.field @"userPoolClientId"
{-# INLINEABLE acacUserPoolClientId #-}
{-# DEPRECATED userPoolClientId "Use generic-lens or generic-optics with 'userPoolClientId' instead"  #-}

-- | The domain prefix or fully-qualified domain name of the Amazon Cognito user pool.
--
-- /Note:/ Consider using 'userPoolDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacUserPoolDomain :: Lens.Lens' AuthenticateCognitoActionConfig Types.UserPoolDomain
acacUserPoolDomain = Lens.field @"userPoolDomain"
{-# INLINEABLE acacUserPoolDomain #-}
{-# DEPRECATED userPoolDomain "Use generic-lens or generic-optics with 'userPoolDomain' instead"  #-}

-- | The query parameters (up to 10) to include in the redirect request to the authorization endpoint.
--
-- /Note:/ Consider using 'authenticationRequestExtraParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacAuthenticationRequestExtraParams :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe (Core.HashMap Types.AuthenticateCognitoActionAuthenticationRequestParamName Types.AuthenticateCognitoActionAuthenticationRequestParamValue))
acacAuthenticationRequestExtraParams = Lens.field @"authenticationRequestExtraParams"
{-# INLINEABLE acacAuthenticationRequestExtraParams #-}
{-# DEPRECATED authenticationRequestExtraParams "Use generic-lens or generic-optics with 'authenticationRequestExtraParams' instead"  #-}

-- | The behavior if the user is not authenticated. The following are possible values:
--
--
--     * deny- Return an HTTP 401 Unauthorized error.
--
--
--     * allow- Allow the request to be forwarded to the target.
--
--
--     * authenticate- Redirect the request to the IdP authorization endpoint. This is the default value.
--
--
--
-- /Note:/ Consider using 'onUnauthenticatedRequest' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacOnUnauthenticatedRequest :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe Types.AuthenticateCognitoActionConditionalBehaviorEnum)
acacOnUnauthenticatedRequest = Lens.field @"onUnauthenticatedRequest"
{-# INLINEABLE acacOnUnauthenticatedRequest #-}
{-# DEPRECATED onUnauthenticatedRequest "Use generic-lens or generic-optics with 'onUnauthenticatedRequest' instead"  #-}

-- | The set of user claims to be requested from the IdP. The default is @openid@ .
--
-- To verify which scope values your IdP supports and how to separate multiple values, see the documentation for your IdP.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacScope :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe Types.AuthenticateCognitoActionScope)
acacScope = Lens.field @"scope"
{-# INLINEABLE acacScope #-}
{-# DEPRECATED scope "Use generic-lens or generic-optics with 'scope' instead"  #-}

-- | The name of the cookie used to maintain session information. The default is AWSELBAuthSessionCookie.
--
-- /Note:/ Consider using 'sessionCookieName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacSessionCookieName :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe Types.SessionCookieName)
acacSessionCookieName = Lens.field @"sessionCookieName"
{-# INLINEABLE acacSessionCookieName #-}
{-# DEPRECATED sessionCookieName "Use generic-lens or generic-optics with 'sessionCookieName' instead"  #-}

-- | The maximum duration of the authentication session, in seconds. The default is 604800 seconds (7 days).
--
-- /Note:/ Consider using 'sessionTimeout' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acacSessionTimeout :: Lens.Lens' AuthenticateCognitoActionConfig (Core.Maybe Core.Integer)
acacSessionTimeout = Lens.field @"sessionTimeout"
{-# INLINEABLE acacSessionTimeout #-}
{-# DEPRECATED sessionTimeout "Use generic-lens or generic-optics with 'sessionTimeout' instead"  #-}

instance Core.ToQuery AuthenticateCognitoActionConfig where
        toQuery AuthenticateCognitoActionConfig{..}
          = Core.toQueryPair "UserPoolArn" userPoolArn Core.<>
              Core.toQueryPair "UserPoolClientId" userPoolClientId
              Core.<> Core.toQueryPair "UserPoolDomain" userPoolDomain
              Core.<>
              Core.toQueryPair "AuthenticationRequestExtraParams"
                (Core.maybe Core.mempty (Core.toQueryMap "entry" "key" "value")
                   authenticationRequestExtraParams)
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "OnUnauthenticatedRequest")
                onUnauthenticatedRequest
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Scope") scope
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SessionCookieName")
                sessionCookieName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SessionTimeout")
                sessionTimeout

instance Core.FromXML AuthenticateCognitoActionConfig where
        parseXML x
          = AuthenticateCognitoActionConfig' Core.<$>
              (x Core..@ "UserPoolArn") Core.<*> x Core..@ "UserPoolClientId"
                Core.<*> x Core..@ "UserPoolDomain"
                Core.<*>
                x Core..@? "AuthenticationRequestExtraParams" Core..<@>
                  Core.parseXMLMap "entry" "key" "value"
                Core.<*> x Core..@? "OnUnauthenticatedRequest"
                Core.<*> x Core..@? "Scope"
                Core.<*> x Core..@? "SessionCookieName"
                Core.<*> x Core..@? "SessionTimeout"
