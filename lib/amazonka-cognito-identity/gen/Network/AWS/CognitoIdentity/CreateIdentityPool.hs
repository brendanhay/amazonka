{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.CreateIdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new identity pool. The identity pool is a store of user identity information that is specific to your AWS account. The keys for @SupportedLoginProviders@ are as follows:
--
--
--     * Facebook: @graph.facebook.com@ 
--
--
--     * Google: @accounts.google.com@ 
--
--
--     * Amazon: @www.amazon.com@ 
--
--
--     * Twitter: @api.twitter.com@ 
--
--
--     * Digits: @www.digits.com@ 
--
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.CreateIdentityPool
    (
    -- * Creating a request
      CreateIdentityPool (..)
    , mkCreateIdentityPool
    -- ** Request lenses
    , cipIdentityPoolName
    , cipAllowUnauthenticatedIdentities
    , cipAllowClassicFlow
    , cipCognitoIdentityProviders
    , cipDeveloperProviderName
    , cipIdentityPoolTags
    , cipOpenIdConnectProviderARNs
    , cipSamlProviderARNs
    , cipSupportedLoginProviders

     -- * Destructuring the response
    , Types.IdentityPool (..)
    , Types.mkIdentityPool
    -- ** Response lenses
    , Types.ipIdentityPoolId
    , Types.ipIdentityPoolName
    , Types.ipAllowUnauthenticatedIdentities
    , Types.ipAllowClassicFlow
    , Types.ipCognitoIdentityProviders
    , Types.ipDeveloperProviderName
    , Types.ipIdentityPoolTags
    , Types.ipOpenIdConnectProviderARNs
    , Types.ipSamlProviderARNs
    , Types.ipSupportedLoginProviders
    ) where

import qualified Network.AWS.CognitoIdentity.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the CreateIdentityPool action.
--
-- /See:/ 'mkCreateIdentityPool' smart constructor.
data CreateIdentityPool = CreateIdentityPool'
  { identityPoolName :: Types.IdentityPoolName
    -- ^ A string that you provide.
  , allowUnauthenticatedIdentities :: Core.Bool
    -- ^ TRUE if the identity pool supports unauthenticated logins.
  , allowClassicFlow :: Core.Maybe Core.Bool
    -- ^ Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
  , cognitoIdentityProviders :: Core.Maybe [Types.CognitoIdentityProvider]
    -- ^ An array of Amazon Cognito user pools and their client IDs.
  , developerProviderName :: Core.Maybe Types.DeveloperProviderName
    -- ^ The "domain" by which Cognito will refer to your users. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (@.@ ), underscore (@_@ ), and dash (@-@ ).
--
-- Once you have set a developer provider name, you cannot change it. Please take care in setting this parameter.
  , identityPoolTags :: Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType)
    -- ^ Tags to assign to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
  , openIdConnectProviderARNs :: Core.Maybe [Types.ARNString]
    -- ^ A list of OpendID Connect provider ARNs.
  , samlProviderARNs :: Core.Maybe [Types.ARNString]
    -- ^ An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
  , supportedLoginProviders :: Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderId)
    -- ^ Optional key:value pairs mapping provider names to provider app IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateIdentityPool' value with any optional fields omitted.
mkCreateIdentityPool
    :: Types.IdentityPoolName -- ^ 'identityPoolName'
    -> Core.Bool -- ^ 'allowUnauthenticatedIdentities'
    -> CreateIdentityPool
mkCreateIdentityPool identityPoolName
  allowUnauthenticatedIdentities
  = CreateIdentityPool'{identityPoolName,
                        allowUnauthenticatedIdentities, allowClassicFlow = Core.Nothing,
                        cognitoIdentityProviders = Core.Nothing,
                        developerProviderName = Core.Nothing,
                        identityPoolTags = Core.Nothing,
                        openIdConnectProviderARNs = Core.Nothing,
                        samlProviderARNs = Core.Nothing,
                        supportedLoginProviders = Core.Nothing}

-- | A string that you provide.
--
-- /Note:/ Consider using 'identityPoolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipIdentityPoolName :: Lens.Lens' CreateIdentityPool Types.IdentityPoolName
cipIdentityPoolName = Lens.field @"identityPoolName"
{-# INLINEABLE cipIdentityPoolName #-}
{-# DEPRECATED identityPoolName "Use generic-lens or generic-optics with 'identityPoolName' instead"  #-}

-- | TRUE if the identity pool supports unauthenticated logins.
--
-- /Note:/ Consider using 'allowUnauthenticatedIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipAllowUnauthenticatedIdentities :: Lens.Lens' CreateIdentityPool Core.Bool
cipAllowUnauthenticatedIdentities = Lens.field @"allowUnauthenticatedIdentities"
{-# INLINEABLE cipAllowUnauthenticatedIdentities #-}
{-# DEPRECATED allowUnauthenticatedIdentities "Use generic-lens or generic-optics with 'allowUnauthenticatedIdentities' instead"  #-}

-- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'allowClassicFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipAllowClassicFlow :: Lens.Lens' CreateIdentityPool (Core.Maybe Core.Bool)
cipAllowClassicFlow = Lens.field @"allowClassicFlow"
{-# INLINEABLE cipAllowClassicFlow #-}
{-# DEPRECATED allowClassicFlow "Use generic-lens or generic-optics with 'allowClassicFlow' instead"  #-}

-- | An array of Amazon Cognito user pools and their client IDs.
--
-- /Note:/ Consider using 'cognitoIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipCognitoIdentityProviders :: Lens.Lens' CreateIdentityPool (Core.Maybe [Types.CognitoIdentityProvider])
cipCognitoIdentityProviders = Lens.field @"cognitoIdentityProviders"
{-# INLINEABLE cipCognitoIdentityProviders #-}
{-# DEPRECATED cognitoIdentityProviders "Use generic-lens or generic-optics with 'cognitoIdentityProviders' instead"  #-}

-- | The "domain" by which Cognito will refer to your users. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (@.@ ), underscore (@_@ ), and dash (@-@ ).
--
-- Once you have set a developer provider name, you cannot change it. Please take care in setting this parameter.
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipDeveloperProviderName :: Lens.Lens' CreateIdentityPool (Core.Maybe Types.DeveloperProviderName)
cipDeveloperProviderName = Lens.field @"developerProviderName"
{-# INLINEABLE cipDeveloperProviderName #-}
{-# DEPRECATED developerProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead"  #-}

-- | Tags to assign to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'identityPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipIdentityPoolTags :: Lens.Lens' CreateIdentityPool (Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType))
cipIdentityPoolTags = Lens.field @"identityPoolTags"
{-# INLINEABLE cipIdentityPoolTags #-}
{-# DEPRECATED identityPoolTags "Use generic-lens or generic-optics with 'identityPoolTags' instead"  #-}

-- | A list of OpendID Connect provider ARNs.
--
-- /Note:/ Consider using 'openIdConnectProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipOpenIdConnectProviderARNs :: Lens.Lens' CreateIdentityPool (Core.Maybe [Types.ARNString])
cipOpenIdConnectProviderARNs = Lens.field @"openIdConnectProviderARNs"
{-# INLINEABLE cipOpenIdConnectProviderARNs #-}
{-# DEPRECATED openIdConnectProviderARNs "Use generic-lens or generic-optics with 'openIdConnectProviderARNs' instead"  #-}

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
--
-- /Note:/ Consider using 'samlProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipSamlProviderARNs :: Lens.Lens' CreateIdentityPool (Core.Maybe [Types.ARNString])
cipSamlProviderARNs = Lens.field @"samlProviderARNs"
{-# INLINEABLE cipSamlProviderARNs #-}
{-# DEPRECATED samlProviderARNs "Use generic-lens or generic-optics with 'samlProviderARNs' instead"  #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
--
-- /Note:/ Consider using 'supportedLoginProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipSupportedLoginProviders :: Lens.Lens' CreateIdentityPool (Core.Maybe (Core.HashMap Types.IdentityProviderName Types.IdentityProviderId))
cipSupportedLoginProviders = Lens.field @"supportedLoginProviders"
{-# INLINEABLE cipSupportedLoginProviders #-}
{-# DEPRECATED supportedLoginProviders "Use generic-lens or generic-optics with 'supportedLoginProviders' instead"  #-}

instance Core.ToQuery CreateIdentityPool where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateIdentityPool where
        toHeaders CreateIdentityPool{..}
          = Core.pure
              ("X-Amz-Target", "AWSCognitoIdentityService.CreateIdentityPool")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateIdentityPool where
        toJSON CreateIdentityPool{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("IdentityPoolName" Core..= identityPoolName),
                  Core.Just
                    ("AllowUnauthenticatedIdentities" Core..=
                       allowUnauthenticatedIdentities),
                  ("AllowClassicFlow" Core..=) Core.<$> allowClassicFlow,
                  ("CognitoIdentityProviders" Core..=) Core.<$>
                    cognitoIdentityProviders,
                  ("DeveloperProviderName" Core..=) Core.<$> developerProviderName,
                  ("IdentityPoolTags" Core..=) Core.<$> identityPoolTags,
                  ("OpenIdConnectProviderARNs" Core..=) Core.<$>
                    openIdConnectProviderARNs,
                  ("SamlProviderARNs" Core..=) Core.<$> samlProviderARNs,
                  ("SupportedLoginProviders" Core..=) Core.<$>
                    supportedLoginProviders])

instance Core.AWSRequest CreateIdentityPool where
        type Rs CreateIdentityPool = Types.IdentityPool
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
