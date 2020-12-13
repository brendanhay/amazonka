{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateIdentityPool (..),
    mkCreateIdentityPool,

    -- ** Request lenses
    cipSamlProviderARNs,
    cipSupportedLoginProviders,
    cipAllowClassicFlow,
    cipIdentityPoolName,
    cipDeveloperProviderName,
    cipIdentityPoolTags,
    cipOpenIdConnectProviderARNs,
    cipCognitoIdentityProviders,
    cipAllowUnauthenticatedIdentities,

    -- * Destructuring the response
    IdentityPool (..),
    mkIdentityPool,

    -- ** Response lenses
    ipSamlProviderARNs,
    ipSupportedLoginProviders,
    ipIdentityPoolId,
    ipAllowClassicFlow,
    ipIdentityPoolName,
    ipDeveloperProviderName,
    ipIdentityPoolTags,
    ipOpenIdConnectProviderARNs,
    ipCognitoIdentityProviders,
    ipAllowUnauthenticatedIdentities,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to the CreateIdentityPool action.
--
-- /See:/ 'mkCreateIdentityPool' smart constructor.
data CreateIdentityPool = CreateIdentityPool'
  { -- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
    samlProviderARNs :: Lude.Maybe [Lude.Text],
    -- | Optional key:value pairs mapping provider names to provider app IDs.
    supportedLoginProviders :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
    allowClassicFlow :: Lude.Maybe Lude.Bool,
    -- | A string that you provide.
    identityPoolName :: Lude.Text,
    -- | The "domain" by which Cognito will refer to your users. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (@.@ ), underscore (@_@ ), and dash (@-@ ).
    --
    -- Once you have set a developer provider name, you cannot change it. Please take care in setting this parameter.
    developerProviderName :: Lude.Maybe Lude.Text,
    -- | Tags to assign to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
    identityPoolTags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A list of OpendID Connect provider ARNs.
    openIdConnectProviderARNs :: Lude.Maybe [Lude.Text],
    -- | An array of Amazon Cognito user pools and their client IDs.
    cognitoIdentityProviders :: Lude.Maybe [CognitoIdentityProvider],
    -- | TRUE if the identity pool supports unauthenticated logins.
    allowUnauthenticatedIdentities :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateIdentityPool' with the minimum fields required to make a request.
--
-- * 'samlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
-- * 'supportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
-- * 'allowClassicFlow' - Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
-- * 'identityPoolName' - A string that you provide.
-- * 'developerProviderName' - The "domain" by which Cognito will refer to your users. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (@.@ ), underscore (@_@ ), and dash (@-@ ).
--
-- Once you have set a developer provider name, you cannot change it. Please take care in setting this parameter.
-- * 'identityPoolTags' - Tags to assign to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
-- * 'openIdConnectProviderARNs' - A list of OpendID Connect provider ARNs.
-- * 'cognitoIdentityProviders' - An array of Amazon Cognito user pools and their client IDs.
-- * 'allowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
mkCreateIdentityPool ::
  -- | 'identityPoolName'
  Lude.Text ->
  -- | 'allowUnauthenticatedIdentities'
  Lude.Bool ->
  CreateIdentityPool
mkCreateIdentityPool
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    CreateIdentityPool'
      { samlProviderARNs = Lude.Nothing,
        supportedLoginProviders = Lude.Nothing,
        allowClassicFlow = Lude.Nothing,
        identityPoolName = pIdentityPoolName_,
        developerProviderName = Lude.Nothing,
        identityPoolTags = Lude.Nothing,
        openIdConnectProviderARNs = Lude.Nothing,
        cognitoIdentityProviders = Lude.Nothing,
        allowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities_
      }

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
--
-- /Note:/ Consider using 'samlProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipSamlProviderARNs :: Lens.Lens' CreateIdentityPool (Lude.Maybe [Lude.Text])
cipSamlProviderARNs = Lens.lens (samlProviderARNs :: CreateIdentityPool -> Lude.Maybe [Lude.Text]) (\s a -> s {samlProviderARNs = a} :: CreateIdentityPool)
{-# DEPRECATED cipSamlProviderARNs "Use generic-lens or generic-optics with 'samlProviderARNs' instead." #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
--
-- /Note:/ Consider using 'supportedLoginProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipSupportedLoginProviders :: Lens.Lens' CreateIdentityPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cipSupportedLoginProviders = Lens.lens (supportedLoginProviders :: CreateIdentityPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {supportedLoginProviders = a} :: CreateIdentityPool)
{-# DEPRECATED cipSupportedLoginProviders "Use generic-lens or generic-optics with 'supportedLoginProviders' instead." #-}

-- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'allowClassicFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipAllowClassicFlow :: Lens.Lens' CreateIdentityPool (Lude.Maybe Lude.Bool)
cipAllowClassicFlow = Lens.lens (allowClassicFlow :: CreateIdentityPool -> Lude.Maybe Lude.Bool) (\s a -> s {allowClassicFlow = a} :: CreateIdentityPool)
{-# DEPRECATED cipAllowClassicFlow "Use generic-lens or generic-optics with 'allowClassicFlow' instead." #-}

-- | A string that you provide.
--
-- /Note:/ Consider using 'identityPoolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipIdentityPoolName :: Lens.Lens' CreateIdentityPool Lude.Text
cipIdentityPoolName = Lens.lens (identityPoolName :: CreateIdentityPool -> Lude.Text) (\s a -> s {identityPoolName = a} :: CreateIdentityPool)
{-# DEPRECATED cipIdentityPoolName "Use generic-lens or generic-optics with 'identityPoolName' instead." #-}

-- | The "domain" by which Cognito will refer to your users. This name acts as a placeholder that allows your backend and the Cognito service to communicate about the developer provider. For the @DeveloperProviderName@ , you can use letters as well as period (@.@ ), underscore (@_@ ), and dash (@-@ ).
--
-- Once you have set a developer provider name, you cannot change it. Please take care in setting this parameter.
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipDeveloperProviderName :: Lens.Lens' CreateIdentityPool (Lude.Maybe Lude.Text)
cipDeveloperProviderName = Lens.lens (developerProviderName :: CreateIdentityPool -> Lude.Maybe Lude.Text) (\s a -> s {developerProviderName = a} :: CreateIdentityPool)
{-# DEPRECATED cipDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | Tags to assign to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'identityPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipIdentityPoolTags :: Lens.Lens' CreateIdentityPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cipIdentityPoolTags = Lens.lens (identityPoolTags :: CreateIdentityPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {identityPoolTags = a} :: CreateIdentityPool)
{-# DEPRECATED cipIdentityPoolTags "Use generic-lens or generic-optics with 'identityPoolTags' instead." #-}

-- | A list of OpendID Connect provider ARNs.
--
-- /Note:/ Consider using 'openIdConnectProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipOpenIdConnectProviderARNs :: Lens.Lens' CreateIdentityPool (Lude.Maybe [Lude.Text])
cipOpenIdConnectProviderARNs = Lens.lens (openIdConnectProviderARNs :: CreateIdentityPool -> Lude.Maybe [Lude.Text]) (\s a -> s {openIdConnectProviderARNs = a} :: CreateIdentityPool)
{-# DEPRECATED cipOpenIdConnectProviderARNs "Use generic-lens or generic-optics with 'openIdConnectProviderARNs' instead." #-}

-- | An array of Amazon Cognito user pools and their client IDs.
--
-- /Note:/ Consider using 'cognitoIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipCognitoIdentityProviders :: Lens.Lens' CreateIdentityPool (Lude.Maybe [CognitoIdentityProvider])
cipCognitoIdentityProviders = Lens.lens (cognitoIdentityProviders :: CreateIdentityPool -> Lude.Maybe [CognitoIdentityProvider]) (\s a -> s {cognitoIdentityProviders = a} :: CreateIdentityPool)
{-# DEPRECATED cipCognitoIdentityProviders "Use generic-lens or generic-optics with 'cognitoIdentityProviders' instead." #-}

-- | TRUE if the identity pool supports unauthenticated logins.
--
-- /Note:/ Consider using 'allowUnauthenticatedIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cipAllowUnauthenticatedIdentities :: Lens.Lens' CreateIdentityPool Lude.Bool
cipAllowUnauthenticatedIdentities = Lens.lens (allowUnauthenticatedIdentities :: CreateIdentityPool -> Lude.Bool) (\s a -> s {allowUnauthenticatedIdentities = a} :: CreateIdentityPool)
{-# DEPRECATED cipAllowUnauthenticatedIdentities "Use generic-lens or generic-optics with 'allowUnauthenticatedIdentities' instead." #-}

instance Lude.AWSRequest CreateIdentityPool where
  type Rs CreateIdentityPool = IdentityPool
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateIdentityPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.CreateIdentityPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateIdentityPool where
  toJSON CreateIdentityPool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SamlProviderARNs" Lude..=) Lude.<$> samlProviderARNs,
            ("SupportedLoginProviders" Lude..=)
              Lude.<$> supportedLoginProviders,
            ("AllowClassicFlow" Lude..=) Lude.<$> allowClassicFlow,
            Lude.Just ("IdentityPoolName" Lude..= identityPoolName),
            ("DeveloperProviderName" Lude..=) Lude.<$> developerProviderName,
            ("IdentityPoolTags" Lude..=) Lude.<$> identityPoolTags,
            ("OpenIdConnectProviderARNs" Lude..=)
              Lude.<$> openIdConnectProviderARNs,
            ("CognitoIdentityProviders" Lude..=)
              Lude.<$> cognitoIdentityProviders,
            Lude.Just
              ( "AllowUnauthenticatedIdentities"
                  Lude..= allowUnauthenticatedIdentities
              )
          ]
      )

instance Lude.ToPath CreateIdentityPool where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateIdentityPool where
  toQuery = Lude.const Lude.mempty
