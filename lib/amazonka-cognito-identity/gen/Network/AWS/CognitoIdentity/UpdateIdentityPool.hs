{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.UpdateIdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an identity pool.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.UpdateIdentityPool
  ( -- * Creating a request
    UpdateIdentityPool (..),
    mkUpdateIdentityPool,

    -- ** Request lenses
    uipSamlProviderARNs,
    uipSupportedLoginProviders,
    uipIdentityPoolId,
    uipAllowClassicFlow,
    uipIdentityPoolName,
    uipDeveloperProviderName,
    uipIdentityPoolTags,
    uipOpenIdConnectProviderARNs,
    uipCognitoIdentityProviders,
    uipAllowUnauthenticatedIdentities,

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

-- | An object representing an Amazon Cognito identity pool.
--
-- /See:/ 'mkUpdateIdentityPool' smart constructor.
data UpdateIdentityPool = UpdateIdentityPool'
  { -- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
    samlProviderARNs :: Lude.Maybe [Lude.Text],
    -- | Optional key:value pairs mapping provider names to provider app IDs.
    supportedLoginProviders :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | An identity pool ID in the format REGION:GUID.
    identityPoolId :: Lude.Text,
    -- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
    allowClassicFlow :: Lude.Maybe Lude.Bool,
    -- | A string that you provide.
    identityPoolName :: Lude.Text,
    -- | The "domain" by which Cognito will refer to your users.
    developerProviderName :: Lude.Maybe Lude.Text,
    -- | The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
    identityPoolTags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | A list of OpendID Connect provider ARNs.
    openIdConnectProviderARNs :: Lude.Maybe [Lude.Text],
    -- | A list representing an Amazon Cognito user pool and its client ID.
    cognitoIdentityProviders :: Lude.Maybe [CognitoIdentityProvider],
    -- | TRUE if the identity pool supports unauthenticated logins.
    allowUnauthenticatedIdentities :: Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateIdentityPool' with the minimum fields required to make a request.
--
-- * 'samlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
-- * 'supportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'allowClassicFlow' - Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
-- * 'identityPoolName' - A string that you provide.
-- * 'developerProviderName' - The "domain" by which Cognito will refer to your users.
-- * 'identityPoolTags' - The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
-- * 'openIdConnectProviderARNs' - A list of OpendID Connect provider ARNs.
-- * 'cognitoIdentityProviders' - A list representing an Amazon Cognito user pool and its client ID.
-- * 'allowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
mkUpdateIdentityPool ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'identityPoolName'
  Lude.Text ->
  -- | 'allowUnauthenticatedIdentities'
  Lude.Bool ->
  UpdateIdentityPool
mkUpdateIdentityPool
  pIdentityPoolId_
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    UpdateIdentityPool'
      { samlProviderARNs = Lude.Nothing,
        supportedLoginProviders = Lude.Nothing,
        identityPoolId = pIdentityPoolId_,
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
uipSamlProviderARNs :: Lens.Lens' UpdateIdentityPool (Lude.Maybe [Lude.Text])
uipSamlProviderARNs = Lens.lens (samlProviderARNs :: UpdateIdentityPool -> Lude.Maybe [Lude.Text]) (\s a -> s {samlProviderARNs = a} :: UpdateIdentityPool)
{-# DEPRECATED uipSamlProviderARNs "Use generic-lens or generic-optics with 'samlProviderARNs' instead." #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
--
-- /Note:/ Consider using 'supportedLoginProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipSupportedLoginProviders :: Lens.Lens' UpdateIdentityPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uipSupportedLoginProviders = Lens.lens (supportedLoginProviders :: UpdateIdentityPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {supportedLoginProviders = a} :: UpdateIdentityPool)
{-# DEPRECATED uipSupportedLoginProviders "Use generic-lens or generic-optics with 'supportedLoginProviders' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdentityPoolId :: Lens.Lens' UpdateIdentityPool Lude.Text
uipIdentityPoolId = Lens.lens (identityPoolId :: UpdateIdentityPool -> Lude.Text) (\s a -> s {identityPoolId = a} :: UpdateIdentityPool)
{-# DEPRECATED uipIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'allowClassicFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipAllowClassicFlow :: Lens.Lens' UpdateIdentityPool (Lude.Maybe Lude.Bool)
uipAllowClassicFlow = Lens.lens (allowClassicFlow :: UpdateIdentityPool -> Lude.Maybe Lude.Bool) (\s a -> s {allowClassicFlow = a} :: UpdateIdentityPool)
{-# DEPRECATED uipAllowClassicFlow "Use generic-lens or generic-optics with 'allowClassicFlow' instead." #-}

-- | A string that you provide.
--
-- /Note:/ Consider using 'identityPoolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdentityPoolName :: Lens.Lens' UpdateIdentityPool Lude.Text
uipIdentityPoolName = Lens.lens (identityPoolName :: UpdateIdentityPool -> Lude.Text) (\s a -> s {identityPoolName = a} :: UpdateIdentityPool)
{-# DEPRECATED uipIdentityPoolName "Use generic-lens or generic-optics with 'identityPoolName' instead." #-}

-- | The "domain" by which Cognito will refer to your users.
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipDeveloperProviderName :: Lens.Lens' UpdateIdentityPool (Lude.Maybe Lude.Text)
uipDeveloperProviderName = Lens.lens (developerProviderName :: UpdateIdentityPool -> Lude.Maybe Lude.Text) (\s a -> s {developerProviderName = a} :: UpdateIdentityPool)
{-# DEPRECATED uipDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'identityPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipIdentityPoolTags :: Lens.Lens' UpdateIdentityPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uipIdentityPoolTags = Lens.lens (identityPoolTags :: UpdateIdentityPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {identityPoolTags = a} :: UpdateIdentityPool)
{-# DEPRECATED uipIdentityPoolTags "Use generic-lens or generic-optics with 'identityPoolTags' instead." #-}

-- | A list of OpendID Connect provider ARNs.
--
-- /Note:/ Consider using 'openIdConnectProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipOpenIdConnectProviderARNs :: Lens.Lens' UpdateIdentityPool (Lude.Maybe [Lude.Text])
uipOpenIdConnectProviderARNs = Lens.lens (openIdConnectProviderARNs :: UpdateIdentityPool -> Lude.Maybe [Lude.Text]) (\s a -> s {openIdConnectProviderARNs = a} :: UpdateIdentityPool)
{-# DEPRECATED uipOpenIdConnectProviderARNs "Use generic-lens or generic-optics with 'openIdConnectProviderARNs' instead." #-}

-- | A list representing an Amazon Cognito user pool and its client ID.
--
-- /Note:/ Consider using 'cognitoIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipCognitoIdentityProviders :: Lens.Lens' UpdateIdentityPool (Lude.Maybe [CognitoIdentityProvider])
uipCognitoIdentityProviders = Lens.lens (cognitoIdentityProviders :: UpdateIdentityPool -> Lude.Maybe [CognitoIdentityProvider]) (\s a -> s {cognitoIdentityProviders = a} :: UpdateIdentityPool)
{-# DEPRECATED uipCognitoIdentityProviders "Use generic-lens or generic-optics with 'cognitoIdentityProviders' instead." #-}

-- | TRUE if the identity pool supports unauthenticated logins.
--
-- /Note:/ Consider using 'allowUnauthenticatedIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uipAllowUnauthenticatedIdentities :: Lens.Lens' UpdateIdentityPool Lude.Bool
uipAllowUnauthenticatedIdentities = Lens.lens (allowUnauthenticatedIdentities :: UpdateIdentityPool -> Lude.Bool) (\s a -> s {allowUnauthenticatedIdentities = a} :: UpdateIdentityPool)
{-# DEPRECATED uipAllowUnauthenticatedIdentities "Use generic-lens or generic-optics with 'allowUnauthenticatedIdentities' instead." #-}

instance Lude.AWSRequest UpdateIdentityPool where
  type Rs UpdateIdentityPool = IdentityPool
  request = Req.postJSON cognitoIdentityService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateIdentityPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityService.UpdateIdentityPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateIdentityPool where
  toJSON UpdateIdentityPool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SamlProviderARNs" Lude..=) Lude.<$> samlProviderARNs,
            ("SupportedLoginProviders" Lude..=)
              Lude.<$> supportedLoginProviders,
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
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

instance Lude.ToPath UpdateIdentityPool where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateIdentityPool where
  toQuery = Lude.const Lude.mempty
