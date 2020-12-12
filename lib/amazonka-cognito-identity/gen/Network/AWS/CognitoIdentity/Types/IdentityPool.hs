{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentity.Types.IdentityPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentity.Types.IdentityPool
  ( IdentityPool (..),

    -- * Smart constructor
    mkIdentityPool,

    -- * Lenses
    ipSamlProviderARNs,
    ipSupportedLoginProviders,
    ipAllowClassicFlow,
    ipDeveloperProviderName,
    ipIdentityPoolTags,
    ipOpenIdConnectProviderARNs,
    ipCognitoIdentityProviders,
    ipIdentityPoolId,
    ipIdentityPoolName,
    ipAllowUnauthenticatedIdentities,
  )
where

import Network.AWS.CognitoIdentity.Types.CognitoIdentityProvider
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing an Amazon Cognito identity pool.
--
-- /See:/ 'mkIdentityPool' smart constructor.
data IdentityPool = IdentityPool'
  { samlProviderARNs ::
      Lude.Maybe [Lude.Text],
    supportedLoginProviders ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    allowClassicFlow :: Lude.Maybe Lude.Bool,
    developerProviderName :: Lude.Maybe Lude.Text,
    identityPoolTags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    openIdConnectProviderARNs :: Lude.Maybe [Lude.Text],
    cognitoIdentityProviders :: Lude.Maybe [CognitoIdentityProvider],
    identityPoolId :: Lude.Text,
    identityPoolName :: Lude.Text,
    allowUnauthenticatedIdentities :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IdentityPool' with the minimum fields required to make a request.
--
-- * 'allowClassicFlow' - Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
-- * 'allowUnauthenticatedIdentities' - TRUE if the identity pool supports unauthenticated logins.
-- * 'cognitoIdentityProviders' - A list representing an Amazon Cognito user pool and its client ID.
-- * 'developerProviderName' - The "domain" by which Cognito will refer to your users.
-- * 'identityPoolId' - An identity pool ID in the format REGION:GUID.
-- * 'identityPoolName' - A string that you provide.
-- * 'identityPoolTags' - The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
-- * 'openIdConnectProviderARNs' - A list of OpendID Connect provider ARNs.
-- * 'samlProviderARNs' - An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
-- * 'supportedLoginProviders' - Optional key:value pairs mapping provider names to provider app IDs.
mkIdentityPool ::
  -- | 'identityPoolId'
  Lude.Text ->
  -- | 'identityPoolName'
  Lude.Text ->
  -- | 'allowUnauthenticatedIdentities'
  Lude.Bool ->
  IdentityPool
mkIdentityPool
  pIdentityPoolId_
  pIdentityPoolName_
  pAllowUnauthenticatedIdentities_ =
    IdentityPool'
      { samlProviderARNs = Lude.Nothing,
        supportedLoginProviders = Lude.Nothing,
        allowClassicFlow = Lude.Nothing,
        developerProviderName = Lude.Nothing,
        identityPoolTags = Lude.Nothing,
        openIdConnectProviderARNs = Lude.Nothing,
        cognitoIdentityProviders = Lude.Nothing,
        identityPoolId = pIdentityPoolId_,
        identityPoolName = pIdentityPoolName_,
        allowUnauthenticatedIdentities = pAllowUnauthenticatedIdentities_
      }

-- | An array of Amazon Resource Names (ARNs) of the SAML provider for your identity pool.
--
-- /Note:/ Consider using 'samlProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipSamlProviderARNs :: Lens.Lens' IdentityPool (Lude.Maybe [Lude.Text])
ipSamlProviderARNs = Lens.lens (samlProviderARNs :: IdentityPool -> Lude.Maybe [Lude.Text]) (\s a -> s {samlProviderARNs = a} :: IdentityPool)
{-# DEPRECATED ipSamlProviderARNs "Use generic-lens or generic-optics with 'samlProviderARNs' instead." #-}

-- | Optional key:value pairs mapping provider names to provider app IDs.
--
-- /Note:/ Consider using 'supportedLoginProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipSupportedLoginProviders :: Lens.Lens' IdentityPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ipSupportedLoginProviders = Lens.lens (supportedLoginProviders :: IdentityPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {supportedLoginProviders = a} :: IdentityPool)
{-# DEPRECATED ipSupportedLoginProviders "Use generic-lens or generic-optics with 'supportedLoginProviders' instead." #-}

-- | Enables or disables the Basic (Classic) authentication flow. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/authentication-flow.html Identity Pools (Federated Identities) Authentication Flow> in the /Amazon Cognito Developer Guide/ .
--
-- /Note:/ Consider using 'allowClassicFlow' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipAllowClassicFlow :: Lens.Lens' IdentityPool (Lude.Maybe Lude.Bool)
ipAllowClassicFlow = Lens.lens (allowClassicFlow :: IdentityPool -> Lude.Maybe Lude.Bool) (\s a -> s {allowClassicFlow = a} :: IdentityPool)
{-# DEPRECATED ipAllowClassicFlow "Use generic-lens or generic-optics with 'allowClassicFlow' instead." #-}

-- | The "domain" by which Cognito will refer to your users.
--
-- /Note:/ Consider using 'developerProviderName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipDeveloperProviderName :: Lens.Lens' IdentityPool (Lude.Maybe Lude.Text)
ipDeveloperProviderName = Lens.lens (developerProviderName :: IdentityPool -> Lude.Maybe Lude.Text) (\s a -> s {developerProviderName = a} :: IdentityPool)
{-# DEPRECATED ipDeveloperProviderName "Use generic-lens or generic-optics with 'developerProviderName' instead." #-}

-- | The tags that are assigned to the identity pool. A tag is a label that you can apply to identity pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'identityPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIdentityPoolTags :: Lens.Lens' IdentityPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
ipIdentityPoolTags = Lens.lens (identityPoolTags :: IdentityPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {identityPoolTags = a} :: IdentityPool)
{-# DEPRECATED ipIdentityPoolTags "Use generic-lens or generic-optics with 'identityPoolTags' instead." #-}

-- | A list of OpendID Connect provider ARNs.
--
-- /Note:/ Consider using 'openIdConnectProviderARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipOpenIdConnectProviderARNs :: Lens.Lens' IdentityPool (Lude.Maybe [Lude.Text])
ipOpenIdConnectProviderARNs = Lens.lens (openIdConnectProviderARNs :: IdentityPool -> Lude.Maybe [Lude.Text]) (\s a -> s {openIdConnectProviderARNs = a} :: IdentityPool)
{-# DEPRECATED ipOpenIdConnectProviderARNs "Use generic-lens or generic-optics with 'openIdConnectProviderARNs' instead." #-}

-- | A list representing an Amazon Cognito user pool and its client ID.
--
-- /Note:/ Consider using 'cognitoIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipCognitoIdentityProviders :: Lens.Lens' IdentityPool (Lude.Maybe [CognitoIdentityProvider])
ipCognitoIdentityProviders = Lens.lens (cognitoIdentityProviders :: IdentityPool -> Lude.Maybe [CognitoIdentityProvider]) (\s a -> s {cognitoIdentityProviders = a} :: IdentityPool)
{-# DEPRECATED ipCognitoIdentityProviders "Use generic-lens or generic-optics with 'cognitoIdentityProviders' instead." #-}

-- | An identity pool ID in the format REGION:GUID.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIdentityPoolId :: Lens.Lens' IdentityPool Lude.Text
ipIdentityPoolId = Lens.lens (identityPoolId :: IdentityPool -> Lude.Text) (\s a -> s {identityPoolId = a} :: IdentityPool)
{-# DEPRECATED ipIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A string that you provide.
--
-- /Note:/ Consider using 'identityPoolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipIdentityPoolName :: Lens.Lens' IdentityPool Lude.Text
ipIdentityPoolName = Lens.lens (identityPoolName :: IdentityPool -> Lude.Text) (\s a -> s {identityPoolName = a} :: IdentityPool)
{-# DEPRECATED ipIdentityPoolName "Use generic-lens or generic-optics with 'identityPoolName' instead." #-}

-- | TRUE if the identity pool supports unauthenticated logins.
--
-- /Note:/ Consider using 'allowUnauthenticatedIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipAllowUnauthenticatedIdentities :: Lens.Lens' IdentityPool Lude.Bool
ipAllowUnauthenticatedIdentities = Lens.lens (allowUnauthenticatedIdentities :: IdentityPool -> Lude.Bool) (\s a -> s {allowUnauthenticatedIdentities = a} :: IdentityPool)
{-# DEPRECATED ipAllowUnauthenticatedIdentities "Use generic-lens or generic-optics with 'allowUnauthenticatedIdentities' instead." #-}

instance Lude.FromJSON IdentityPool where
  parseJSON =
    Lude.withObject
      "IdentityPool"
      ( \x ->
          IdentityPool'
            Lude.<$> (x Lude..:? "SamlProviderARNs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "SupportedLoginProviders" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AllowClassicFlow")
            Lude.<*> (x Lude..:? "DeveloperProviderName")
            Lude.<*> (x Lude..:? "IdentityPoolTags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "OpenIdConnectProviderARNs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CognitoIdentityProviders" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "IdentityPoolId")
            Lude.<*> (x Lude..: "IdentityPoolName")
            Lude.<*> (x Lude..: "AllowUnauthenticatedIdentities")
      )

instance Lude.ToJSON IdentityPool where
  toJSON IdentityPool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SamlProviderARNs" Lude..=) Lude.<$> samlProviderARNs,
            ("SupportedLoginProviders" Lude..=)
              Lude.<$> supportedLoginProviders,
            ("AllowClassicFlow" Lude..=) Lude.<$> allowClassicFlow,
            ("DeveloperProviderName" Lude..=) Lude.<$> developerProviderName,
            ("IdentityPoolTags" Lude..=) Lude.<$> identityPoolTags,
            ("OpenIdConnectProviderARNs" Lude..=)
              Lude.<$> openIdConnectProviderARNs,
            ("CognitoIdentityProviders" Lude..=)
              Lude.<$> cognitoIdentityProviders,
            Lude.Just ("IdentityPoolId" Lude..= identityPoolId),
            Lude.Just ("IdentityPoolName" Lude..= identityPoolName),
            Lude.Just
              ( "AllowUnauthenticatedIdentities"
                  Lude..= allowUnauthenticatedIdentities
              )
          ]
      )
