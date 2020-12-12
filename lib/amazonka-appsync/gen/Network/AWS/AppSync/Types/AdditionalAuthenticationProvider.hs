{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
  ( AdditionalAuthenticationProvider (..),

    -- * Smart constructor
    mkAdditionalAuthenticationProvider,

    -- * Lenses
    aapOpenIdConnectConfig,
    aapUserPoolConfig,
    aapAuthenticationType,
  )
where

import Network.AWS.AppSync.Types.AuthenticationType
import Network.AWS.AppSync.Types.CognitoUserPoolConfig
import Network.AWS.AppSync.Types.OpenIdConnectConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an additional authentication provider.
--
-- /See:/ 'mkAdditionalAuthenticationProvider' smart constructor.
data AdditionalAuthenticationProvider = AdditionalAuthenticationProvider'
  { openIdConnectConfig ::
      Lude.Maybe
        OpenIdConnectConfig,
    userPoolConfig ::
      Lude.Maybe
        CognitoUserPoolConfig,
    authenticationType ::
      Lude.Maybe
        AuthenticationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AdditionalAuthenticationProvider' with the minimum fields required to make a request.
--
-- * 'authenticationType' - The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
-- * 'openIdConnectConfig' - The OpenID Connect configuration.
-- * 'userPoolConfig' - The Amazon Cognito user pool configuration.
mkAdditionalAuthenticationProvider ::
  AdditionalAuthenticationProvider
mkAdditionalAuthenticationProvider =
  AdditionalAuthenticationProvider'
    { openIdConnectConfig =
        Lude.Nothing,
      userPoolConfig = Lude.Nothing,
      authenticationType = Lude.Nothing
    }

-- | The OpenID Connect configuration.
--
-- /Note:/ Consider using 'openIdConnectConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aapOpenIdConnectConfig :: Lens.Lens' AdditionalAuthenticationProvider (Lude.Maybe OpenIdConnectConfig)
aapOpenIdConnectConfig = Lens.lens (openIdConnectConfig :: AdditionalAuthenticationProvider -> Lude.Maybe OpenIdConnectConfig) (\s a -> s {openIdConnectConfig = a} :: AdditionalAuthenticationProvider)
{-# DEPRECATED aapOpenIdConnectConfig "Use generic-lens or generic-optics with 'openIdConnectConfig' instead." #-}

-- | The Amazon Cognito user pool configuration.
--
-- /Note:/ Consider using 'userPoolConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aapUserPoolConfig :: Lens.Lens' AdditionalAuthenticationProvider (Lude.Maybe CognitoUserPoolConfig)
aapUserPoolConfig = Lens.lens (userPoolConfig :: AdditionalAuthenticationProvider -> Lude.Maybe CognitoUserPoolConfig) (\s a -> s {userPoolConfig = a} :: AdditionalAuthenticationProvider)
{-# DEPRECATED aapUserPoolConfig "Use generic-lens or generic-optics with 'userPoolConfig' instead." #-}

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user pools.
--
-- /Note:/ Consider using 'authenticationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aapAuthenticationType :: Lens.Lens' AdditionalAuthenticationProvider (Lude.Maybe AuthenticationType)
aapAuthenticationType = Lens.lens (authenticationType :: AdditionalAuthenticationProvider -> Lude.Maybe AuthenticationType) (\s a -> s {authenticationType = a} :: AdditionalAuthenticationProvider)
{-# DEPRECATED aapAuthenticationType "Use generic-lens or generic-optics with 'authenticationType' instead." #-}

instance Lude.FromJSON AdditionalAuthenticationProvider where
  parseJSON =
    Lude.withObject
      "AdditionalAuthenticationProvider"
      ( \x ->
          AdditionalAuthenticationProvider'
            Lude.<$> (x Lude..:? "openIDConnectConfig")
            Lude.<*> (x Lude..:? "userPoolConfig")
            Lude.<*> (x Lude..:? "authenticationType")
      )

instance Lude.ToJSON AdditionalAuthenticationProvider where
  toJSON AdditionalAuthenticationProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("openIDConnectConfig" Lude..=) Lude.<$> openIdConnectConfig,
            ("userPoolConfig" Lude..=) Lude.<$> userPoolConfig,
            ("authenticationType" Lude..=) Lude.<$> authenticationType
          ]
      )
