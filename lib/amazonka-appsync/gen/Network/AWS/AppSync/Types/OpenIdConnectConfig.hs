{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.OpenIdConnectConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.OpenIdConnectConfig
  ( OpenIdConnectConfig (..),

    -- * Smart constructor
    mkOpenIdConnectConfig,

    -- * Lenses
    oiccAuthTTL,
    oiccClientId,
    oiccIatTTL,
    oiccIssuer,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an OpenID Connect configuration.
--
-- /See:/ 'mkOpenIdConnectConfig' smart constructor.
data OpenIdConnectConfig = OpenIdConnectConfig'
  { authTTL ::
      Lude.Maybe Lude.Integer,
    clientId :: Lude.Maybe Lude.Text,
    iatTTL :: Lude.Maybe Lude.Integer,
    issuer :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpenIdConnectConfig' with the minimum fields required to make a request.
--
-- * 'authTTL' - The number of milliseconds a token is valid after being authenticated.
-- * 'clientId' - The client identifier of the Relying party at the OpenID identity provider. This identifier is typically obtained when the Relying party is registered with the OpenID identity provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time.
-- * 'iatTTL' - The number of milliseconds a token is valid after being issued to a user.
-- * 'issuer' - The issuer for the OpenID Connect configuration. The issuer returned by discovery must exactly match the value of @iss@ in the ID token.
mkOpenIdConnectConfig ::
  -- | 'issuer'
  Lude.Text ->
  OpenIdConnectConfig
mkOpenIdConnectConfig pIssuer_ =
  OpenIdConnectConfig'
    { authTTL = Lude.Nothing,
      clientId = Lude.Nothing,
      iatTTL = Lude.Nothing,
      issuer = pIssuer_
    }

-- | The number of milliseconds a token is valid after being authenticated.
--
-- /Note:/ Consider using 'authTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiccAuthTTL :: Lens.Lens' OpenIdConnectConfig (Lude.Maybe Lude.Integer)
oiccAuthTTL = Lens.lens (authTTL :: OpenIdConnectConfig -> Lude.Maybe Lude.Integer) (\s a -> s {authTTL = a} :: OpenIdConnectConfig)
{-# DEPRECATED oiccAuthTTL "Use generic-lens or generic-optics with 'authTTL' instead." #-}

-- | The client identifier of the Relying party at the OpenID identity provider. This identifier is typically obtained when the Relying party is registered with the OpenID identity provider. You can specify a regular expression so the AWS AppSync can validate against multiple client identifiers at a time.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiccClientId :: Lens.Lens' OpenIdConnectConfig (Lude.Maybe Lude.Text)
oiccClientId = Lens.lens (clientId :: OpenIdConnectConfig -> Lude.Maybe Lude.Text) (\s a -> s {clientId = a} :: OpenIdConnectConfig)
{-# DEPRECATED oiccClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The number of milliseconds a token is valid after being issued to a user.
--
-- /Note:/ Consider using 'iatTTL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiccIatTTL :: Lens.Lens' OpenIdConnectConfig (Lude.Maybe Lude.Integer)
oiccIatTTL = Lens.lens (iatTTL :: OpenIdConnectConfig -> Lude.Maybe Lude.Integer) (\s a -> s {iatTTL = a} :: OpenIdConnectConfig)
{-# DEPRECATED oiccIatTTL "Use generic-lens or generic-optics with 'iatTTL' instead." #-}

-- | The issuer for the OpenID Connect configuration. The issuer returned by discovery must exactly match the value of @iss@ in the ID token.
--
-- /Note:/ Consider using 'issuer' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oiccIssuer :: Lens.Lens' OpenIdConnectConfig Lude.Text
oiccIssuer = Lens.lens (issuer :: OpenIdConnectConfig -> Lude.Text) (\s a -> s {issuer = a} :: OpenIdConnectConfig)
{-# DEPRECATED oiccIssuer "Use generic-lens or generic-optics with 'issuer' instead." #-}

instance Lude.FromJSON OpenIdConnectConfig where
  parseJSON =
    Lude.withObject
      "OpenIdConnectConfig"
      ( \x ->
          OpenIdConnectConfig'
            Lude.<$> (x Lude..:? "authTTL")
            Lude.<*> (x Lude..:? "clientId")
            Lude.<*> (x Lude..:? "iatTTL")
            Lude.<*> (x Lude..: "issuer")
      )

instance Lude.ToJSON OpenIdConnectConfig where
  toJSON OpenIdConnectConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("authTTL" Lude..=) Lude.<$> authTTL,
            ("clientId" Lude..=) Lude.<$> clientId,
            ("iatTTL" Lude..=) Lude.<$> iatTTL,
            Lude.Just ("issuer" Lude..= issuer)
          ]
      )
