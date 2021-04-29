{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.OpenIDConnectConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.OpenIDConnectConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an OpenID Connect configuration.
--
-- /See:/ 'newOpenIDConnectConfig' smart constructor.
data OpenIDConnectConfig = OpenIDConnectConfig'
  { -- | The client identifier of the Relying party at the OpenID identity
    -- provider. This identifier is typically obtained when the Relying party
    -- is registered with the OpenID identity provider. You can specify a
    -- regular expression so the AWS AppSync can validate against multiple
    -- client identifiers at a time.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The number of milliseconds a token is valid after being authenticated.
    authTTL :: Prelude.Maybe Prelude.Integer,
    -- | The number of milliseconds a token is valid after being issued to a
    -- user.
    iatTTL :: Prelude.Maybe Prelude.Integer,
    -- | The issuer for the OpenID Connect configuration. The issuer returned by
    -- discovery must exactly match the value of @iss@ in the ID token.
    issuer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OpenIDConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'openIDConnectConfig_clientId' - The client identifier of the Relying party at the OpenID identity
-- provider. This identifier is typically obtained when the Relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so the AWS AppSync can validate against multiple
-- client identifiers at a time.
--
-- 'authTTL', 'openIDConnectConfig_authTTL' - The number of milliseconds a token is valid after being authenticated.
--
-- 'iatTTL', 'openIDConnectConfig_iatTTL' - The number of milliseconds a token is valid after being issued to a
-- user.
--
-- 'issuer', 'openIDConnectConfig_issuer' - The issuer for the OpenID Connect configuration. The issuer returned by
-- discovery must exactly match the value of @iss@ in the ID token.
newOpenIDConnectConfig ::
  -- | 'issuer'
  Prelude.Text ->
  OpenIDConnectConfig
newOpenIDConnectConfig pIssuer_ =
  OpenIDConnectConfig'
    { clientId = Prelude.Nothing,
      authTTL = Prelude.Nothing,
      iatTTL = Prelude.Nothing,
      issuer = pIssuer_
    }

-- | The client identifier of the Relying party at the OpenID identity
-- provider. This identifier is typically obtained when the Relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so the AWS AppSync can validate against multiple
-- client identifiers at a time.
openIDConnectConfig_clientId :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Text)
openIDConnectConfig_clientId = Lens.lens (\OpenIDConnectConfig' {clientId} -> clientId) (\s@OpenIDConnectConfig' {} a -> s {clientId = a} :: OpenIDConnectConfig)

-- | The number of milliseconds a token is valid after being authenticated.
openIDConnectConfig_authTTL :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Integer)
openIDConnectConfig_authTTL = Lens.lens (\OpenIDConnectConfig' {authTTL} -> authTTL) (\s@OpenIDConnectConfig' {} a -> s {authTTL = a} :: OpenIDConnectConfig)

-- | The number of milliseconds a token is valid after being issued to a
-- user.
openIDConnectConfig_iatTTL :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Integer)
openIDConnectConfig_iatTTL = Lens.lens (\OpenIDConnectConfig' {iatTTL} -> iatTTL) (\s@OpenIDConnectConfig' {} a -> s {iatTTL = a} :: OpenIDConnectConfig)

-- | The issuer for the OpenID Connect configuration. The issuer returned by
-- discovery must exactly match the value of @iss@ in the ID token.
openIDConnectConfig_issuer :: Lens.Lens' OpenIDConnectConfig Prelude.Text
openIDConnectConfig_issuer = Lens.lens (\OpenIDConnectConfig' {issuer} -> issuer) (\s@OpenIDConnectConfig' {} a -> s {issuer = a} :: OpenIDConnectConfig)

instance Prelude.FromJSON OpenIDConnectConfig where
  parseJSON =
    Prelude.withObject
      "OpenIDConnectConfig"
      ( \x ->
          OpenIDConnectConfig'
            Prelude.<$> (x Prelude..:? "clientId")
            Prelude.<*> (x Prelude..:? "authTTL")
            Prelude.<*> (x Prelude..:? "iatTTL")
            Prelude.<*> (x Prelude..: "issuer")
      )

instance Prelude.Hashable OpenIDConnectConfig

instance Prelude.NFData OpenIDConnectConfig

instance Prelude.ToJSON OpenIDConnectConfig where
  toJSON OpenIDConnectConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("clientId" Prelude..=) Prelude.<$> clientId,
            ("authTTL" Prelude..=) Prelude.<$> authTTL,
            ("iatTTL" Prelude..=) Prelude.<$> iatTTL,
            Prelude.Just ("issuer" Prelude..= issuer)
          ]
      )
