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
-- Module      : Amazonka.AppSync.Types.OpenIDConnectConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.OpenIDConnectConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an OpenID Connect (OIDC) configuration.
--
-- /See:/ 'newOpenIDConnectConfig' smart constructor.
data OpenIDConnectConfig = OpenIDConnectConfig'
  { -- | The number of milliseconds that a token is valid after it\'s issued to a
    -- user.
    iatTTL :: Prelude.Maybe Prelude.Integer,
    -- | The client identifier of the relying party at the OpenID identity
    -- provider. This identifier is typically obtained when the relying party
    -- is registered with the OpenID identity provider. You can specify a
    -- regular expression so that AppSync can validate against multiple client
    -- identifiers at a time.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The number of milliseconds that a token is valid after being
    -- authenticated.
    authTTL :: Prelude.Maybe Prelude.Integer,
    -- | The issuer for the OIDC configuration. The issuer returned by discovery
    -- must exactly match the value of @iss@ in the ID token.
    issuer :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenIDConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iatTTL', 'openIDConnectConfig_iatTTL' - The number of milliseconds that a token is valid after it\'s issued to a
-- user.
--
-- 'clientId', 'openIDConnectConfig_clientId' - The client identifier of the relying party at the OpenID identity
-- provider. This identifier is typically obtained when the relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so that AppSync can validate against multiple client
-- identifiers at a time.
--
-- 'authTTL', 'openIDConnectConfig_authTTL' - The number of milliseconds that a token is valid after being
-- authenticated.
--
-- 'issuer', 'openIDConnectConfig_issuer' - The issuer for the OIDC configuration. The issuer returned by discovery
-- must exactly match the value of @iss@ in the ID token.
newOpenIDConnectConfig ::
  -- | 'issuer'
  Prelude.Text ->
  OpenIDConnectConfig
newOpenIDConnectConfig pIssuer_ =
  OpenIDConnectConfig'
    { iatTTL = Prelude.Nothing,
      clientId = Prelude.Nothing,
      authTTL = Prelude.Nothing,
      issuer = pIssuer_
    }

-- | The number of milliseconds that a token is valid after it\'s issued to a
-- user.
openIDConnectConfig_iatTTL :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Integer)
openIDConnectConfig_iatTTL = Lens.lens (\OpenIDConnectConfig' {iatTTL} -> iatTTL) (\s@OpenIDConnectConfig' {} a -> s {iatTTL = a} :: OpenIDConnectConfig)

-- | The client identifier of the relying party at the OpenID identity
-- provider. This identifier is typically obtained when the relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so that AppSync can validate against multiple client
-- identifiers at a time.
openIDConnectConfig_clientId :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Text)
openIDConnectConfig_clientId = Lens.lens (\OpenIDConnectConfig' {clientId} -> clientId) (\s@OpenIDConnectConfig' {} a -> s {clientId = a} :: OpenIDConnectConfig)

-- | The number of milliseconds that a token is valid after being
-- authenticated.
openIDConnectConfig_authTTL :: Lens.Lens' OpenIDConnectConfig (Prelude.Maybe Prelude.Integer)
openIDConnectConfig_authTTL = Lens.lens (\OpenIDConnectConfig' {authTTL} -> authTTL) (\s@OpenIDConnectConfig' {} a -> s {authTTL = a} :: OpenIDConnectConfig)

-- | The issuer for the OIDC configuration. The issuer returned by discovery
-- must exactly match the value of @iss@ in the ID token.
openIDConnectConfig_issuer :: Lens.Lens' OpenIDConnectConfig Prelude.Text
openIDConnectConfig_issuer = Lens.lens (\OpenIDConnectConfig' {issuer} -> issuer) (\s@OpenIDConnectConfig' {} a -> s {issuer = a} :: OpenIDConnectConfig)

instance Data.FromJSON OpenIDConnectConfig where
  parseJSON =
    Data.withObject
      "OpenIDConnectConfig"
      ( \x ->
          OpenIDConnectConfig'
            Prelude.<$> (x Data..:? "iatTTL")
            Prelude.<*> (x Data..:? "clientId")
            Prelude.<*> (x Data..:? "authTTL")
            Prelude.<*> (x Data..: "issuer")
      )

instance Prelude.Hashable OpenIDConnectConfig where
  hashWithSalt _salt OpenIDConnectConfig' {..} =
    _salt `Prelude.hashWithSalt` iatTTL
      `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` authTTL
      `Prelude.hashWithSalt` issuer

instance Prelude.NFData OpenIDConnectConfig where
  rnf OpenIDConnectConfig' {..} =
    Prelude.rnf iatTTL
      `Prelude.seq` Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf authTTL
      `Prelude.seq` Prelude.rnf issuer

instance Data.ToJSON OpenIDConnectConfig where
  toJSON OpenIDConnectConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("iatTTL" Data..=) Prelude.<$> iatTTL,
            ("clientId" Data..=) Prelude.<$> clientId,
            ("authTTL" Data..=) Prelude.<$> authTTL,
            Prelude.Just ("issuer" Data..= issuer)
          ]
      )
