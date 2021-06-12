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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes an OpenID Connect configuration.
--
-- /See:/ 'newOpenIDConnectConfig' smart constructor.
data OpenIDConnectConfig = OpenIDConnectConfig'
  { -- | The client identifier of the Relying party at the OpenID identity
    -- provider. This identifier is typically obtained when the Relying party
    -- is registered with the OpenID identity provider. You can specify a
    -- regular expression so the AWS AppSync can validate against multiple
    -- client identifiers at a time.
    clientId :: Core.Maybe Core.Text,
    -- | The number of milliseconds a token is valid after being authenticated.
    authTTL :: Core.Maybe Core.Integer,
    -- | The number of milliseconds a token is valid after being issued to a
    -- user.
    iatTTL :: Core.Maybe Core.Integer,
    -- | The issuer for the OpenID Connect configuration. The issuer returned by
    -- discovery must exactly match the value of @iss@ in the ID token.
    issuer :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  OpenIDConnectConfig
newOpenIDConnectConfig pIssuer_ =
  OpenIDConnectConfig'
    { clientId = Core.Nothing,
      authTTL = Core.Nothing,
      iatTTL = Core.Nothing,
      issuer = pIssuer_
    }

-- | The client identifier of the Relying party at the OpenID identity
-- provider. This identifier is typically obtained when the Relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so the AWS AppSync can validate against multiple
-- client identifiers at a time.
openIDConnectConfig_clientId :: Lens.Lens' OpenIDConnectConfig (Core.Maybe Core.Text)
openIDConnectConfig_clientId = Lens.lens (\OpenIDConnectConfig' {clientId} -> clientId) (\s@OpenIDConnectConfig' {} a -> s {clientId = a} :: OpenIDConnectConfig)

-- | The number of milliseconds a token is valid after being authenticated.
openIDConnectConfig_authTTL :: Lens.Lens' OpenIDConnectConfig (Core.Maybe Core.Integer)
openIDConnectConfig_authTTL = Lens.lens (\OpenIDConnectConfig' {authTTL} -> authTTL) (\s@OpenIDConnectConfig' {} a -> s {authTTL = a} :: OpenIDConnectConfig)

-- | The number of milliseconds a token is valid after being issued to a
-- user.
openIDConnectConfig_iatTTL :: Lens.Lens' OpenIDConnectConfig (Core.Maybe Core.Integer)
openIDConnectConfig_iatTTL = Lens.lens (\OpenIDConnectConfig' {iatTTL} -> iatTTL) (\s@OpenIDConnectConfig' {} a -> s {iatTTL = a} :: OpenIDConnectConfig)

-- | The issuer for the OpenID Connect configuration. The issuer returned by
-- discovery must exactly match the value of @iss@ in the ID token.
openIDConnectConfig_issuer :: Lens.Lens' OpenIDConnectConfig Core.Text
openIDConnectConfig_issuer = Lens.lens (\OpenIDConnectConfig' {issuer} -> issuer) (\s@OpenIDConnectConfig' {} a -> s {issuer = a} :: OpenIDConnectConfig)

instance Core.FromJSON OpenIDConnectConfig where
  parseJSON =
    Core.withObject
      "OpenIDConnectConfig"
      ( \x ->
          OpenIDConnectConfig'
            Core.<$> (x Core..:? "clientId")
            Core.<*> (x Core..:? "authTTL")
            Core.<*> (x Core..:? "iatTTL")
            Core.<*> (x Core..: "issuer")
      )

instance Core.Hashable OpenIDConnectConfig

instance Core.NFData OpenIDConnectConfig

instance Core.ToJSON OpenIDConnectConfig where
  toJSON OpenIDConnectConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("clientId" Core..=) Core.<$> clientId,
            ("authTTL" Core..=) Core.<$> authTTL,
            ("iatTTL" Core..=) Core.<$> iatTTL,
            Core.Just ("issuer" Core..= issuer)
          ]
      )
