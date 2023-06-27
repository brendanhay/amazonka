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
-- Module      : Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiOpenIdConnectConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the authorization configuration for using an OpenID Connect
-- compliant service with your AppSync GraphQL API endpoint.
--
-- /See:/ 'newAwsAppSyncGraphQlApiOpenIdConnectConfigDetails' smart constructor.
data AwsAppSyncGraphQlApiOpenIdConnectConfigDetails = AwsAppSyncGraphQlApiOpenIdConnectConfigDetails'
  { -- | The number of milliseconds that a token is valid after being
    -- authenticated.
    authTtL :: Prelude.Maybe Prelude.Integer,
    -- | The client identifier of the relying party at the OpenID identity
    -- provider. This identifier is typically obtained when the relying party
    -- is registered with the OpenID identity provider. You can specify a
    -- regular expression so that AppSync can validate against multiple client
    -- identifiers at a time.
    clientId :: Prelude.Maybe Prelude.Text,
    -- | The number of milliseconds that a token is valid after it\'s issued to a
    -- user.
    iatTtL :: Prelude.Maybe Prelude.Integer,
    -- | The issuer for the OIDC configuration. The issuer returned by discovery
    -- must exactly match the value of @iss@ in the ID token.
    issuer :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authTtL', 'awsAppSyncGraphQlApiOpenIdConnectConfigDetails_authTtL' - The number of milliseconds that a token is valid after being
-- authenticated.
--
-- 'clientId', 'awsAppSyncGraphQlApiOpenIdConnectConfigDetails_clientId' - The client identifier of the relying party at the OpenID identity
-- provider. This identifier is typically obtained when the relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so that AppSync can validate against multiple client
-- identifiers at a time.
--
-- 'iatTtL', 'awsAppSyncGraphQlApiOpenIdConnectConfigDetails_iatTtL' - The number of milliseconds that a token is valid after it\'s issued to a
-- user.
--
-- 'issuer', 'awsAppSyncGraphQlApiOpenIdConnectConfigDetails_issuer' - The issuer for the OIDC configuration. The issuer returned by discovery
-- must exactly match the value of @iss@ in the ID token.
newAwsAppSyncGraphQlApiOpenIdConnectConfigDetails ::
  AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
newAwsAppSyncGraphQlApiOpenIdConnectConfigDetails =
  AwsAppSyncGraphQlApiOpenIdConnectConfigDetails'
    { authTtL =
        Prelude.Nothing,
      clientId = Prelude.Nothing,
      iatTtL = Prelude.Nothing,
      issuer = Prelude.Nothing
    }

-- | The number of milliseconds that a token is valid after being
-- authenticated.
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_authTtL :: Lens.Lens' AwsAppSyncGraphQlApiOpenIdConnectConfigDetails (Prelude.Maybe Prelude.Integer)
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_authTtL = Lens.lens (\AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {authTtL} -> authTtL) (\s@AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {} a -> s {authTtL = a} :: AwsAppSyncGraphQlApiOpenIdConnectConfigDetails)

-- | The client identifier of the relying party at the OpenID identity
-- provider. This identifier is typically obtained when the relying party
-- is registered with the OpenID identity provider. You can specify a
-- regular expression so that AppSync can validate against multiple client
-- identifiers at a time.
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_clientId :: Lens.Lens' AwsAppSyncGraphQlApiOpenIdConnectConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_clientId = Lens.lens (\AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {clientId} -> clientId) (\s@AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {} a -> s {clientId = a} :: AwsAppSyncGraphQlApiOpenIdConnectConfigDetails)

-- | The number of milliseconds that a token is valid after it\'s issued to a
-- user.
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_iatTtL :: Lens.Lens' AwsAppSyncGraphQlApiOpenIdConnectConfigDetails (Prelude.Maybe Prelude.Integer)
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_iatTtL = Lens.lens (\AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {iatTtL} -> iatTtL) (\s@AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {} a -> s {iatTtL = a} :: AwsAppSyncGraphQlApiOpenIdConnectConfigDetails)

-- | The issuer for the OIDC configuration. The issuer returned by discovery
-- must exactly match the value of @iss@ in the ID token.
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_issuer :: Lens.Lens' AwsAppSyncGraphQlApiOpenIdConnectConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiOpenIdConnectConfigDetails_issuer = Lens.lens (\AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {issuer} -> issuer) (\s@AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {} a -> s {issuer = a} :: AwsAppSyncGraphQlApiOpenIdConnectConfigDetails)

instance
  Data.FromJSON
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsAppSyncGraphQlApiOpenIdConnectConfigDetails"
      ( \x ->
          AwsAppSyncGraphQlApiOpenIdConnectConfigDetails'
            Prelude.<$> (x Data..:? "AuthTtL")
            Prelude.<*> (x Data..:? "ClientId")
            Prelude.<*> (x Data..:? "IatTtL")
            Prelude.<*> (x Data..:? "Issuer")
      )

instance
  Prelude.Hashable
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
  where
  hashWithSalt
    _salt
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {..} =
      _salt
        `Prelude.hashWithSalt` authTtL
        `Prelude.hashWithSalt` clientId
        `Prelude.hashWithSalt` iatTtL
        `Prelude.hashWithSalt` issuer

instance
  Prelude.NFData
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
  where
  rnf
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {..} =
      Prelude.rnf authTtL
        `Prelude.seq` Prelude.rnf clientId
        `Prelude.seq` Prelude.rnf iatTtL
        `Prelude.seq` Prelude.rnf issuer

instance
  Data.ToJSON
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
  where
  toJSON
    AwsAppSyncGraphQlApiOpenIdConnectConfigDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AuthTtL" Data..=) Prelude.<$> authTtL,
              ("ClientId" Data..=) Prelude.<$> clientId,
              ("IatTtL" Data..=) Prelude.<$> iatTtL,
              ("Issuer" Data..=) Prelude.<$> issuer
            ]
        )
