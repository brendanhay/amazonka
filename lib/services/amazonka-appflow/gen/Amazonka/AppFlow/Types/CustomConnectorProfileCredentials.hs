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
-- Module      : Amazonka.AppFlow.Types.CustomConnectorProfileCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.CustomConnectorProfileCredentials where

import Amazonka.AppFlow.Types.ApiKeyCredentials
import Amazonka.AppFlow.Types.AuthenticationType
import Amazonka.AppFlow.Types.BasicAuthCredentials
import Amazonka.AppFlow.Types.CustomAuthCredentials
import Amazonka.AppFlow.Types.OAuth2Credentials
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The connector-specific profile credentials that are required when using
-- the custom connector.
--
-- /See:/ 'newCustomConnectorProfileCredentials' smart constructor.
data CustomConnectorProfileCredentials = CustomConnectorProfileCredentials'
  { -- | The API keys required for the authentication of the user.
    apiKey :: Prelude.Maybe ApiKeyCredentials,
    -- | The basic credentials that are required for the authentication of the
    -- user.
    basic :: Prelude.Maybe BasicAuthCredentials,
    -- | If the connector uses the custom authentication mechanism, this holds
    -- the required credentials.
    custom :: Prelude.Maybe CustomAuthCredentials,
    -- | The OAuth 2.0 credentials required for the authentication of the user.
    oauth2 :: Prelude.Maybe OAuth2Credentials,
    -- | The authentication type that the custom connector uses for
    -- authenticating while creating a connector profile.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomConnectorProfileCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiKey', 'customConnectorProfileCredentials_apiKey' - The API keys required for the authentication of the user.
--
-- 'basic', 'customConnectorProfileCredentials_basic' - The basic credentials that are required for the authentication of the
-- user.
--
-- 'custom', 'customConnectorProfileCredentials_custom' - If the connector uses the custom authentication mechanism, this holds
-- the required credentials.
--
-- 'oauth2', 'customConnectorProfileCredentials_oauth2' - The OAuth 2.0 credentials required for the authentication of the user.
--
-- 'authenticationType', 'customConnectorProfileCredentials_authenticationType' - The authentication type that the custom connector uses for
-- authenticating while creating a connector profile.
newCustomConnectorProfileCredentials ::
  -- | 'authenticationType'
  AuthenticationType ->
  CustomConnectorProfileCredentials
newCustomConnectorProfileCredentials
  pAuthenticationType_ =
    CustomConnectorProfileCredentials'
      { apiKey =
          Prelude.Nothing,
        basic = Prelude.Nothing,
        custom = Prelude.Nothing,
        oauth2 = Prelude.Nothing,
        authenticationType =
          pAuthenticationType_
      }

-- | The API keys required for the authentication of the user.
customConnectorProfileCredentials_apiKey :: Lens.Lens' CustomConnectorProfileCredentials (Prelude.Maybe ApiKeyCredentials)
customConnectorProfileCredentials_apiKey = Lens.lens (\CustomConnectorProfileCredentials' {apiKey} -> apiKey) (\s@CustomConnectorProfileCredentials' {} a -> s {apiKey = a} :: CustomConnectorProfileCredentials)

-- | The basic credentials that are required for the authentication of the
-- user.
customConnectorProfileCredentials_basic :: Lens.Lens' CustomConnectorProfileCredentials (Prelude.Maybe BasicAuthCredentials)
customConnectorProfileCredentials_basic = Lens.lens (\CustomConnectorProfileCredentials' {basic} -> basic) (\s@CustomConnectorProfileCredentials' {} a -> s {basic = a} :: CustomConnectorProfileCredentials)

-- | If the connector uses the custom authentication mechanism, this holds
-- the required credentials.
customConnectorProfileCredentials_custom :: Lens.Lens' CustomConnectorProfileCredentials (Prelude.Maybe CustomAuthCredentials)
customConnectorProfileCredentials_custom = Lens.lens (\CustomConnectorProfileCredentials' {custom} -> custom) (\s@CustomConnectorProfileCredentials' {} a -> s {custom = a} :: CustomConnectorProfileCredentials)

-- | The OAuth 2.0 credentials required for the authentication of the user.
customConnectorProfileCredentials_oauth2 :: Lens.Lens' CustomConnectorProfileCredentials (Prelude.Maybe OAuth2Credentials)
customConnectorProfileCredentials_oauth2 = Lens.lens (\CustomConnectorProfileCredentials' {oauth2} -> oauth2) (\s@CustomConnectorProfileCredentials' {} a -> s {oauth2 = a} :: CustomConnectorProfileCredentials)

-- | The authentication type that the custom connector uses for
-- authenticating while creating a connector profile.
customConnectorProfileCredentials_authenticationType :: Lens.Lens' CustomConnectorProfileCredentials AuthenticationType
customConnectorProfileCredentials_authenticationType = Lens.lens (\CustomConnectorProfileCredentials' {authenticationType} -> authenticationType) (\s@CustomConnectorProfileCredentials' {} a -> s {authenticationType = a} :: CustomConnectorProfileCredentials)

instance
  Prelude.Hashable
    CustomConnectorProfileCredentials
  where
  hashWithSalt
    _salt
    CustomConnectorProfileCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` apiKey
        `Prelude.hashWithSalt` basic
        `Prelude.hashWithSalt` custom
        `Prelude.hashWithSalt` oauth2
        `Prelude.hashWithSalt` authenticationType

instance
  Prelude.NFData
    CustomConnectorProfileCredentials
  where
  rnf CustomConnectorProfileCredentials' {..} =
    Prelude.rnf apiKey
      `Prelude.seq` Prelude.rnf basic
      `Prelude.seq` Prelude.rnf custom
      `Prelude.seq` Prelude.rnf oauth2
      `Prelude.seq` Prelude.rnf authenticationType

instance
  Data.ToJSON
    CustomConnectorProfileCredentials
  where
  toJSON CustomConnectorProfileCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiKey" Data..=) Prelude.<$> apiKey,
            ("basic" Data..=) Prelude.<$> basic,
            ("custom" Data..=) Prelude.<$> custom,
            ("oauth2" Data..=) Prelude.<$> oauth2,
            Prelude.Just
              ("authenticationType" Data..= authenticationType)
          ]
      )
