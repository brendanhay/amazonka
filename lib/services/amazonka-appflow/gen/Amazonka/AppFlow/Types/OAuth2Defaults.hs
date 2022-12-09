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
-- Module      : Amazonka.AppFlow.Types.OAuth2Defaults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.OAuth2Defaults where

import Amazonka.AppFlow.Types.OAuth2CustomParameter
import Amazonka.AppFlow.Types.OAuth2GrantType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the default values required for OAuth 2.0 authentication.
--
-- /See:/ 'newOAuth2Defaults' smart constructor.
data OAuth2Defaults = OAuth2Defaults'
  { -- | Auth code URLs that can be used for OAuth 2.0 authentication.
    authCodeUrls :: Prelude.Maybe [Prelude.Text],
    -- | List of custom parameters required for OAuth 2.0 authentication.
    oauth2CustomProperties :: Prelude.Maybe [OAuth2CustomParameter],
    -- | OAuth 2.0 grant types supported by the connector.
    oauth2GrantTypesSupported :: Prelude.Maybe [OAuth2GrantType],
    -- | OAuth 2.0 scopes that the connector supports.
    oauthScopes :: Prelude.Maybe [Prelude.Text],
    -- | Token URLs that can be used for OAuth 2.0 authentication.
    tokenUrls :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OAuth2Defaults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authCodeUrls', 'oAuth2Defaults_authCodeUrls' - Auth code URLs that can be used for OAuth 2.0 authentication.
--
-- 'oauth2CustomProperties', 'oAuth2Defaults_oauth2CustomProperties' - List of custom parameters required for OAuth 2.0 authentication.
--
-- 'oauth2GrantTypesSupported', 'oAuth2Defaults_oauth2GrantTypesSupported' - OAuth 2.0 grant types supported by the connector.
--
-- 'oauthScopes', 'oAuth2Defaults_oauthScopes' - OAuth 2.0 scopes that the connector supports.
--
-- 'tokenUrls', 'oAuth2Defaults_tokenUrls' - Token URLs that can be used for OAuth 2.0 authentication.
newOAuth2Defaults ::
  OAuth2Defaults
newOAuth2Defaults =
  OAuth2Defaults'
    { authCodeUrls = Prelude.Nothing,
      oauth2CustomProperties = Prelude.Nothing,
      oauth2GrantTypesSupported = Prelude.Nothing,
      oauthScopes = Prelude.Nothing,
      tokenUrls = Prelude.Nothing
    }

-- | Auth code URLs that can be used for OAuth 2.0 authentication.
oAuth2Defaults_authCodeUrls :: Lens.Lens' OAuth2Defaults (Prelude.Maybe [Prelude.Text])
oAuth2Defaults_authCodeUrls = Lens.lens (\OAuth2Defaults' {authCodeUrls} -> authCodeUrls) (\s@OAuth2Defaults' {} a -> s {authCodeUrls = a} :: OAuth2Defaults) Prelude.. Lens.mapping Lens.coerced

-- | List of custom parameters required for OAuth 2.0 authentication.
oAuth2Defaults_oauth2CustomProperties :: Lens.Lens' OAuth2Defaults (Prelude.Maybe [OAuth2CustomParameter])
oAuth2Defaults_oauth2CustomProperties = Lens.lens (\OAuth2Defaults' {oauth2CustomProperties} -> oauth2CustomProperties) (\s@OAuth2Defaults' {} a -> s {oauth2CustomProperties = a} :: OAuth2Defaults) Prelude.. Lens.mapping Lens.coerced

-- | OAuth 2.0 grant types supported by the connector.
oAuth2Defaults_oauth2GrantTypesSupported :: Lens.Lens' OAuth2Defaults (Prelude.Maybe [OAuth2GrantType])
oAuth2Defaults_oauth2GrantTypesSupported = Lens.lens (\OAuth2Defaults' {oauth2GrantTypesSupported} -> oauth2GrantTypesSupported) (\s@OAuth2Defaults' {} a -> s {oauth2GrantTypesSupported = a} :: OAuth2Defaults) Prelude.. Lens.mapping Lens.coerced

-- | OAuth 2.0 scopes that the connector supports.
oAuth2Defaults_oauthScopes :: Lens.Lens' OAuth2Defaults (Prelude.Maybe [Prelude.Text])
oAuth2Defaults_oauthScopes = Lens.lens (\OAuth2Defaults' {oauthScopes} -> oauthScopes) (\s@OAuth2Defaults' {} a -> s {oauthScopes = a} :: OAuth2Defaults) Prelude.. Lens.mapping Lens.coerced

-- | Token URLs that can be used for OAuth 2.0 authentication.
oAuth2Defaults_tokenUrls :: Lens.Lens' OAuth2Defaults (Prelude.Maybe [Prelude.Text])
oAuth2Defaults_tokenUrls = Lens.lens (\OAuth2Defaults' {tokenUrls} -> tokenUrls) (\s@OAuth2Defaults' {} a -> s {tokenUrls = a} :: OAuth2Defaults) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON OAuth2Defaults where
  parseJSON =
    Data.withObject
      "OAuth2Defaults"
      ( \x ->
          OAuth2Defaults'
            Prelude.<$> (x Data..:? "authCodeUrls" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "oauth2CustomProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x Data..:? "oauth2GrantTypesSupported"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "oauthScopes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tokenUrls" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable OAuth2Defaults where
  hashWithSalt _salt OAuth2Defaults' {..} =
    _salt `Prelude.hashWithSalt` authCodeUrls
      `Prelude.hashWithSalt` oauth2CustomProperties
      `Prelude.hashWithSalt` oauth2GrantTypesSupported
      `Prelude.hashWithSalt` oauthScopes
      `Prelude.hashWithSalt` tokenUrls

instance Prelude.NFData OAuth2Defaults where
  rnf OAuth2Defaults' {..} =
    Prelude.rnf authCodeUrls
      `Prelude.seq` Prelude.rnf oauth2CustomProperties
      `Prelude.seq` Prelude.rnf oauth2GrantTypesSupported
      `Prelude.seq` Prelude.rnf oauthScopes
      `Prelude.seq` Prelude.rnf tokenUrls
