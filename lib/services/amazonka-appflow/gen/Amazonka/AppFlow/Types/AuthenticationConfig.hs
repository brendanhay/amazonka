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
-- Module      : Amazonka.AppFlow.Types.AuthenticationConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.AuthenticationConfig where

import Amazonka.AppFlow.Types.CustomAuthConfig
import Amazonka.AppFlow.Types.OAuth2Defaults
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the authentication config that the connector
-- supports.
--
-- /See:/ 'newAuthenticationConfig' smart constructor.
data AuthenticationConfig = AuthenticationConfig'
  { -- | Contains information required for custom authentication.
    customAuthConfigs :: Prelude.Maybe [CustomAuthConfig],
    -- | Indicates whether API key authentication is supported by the connector
    isApiKeyAuthSupported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether basic authentication is supported by the connector.
    isBasicAuthSupported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether custom authentication is supported by the connector
    isCustomAuthSupported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether OAuth 2.0 authentication is supported by the
    -- connector.
    isOAuth2Supported :: Prelude.Maybe Prelude.Bool,
    -- | Contains the default values required for OAuth 2.0 authentication.
    oAuth2Defaults :: Prelude.Maybe OAuth2Defaults
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthenticationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAuthConfigs', 'authenticationConfig_customAuthConfigs' - Contains information required for custom authentication.
--
-- 'isApiKeyAuthSupported', 'authenticationConfig_isApiKeyAuthSupported' - Indicates whether API key authentication is supported by the connector
--
-- 'isBasicAuthSupported', 'authenticationConfig_isBasicAuthSupported' - Indicates whether basic authentication is supported by the connector.
--
-- 'isCustomAuthSupported', 'authenticationConfig_isCustomAuthSupported' - Indicates whether custom authentication is supported by the connector
--
-- 'isOAuth2Supported', 'authenticationConfig_isOAuth2Supported' - Indicates whether OAuth 2.0 authentication is supported by the
-- connector.
--
-- 'oAuth2Defaults', 'authenticationConfig_oAuth2Defaults' - Contains the default values required for OAuth 2.0 authentication.
newAuthenticationConfig ::
  AuthenticationConfig
newAuthenticationConfig =
  AuthenticationConfig'
    { customAuthConfigs =
        Prelude.Nothing,
      isApiKeyAuthSupported = Prelude.Nothing,
      isBasicAuthSupported = Prelude.Nothing,
      isCustomAuthSupported = Prelude.Nothing,
      isOAuth2Supported = Prelude.Nothing,
      oAuth2Defaults = Prelude.Nothing
    }

-- | Contains information required for custom authentication.
authenticationConfig_customAuthConfigs :: Lens.Lens' AuthenticationConfig (Prelude.Maybe [CustomAuthConfig])
authenticationConfig_customAuthConfigs = Lens.lens (\AuthenticationConfig' {customAuthConfigs} -> customAuthConfigs) (\s@AuthenticationConfig' {} a -> s {customAuthConfigs = a} :: AuthenticationConfig) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether API key authentication is supported by the connector
authenticationConfig_isApiKeyAuthSupported :: Lens.Lens' AuthenticationConfig (Prelude.Maybe Prelude.Bool)
authenticationConfig_isApiKeyAuthSupported = Lens.lens (\AuthenticationConfig' {isApiKeyAuthSupported} -> isApiKeyAuthSupported) (\s@AuthenticationConfig' {} a -> s {isApiKeyAuthSupported = a} :: AuthenticationConfig)

-- | Indicates whether basic authentication is supported by the connector.
authenticationConfig_isBasicAuthSupported :: Lens.Lens' AuthenticationConfig (Prelude.Maybe Prelude.Bool)
authenticationConfig_isBasicAuthSupported = Lens.lens (\AuthenticationConfig' {isBasicAuthSupported} -> isBasicAuthSupported) (\s@AuthenticationConfig' {} a -> s {isBasicAuthSupported = a} :: AuthenticationConfig)

-- | Indicates whether custom authentication is supported by the connector
authenticationConfig_isCustomAuthSupported :: Lens.Lens' AuthenticationConfig (Prelude.Maybe Prelude.Bool)
authenticationConfig_isCustomAuthSupported = Lens.lens (\AuthenticationConfig' {isCustomAuthSupported} -> isCustomAuthSupported) (\s@AuthenticationConfig' {} a -> s {isCustomAuthSupported = a} :: AuthenticationConfig)

-- | Indicates whether OAuth 2.0 authentication is supported by the
-- connector.
authenticationConfig_isOAuth2Supported :: Lens.Lens' AuthenticationConfig (Prelude.Maybe Prelude.Bool)
authenticationConfig_isOAuth2Supported = Lens.lens (\AuthenticationConfig' {isOAuth2Supported} -> isOAuth2Supported) (\s@AuthenticationConfig' {} a -> s {isOAuth2Supported = a} :: AuthenticationConfig)

-- | Contains the default values required for OAuth 2.0 authentication.
authenticationConfig_oAuth2Defaults :: Lens.Lens' AuthenticationConfig (Prelude.Maybe OAuth2Defaults)
authenticationConfig_oAuth2Defaults = Lens.lens (\AuthenticationConfig' {oAuth2Defaults} -> oAuth2Defaults) (\s@AuthenticationConfig' {} a -> s {oAuth2Defaults = a} :: AuthenticationConfig)

instance Data.FromJSON AuthenticationConfig where
  parseJSON =
    Data.withObject
      "AuthenticationConfig"
      ( \x ->
          AuthenticationConfig'
            Prelude.<$> ( x
                            Data..:? "customAuthConfigs"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "isApiKeyAuthSupported")
            Prelude.<*> (x Data..:? "isBasicAuthSupported")
            Prelude.<*> (x Data..:? "isCustomAuthSupported")
            Prelude.<*> (x Data..:? "isOAuth2Supported")
            Prelude.<*> (x Data..:? "oAuth2Defaults")
      )

instance Prelude.Hashable AuthenticationConfig where
  hashWithSalt _salt AuthenticationConfig' {..} =
    _salt
      `Prelude.hashWithSalt` customAuthConfigs
      `Prelude.hashWithSalt` isApiKeyAuthSupported
      `Prelude.hashWithSalt` isBasicAuthSupported
      `Prelude.hashWithSalt` isCustomAuthSupported
      `Prelude.hashWithSalt` isOAuth2Supported
      `Prelude.hashWithSalt` oAuth2Defaults

instance Prelude.NFData AuthenticationConfig where
  rnf AuthenticationConfig' {..} =
    Prelude.rnf customAuthConfigs `Prelude.seq`
      Prelude.rnf isApiKeyAuthSupported `Prelude.seq`
        Prelude.rnf isBasicAuthSupported `Prelude.seq`
          Prelude.rnf isCustomAuthSupported `Prelude.seq`
            Prelude.rnf isOAuth2Supported `Prelude.seq`
              Prelude.rnf oAuth2Defaults
