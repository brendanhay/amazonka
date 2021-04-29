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
-- Module      : Network.AWS.AppSync.Types.AdditionalAuthenticationProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.AdditionalAuthenticationProvider where

import Network.AWS.AppSync.Types.AuthenticationType
import Network.AWS.AppSync.Types.CognitoUserPoolConfig
import Network.AWS.AppSync.Types.OpenIDConnectConfig
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an additional authentication provider.
--
-- /See:/ 'newAdditionalAuthenticationProvider' smart constructor.
data AdditionalAuthenticationProvider = AdditionalAuthenticationProvider'
  { -- | The OpenID Connect configuration.
    openIDConnectConfig :: Prelude.Maybe OpenIDConnectConfig,
    -- | The Amazon Cognito user pool configuration.
    userPoolConfig :: Prelude.Maybe CognitoUserPoolConfig,
    -- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
    -- pools.
    authenticationType :: Prelude.Maybe AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AdditionalAuthenticationProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectConfig', 'additionalAuthenticationProvider_openIDConnectConfig' - The OpenID Connect configuration.
--
-- 'userPoolConfig', 'additionalAuthenticationProvider_userPoolConfig' - The Amazon Cognito user pool configuration.
--
-- 'authenticationType', 'additionalAuthenticationProvider_authenticationType' - The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
-- pools.
newAdditionalAuthenticationProvider ::
  AdditionalAuthenticationProvider
newAdditionalAuthenticationProvider =
  AdditionalAuthenticationProvider'
    { openIDConnectConfig =
        Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      authenticationType = Prelude.Nothing
    }

-- | The OpenID Connect configuration.
additionalAuthenticationProvider_openIDConnectConfig :: Lens.Lens' AdditionalAuthenticationProvider (Prelude.Maybe OpenIDConnectConfig)
additionalAuthenticationProvider_openIDConnectConfig = Lens.lens (\AdditionalAuthenticationProvider' {openIDConnectConfig} -> openIDConnectConfig) (\s@AdditionalAuthenticationProvider' {} a -> s {openIDConnectConfig = a} :: AdditionalAuthenticationProvider)

-- | The Amazon Cognito user pool configuration.
additionalAuthenticationProvider_userPoolConfig :: Lens.Lens' AdditionalAuthenticationProvider (Prelude.Maybe CognitoUserPoolConfig)
additionalAuthenticationProvider_userPoolConfig = Lens.lens (\AdditionalAuthenticationProvider' {userPoolConfig} -> userPoolConfig) (\s@AdditionalAuthenticationProvider' {} a -> s {userPoolConfig = a} :: AdditionalAuthenticationProvider)

-- | The authentication type: API key, AWS IAM, OIDC, or Amazon Cognito user
-- pools.
additionalAuthenticationProvider_authenticationType :: Lens.Lens' AdditionalAuthenticationProvider (Prelude.Maybe AuthenticationType)
additionalAuthenticationProvider_authenticationType = Lens.lens (\AdditionalAuthenticationProvider' {authenticationType} -> authenticationType) (\s@AdditionalAuthenticationProvider' {} a -> s {authenticationType = a} :: AdditionalAuthenticationProvider)

instance
  Prelude.FromJSON
    AdditionalAuthenticationProvider
  where
  parseJSON =
    Prelude.withObject
      "AdditionalAuthenticationProvider"
      ( \x ->
          AdditionalAuthenticationProvider'
            Prelude.<$> (x Prelude..:? "openIDConnectConfig")
            Prelude.<*> (x Prelude..:? "userPoolConfig")
            Prelude.<*> (x Prelude..:? "authenticationType")
      )

instance
  Prelude.Hashable
    AdditionalAuthenticationProvider

instance
  Prelude.NFData
    AdditionalAuthenticationProvider

instance
  Prelude.ToJSON
    AdditionalAuthenticationProvider
  where
  toJSON AdditionalAuthenticationProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("openIDConnectConfig" Prelude..=)
              Prelude.<$> openIDConnectConfig,
            ("userPoolConfig" Prelude..=)
              Prelude.<$> userPoolConfig,
            ("authenticationType" Prelude..=)
              Prelude.<$> authenticationType
          ]
      )
