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
import Network.AWS.AppSync.Types.LambdaAuthorizerConfig
import Network.AWS.AppSync.Types.OpenIDConnectConfig
import qualified Network.AWS.Core as Core
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
    -- | Configuration for Amazon Web Services Lambda function authorization.
    lambdaAuthorizerConfig :: Prelude.Maybe LambdaAuthorizerConfig,
    -- | The authentication type: API key, Identity and Access Management, OIDC,
    -- Amazon Cognito user pools, or Amazon Web Services Lambda.
    authenticationType :: Prelude.Maybe AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'lambdaAuthorizerConfig', 'additionalAuthenticationProvider_lambdaAuthorizerConfig' - Configuration for Amazon Web Services Lambda function authorization.
--
-- 'authenticationType', 'additionalAuthenticationProvider_authenticationType' - The authentication type: API key, Identity and Access Management, OIDC,
-- Amazon Cognito user pools, or Amazon Web Services Lambda.
newAdditionalAuthenticationProvider ::
  AdditionalAuthenticationProvider
newAdditionalAuthenticationProvider =
  AdditionalAuthenticationProvider'
    { openIDConnectConfig =
        Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      lambdaAuthorizerConfig = Prelude.Nothing,
      authenticationType = Prelude.Nothing
    }

-- | The OpenID Connect configuration.
additionalAuthenticationProvider_openIDConnectConfig :: Lens.Lens' AdditionalAuthenticationProvider (Prelude.Maybe OpenIDConnectConfig)
additionalAuthenticationProvider_openIDConnectConfig = Lens.lens (\AdditionalAuthenticationProvider' {openIDConnectConfig} -> openIDConnectConfig) (\s@AdditionalAuthenticationProvider' {} a -> s {openIDConnectConfig = a} :: AdditionalAuthenticationProvider)

-- | The Amazon Cognito user pool configuration.
additionalAuthenticationProvider_userPoolConfig :: Lens.Lens' AdditionalAuthenticationProvider (Prelude.Maybe CognitoUserPoolConfig)
additionalAuthenticationProvider_userPoolConfig = Lens.lens (\AdditionalAuthenticationProvider' {userPoolConfig} -> userPoolConfig) (\s@AdditionalAuthenticationProvider' {} a -> s {userPoolConfig = a} :: AdditionalAuthenticationProvider)

-- | Configuration for Amazon Web Services Lambda function authorization.
additionalAuthenticationProvider_lambdaAuthorizerConfig :: Lens.Lens' AdditionalAuthenticationProvider (Prelude.Maybe LambdaAuthorizerConfig)
additionalAuthenticationProvider_lambdaAuthorizerConfig = Lens.lens (\AdditionalAuthenticationProvider' {lambdaAuthorizerConfig} -> lambdaAuthorizerConfig) (\s@AdditionalAuthenticationProvider' {} a -> s {lambdaAuthorizerConfig = a} :: AdditionalAuthenticationProvider)

-- | The authentication type: API key, Identity and Access Management, OIDC,
-- Amazon Cognito user pools, or Amazon Web Services Lambda.
additionalAuthenticationProvider_authenticationType :: Lens.Lens' AdditionalAuthenticationProvider (Prelude.Maybe AuthenticationType)
additionalAuthenticationProvider_authenticationType = Lens.lens (\AdditionalAuthenticationProvider' {authenticationType} -> authenticationType) (\s@AdditionalAuthenticationProvider' {} a -> s {authenticationType = a} :: AdditionalAuthenticationProvider)

instance
  Core.FromJSON
    AdditionalAuthenticationProvider
  where
  parseJSON =
    Core.withObject
      "AdditionalAuthenticationProvider"
      ( \x ->
          AdditionalAuthenticationProvider'
            Prelude.<$> (x Core..:? "openIDConnectConfig")
            Prelude.<*> (x Core..:? "userPoolConfig")
            Prelude.<*> (x Core..:? "lambdaAuthorizerConfig")
            Prelude.<*> (x Core..:? "authenticationType")
      )

instance
  Prelude.Hashable
    AdditionalAuthenticationProvider

instance
  Prelude.NFData
    AdditionalAuthenticationProvider

instance Core.ToJSON AdditionalAuthenticationProvider where
  toJSON AdditionalAuthenticationProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("openIDConnectConfig" Core..=)
              Prelude.<$> openIDConnectConfig,
            ("userPoolConfig" Core..=)
              Prelude.<$> userPoolConfig,
            ("lambdaAuthorizerConfig" Core..=)
              Prelude.<$> lambdaAuthorizerConfig,
            ("authenticationType" Core..=)
              Prelude.<$> authenticationType
          ]
      )
