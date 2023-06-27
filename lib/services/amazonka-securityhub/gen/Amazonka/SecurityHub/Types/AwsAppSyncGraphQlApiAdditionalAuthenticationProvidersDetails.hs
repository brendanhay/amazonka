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
-- Module      : Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiUserPoolConfigDetails

-- | A list of additional authentication providers for the GraphqlApi API.
--
-- /See:/ 'newAwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' smart constructor.
data AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails = AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails'
  { -- | The type of security configuration for your GraphQL API: API key,
    -- Identity and Access Management (IAM), OpenID Connect (OIDC), Amazon
    -- Cognito user pools, or Lambda.
    authenticationType :: Prelude.Maybe Prelude.Text,
    -- | The configuration for Lambda function authorization.
    lambdaAuthorizerConfig :: Prelude.Maybe AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails,
    -- | The OpenID Connect configuration.
    openIdConnectConfig :: Prelude.Maybe AwsAppSyncGraphQlApiOpenIdConnectConfigDetails,
    -- | The Amazon Cognito user pools configuration.
    userPoolConfig :: Prelude.Maybe AwsAppSyncGraphQlApiUserPoolConfigDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationType', 'awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_authenticationType' - The type of security configuration for your GraphQL API: API key,
-- Identity and Access Management (IAM), OpenID Connect (OIDC), Amazon
-- Cognito user pools, or Lambda.
--
-- 'lambdaAuthorizerConfig', 'awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_lambdaAuthorizerConfig' - The configuration for Lambda function authorization.
--
-- 'openIdConnectConfig', 'awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_openIdConnectConfig' - The OpenID Connect configuration.
--
-- 'userPoolConfig', 'awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_userPoolConfig' - The Amazon Cognito user pools configuration.
newAwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails ::
  AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
newAwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails =
  AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails'
    { authenticationType =
        Prelude.Nothing,
      lambdaAuthorizerConfig =
        Prelude.Nothing,
      openIdConnectConfig =
        Prelude.Nothing,
      userPoolConfig =
        Prelude.Nothing
    }

-- | The type of security configuration for your GraphQL API: API key,
-- Identity and Access Management (IAM), OpenID Connect (OIDC), Amazon
-- Cognito user pools, or Lambda.
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_authenticationType :: Lens.Lens' AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_authenticationType = Lens.lens (\AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {authenticationType} -> authenticationType) (\s@AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {} a -> s {authenticationType = a} :: AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails)

-- | The configuration for Lambda function authorization.
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_lambdaAuthorizerConfig :: Lens.Lens' AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails (Prelude.Maybe AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails)
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_lambdaAuthorizerConfig = Lens.lens (\AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {lambdaAuthorizerConfig} -> lambdaAuthorizerConfig) (\s@AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {} a -> s {lambdaAuthorizerConfig = a} :: AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails)

-- | The OpenID Connect configuration.
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_openIdConnectConfig :: Lens.Lens' AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails (Prelude.Maybe AwsAppSyncGraphQlApiOpenIdConnectConfigDetails)
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_openIdConnectConfig = Lens.lens (\AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {openIdConnectConfig} -> openIdConnectConfig) (\s@AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {} a -> s {openIdConnectConfig = a} :: AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails)

-- | The Amazon Cognito user pools configuration.
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_userPoolConfig :: Lens.Lens' AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails (Prelude.Maybe AwsAppSyncGraphQlApiUserPoolConfigDetails)
awsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails_userPoolConfig = Lens.lens (\AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {userPoolConfig} -> userPoolConfig) (\s@AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {} a -> s {userPoolConfig = a} :: AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails)

instance
  Data.FromJSON
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
  where
  parseJSON =
    Data.withObject
      "AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails"
      ( \x ->
          AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails'
            Prelude.<$> (x Data..:? "AuthenticationType")
            Prelude.<*> (x Data..:? "LambdaAuthorizerConfig")
            Prelude.<*> (x Data..:? "OpenIdConnectConfig")
            Prelude.<*> (x Data..:? "UserPoolConfig")
      )

instance
  Prelude.Hashable
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
  where
  hashWithSalt
    _salt
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {..} =
      _salt
        `Prelude.hashWithSalt` authenticationType
        `Prelude.hashWithSalt` lambdaAuthorizerConfig
        `Prelude.hashWithSalt` openIdConnectConfig
        `Prelude.hashWithSalt` userPoolConfig

instance
  Prelude.NFData
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
  where
  rnf
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {..} =
      Prelude.rnf authenticationType
        `Prelude.seq` Prelude.rnf lambdaAuthorizerConfig
        `Prelude.seq` Prelude.rnf openIdConnectConfig
        `Prelude.seq` Prelude.rnf userPoolConfig

instance
  Data.ToJSON
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
  where
  toJSON
    AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AuthenticationType" Data..=)
                Prelude.<$> authenticationType,
              ("LambdaAuthorizerConfig" Data..=)
                Prelude.<$> lambdaAuthorizerConfig,
              ("OpenIdConnectConfig" Data..=)
                Prelude.<$> openIdConnectConfig,
              ("UserPoolConfig" Data..=)
                Prelude.<$> userPoolConfig
            ]
        )
