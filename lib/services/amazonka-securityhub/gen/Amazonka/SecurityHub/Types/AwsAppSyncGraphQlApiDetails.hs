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
-- Module      : Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLogConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiOpenIdConnectConfigDetails
import Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiUserPoolConfigDetails

-- | Provides details about an AppSync Graph QL API, which lets you query
-- multiple databases, microservices, and APIs from a single GraphQL
-- endpoint.
--
-- /See:/ 'newAwsAppSyncGraphQlApiDetails' smart constructor.
data AwsAppSyncGraphQlApiDetails = AwsAppSyncGraphQlApiDetails'
  { -- | A list of additional authentication providers for the GraphQL API.
    additionalAuthenticationProviders :: Prelude.Maybe [AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails],
    -- | The unique identifier for the API.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the API.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of security configuration for your GraphQL API: API key,
    -- Identity and Access Management (IAM), OpenID Connect (OIDC), Amazon
    -- Cognito user pools, or Lambda.
    authenticationType :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the API.
    id :: Prelude.Maybe Prelude.Text,
    -- | Specifies the configuration for Lambda function authorization.
    lambdaAuthorizerConfig :: Prelude.Maybe AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails,
    -- | The Amazon CloudWatch Logs configuration.
    logConfig :: Prelude.Maybe AwsAppSyncGraphQlApiLogConfigDetails,
    -- | The API name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the authorization configuration for using an OpenID Connect
    -- compliant service with an AppSync GraphQL API endpoint.
    openIdConnectConfig :: Prelude.Maybe AwsAppSyncGraphQlApiOpenIdConnectConfigDetails,
    -- | The Amazon Cognito user pools configuration.
    userPoolConfig :: Prelude.Maybe AwsAppSyncGraphQlApiUserPoolConfigDetails,
    -- | The Amazon Resource Name (ARN) of the WAF web access control list (web
    -- ACL) associated with this GraphQL API, if one exists.
    wafWebAclArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to use X-Ray tracing for the GraphQL API.
    xrayEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAppSyncGraphQlApiDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalAuthenticationProviders', 'awsAppSyncGraphQlApiDetails_additionalAuthenticationProviders' - A list of additional authentication providers for the GraphQL API.
--
-- 'apiId', 'awsAppSyncGraphQlApiDetails_apiId' - The unique identifier for the API.
--
-- 'arn', 'awsAppSyncGraphQlApiDetails_arn' - The Amazon Resource Name (ARN) of the API.
--
-- 'authenticationType', 'awsAppSyncGraphQlApiDetails_authenticationType' - The type of security configuration for your GraphQL API: API key,
-- Identity and Access Management (IAM), OpenID Connect (OIDC), Amazon
-- Cognito user pools, or Lambda.
--
-- 'id', 'awsAppSyncGraphQlApiDetails_id' - The unique identifier for the API.
--
-- 'lambdaAuthorizerConfig', 'awsAppSyncGraphQlApiDetails_lambdaAuthorizerConfig' - Specifies the configuration for Lambda function authorization.
--
-- 'logConfig', 'awsAppSyncGraphQlApiDetails_logConfig' - The Amazon CloudWatch Logs configuration.
--
-- 'name', 'awsAppSyncGraphQlApiDetails_name' - The API name.
--
-- 'openIdConnectConfig', 'awsAppSyncGraphQlApiDetails_openIdConnectConfig' - Specifies the authorization configuration for using an OpenID Connect
-- compliant service with an AppSync GraphQL API endpoint.
--
-- 'userPoolConfig', 'awsAppSyncGraphQlApiDetails_userPoolConfig' - The Amazon Cognito user pools configuration.
--
-- 'wafWebAclArn', 'awsAppSyncGraphQlApiDetails_wafWebAclArn' - The Amazon Resource Name (ARN) of the WAF web access control list (web
-- ACL) associated with this GraphQL API, if one exists.
--
-- 'xrayEnabled', 'awsAppSyncGraphQlApiDetails_xrayEnabled' - Indicates whether to use X-Ray tracing for the GraphQL API.
newAwsAppSyncGraphQlApiDetails ::
  AwsAppSyncGraphQlApiDetails
newAwsAppSyncGraphQlApiDetails =
  AwsAppSyncGraphQlApiDetails'
    { additionalAuthenticationProviders =
        Prelude.Nothing,
      apiId = Prelude.Nothing,
      arn = Prelude.Nothing,
      authenticationType = Prelude.Nothing,
      id = Prelude.Nothing,
      lambdaAuthorizerConfig = Prelude.Nothing,
      logConfig = Prelude.Nothing,
      name = Prelude.Nothing,
      openIdConnectConfig = Prelude.Nothing,
      userPoolConfig = Prelude.Nothing,
      wafWebAclArn = Prelude.Nothing,
      xrayEnabled = Prelude.Nothing
    }

-- | A list of additional authentication providers for the GraphQL API.
awsAppSyncGraphQlApiDetails_additionalAuthenticationProviders :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe [AwsAppSyncGraphQlApiAdditionalAuthenticationProvidersDetails])
awsAppSyncGraphQlApiDetails_additionalAuthenticationProviders = Lens.lens (\AwsAppSyncGraphQlApiDetails' {additionalAuthenticationProviders} -> additionalAuthenticationProviders) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {additionalAuthenticationProviders = a} :: AwsAppSyncGraphQlApiDetails) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the API.
awsAppSyncGraphQlApiDetails_apiId :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiDetails_apiId = Lens.lens (\AwsAppSyncGraphQlApiDetails' {apiId} -> apiId) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {apiId = a} :: AwsAppSyncGraphQlApiDetails)

-- | The Amazon Resource Name (ARN) of the API.
awsAppSyncGraphQlApiDetails_arn :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiDetails_arn = Lens.lens (\AwsAppSyncGraphQlApiDetails' {arn} -> arn) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {arn = a} :: AwsAppSyncGraphQlApiDetails)

-- | The type of security configuration for your GraphQL API: API key,
-- Identity and Access Management (IAM), OpenID Connect (OIDC), Amazon
-- Cognito user pools, or Lambda.
awsAppSyncGraphQlApiDetails_authenticationType :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiDetails_authenticationType = Lens.lens (\AwsAppSyncGraphQlApiDetails' {authenticationType} -> authenticationType) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {authenticationType = a} :: AwsAppSyncGraphQlApiDetails)

-- | The unique identifier for the API.
awsAppSyncGraphQlApiDetails_id :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiDetails_id = Lens.lens (\AwsAppSyncGraphQlApiDetails' {id} -> id) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {id = a} :: AwsAppSyncGraphQlApiDetails)

-- | Specifies the configuration for Lambda function authorization.
awsAppSyncGraphQlApiDetails_lambdaAuthorizerConfig :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails)
awsAppSyncGraphQlApiDetails_lambdaAuthorizerConfig = Lens.lens (\AwsAppSyncGraphQlApiDetails' {lambdaAuthorizerConfig} -> lambdaAuthorizerConfig) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {lambdaAuthorizerConfig = a} :: AwsAppSyncGraphQlApiDetails)

-- | The Amazon CloudWatch Logs configuration.
awsAppSyncGraphQlApiDetails_logConfig :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe AwsAppSyncGraphQlApiLogConfigDetails)
awsAppSyncGraphQlApiDetails_logConfig = Lens.lens (\AwsAppSyncGraphQlApiDetails' {logConfig} -> logConfig) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {logConfig = a} :: AwsAppSyncGraphQlApiDetails)

-- | The API name.
awsAppSyncGraphQlApiDetails_name :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiDetails_name = Lens.lens (\AwsAppSyncGraphQlApiDetails' {name} -> name) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {name = a} :: AwsAppSyncGraphQlApiDetails)

-- | Specifies the authorization configuration for using an OpenID Connect
-- compliant service with an AppSync GraphQL API endpoint.
awsAppSyncGraphQlApiDetails_openIdConnectConfig :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe AwsAppSyncGraphQlApiOpenIdConnectConfigDetails)
awsAppSyncGraphQlApiDetails_openIdConnectConfig = Lens.lens (\AwsAppSyncGraphQlApiDetails' {openIdConnectConfig} -> openIdConnectConfig) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {openIdConnectConfig = a} :: AwsAppSyncGraphQlApiDetails)

-- | The Amazon Cognito user pools configuration.
awsAppSyncGraphQlApiDetails_userPoolConfig :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe AwsAppSyncGraphQlApiUserPoolConfigDetails)
awsAppSyncGraphQlApiDetails_userPoolConfig = Lens.lens (\AwsAppSyncGraphQlApiDetails' {userPoolConfig} -> userPoolConfig) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {userPoolConfig = a} :: AwsAppSyncGraphQlApiDetails)

-- | The Amazon Resource Name (ARN) of the WAF web access control list (web
-- ACL) associated with this GraphQL API, if one exists.
awsAppSyncGraphQlApiDetails_wafWebAclArn :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiDetails_wafWebAclArn = Lens.lens (\AwsAppSyncGraphQlApiDetails' {wafWebAclArn} -> wafWebAclArn) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {wafWebAclArn = a} :: AwsAppSyncGraphQlApiDetails)

-- | Indicates whether to use X-Ray tracing for the GraphQL API.
awsAppSyncGraphQlApiDetails_xrayEnabled :: Lens.Lens' AwsAppSyncGraphQlApiDetails (Prelude.Maybe Prelude.Bool)
awsAppSyncGraphQlApiDetails_xrayEnabled = Lens.lens (\AwsAppSyncGraphQlApiDetails' {xrayEnabled} -> xrayEnabled) (\s@AwsAppSyncGraphQlApiDetails' {} a -> s {xrayEnabled = a} :: AwsAppSyncGraphQlApiDetails)

instance Data.FromJSON AwsAppSyncGraphQlApiDetails where
  parseJSON =
    Data.withObject
      "AwsAppSyncGraphQlApiDetails"
      ( \x ->
          AwsAppSyncGraphQlApiDetails'
            Prelude.<$> ( x
                            Data..:? "AdditionalAuthenticationProviders"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ApiId")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "AuthenticationType")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LambdaAuthorizerConfig")
            Prelude.<*> (x Data..:? "LogConfig")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OpenIdConnectConfig")
            Prelude.<*> (x Data..:? "UserPoolConfig")
            Prelude.<*> (x Data..:? "WafWebAclArn")
            Prelude.<*> (x Data..:? "XrayEnabled")
      )

instance Prelude.Hashable AwsAppSyncGraphQlApiDetails where
  hashWithSalt _salt AwsAppSyncGraphQlApiDetails' {..} =
    _salt
      `Prelude.hashWithSalt` additionalAuthenticationProviders
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lambdaAuthorizerConfig
      `Prelude.hashWithSalt` logConfig
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` openIdConnectConfig
      `Prelude.hashWithSalt` userPoolConfig
      `Prelude.hashWithSalt` wafWebAclArn
      `Prelude.hashWithSalt` xrayEnabled

instance Prelude.NFData AwsAppSyncGraphQlApiDetails where
  rnf AwsAppSyncGraphQlApiDetails' {..} =
    Prelude.rnf additionalAuthenticationProviders
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lambdaAuthorizerConfig
      `Prelude.seq` Prelude.rnf logConfig
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf openIdConnectConfig
      `Prelude.seq` Prelude.rnf userPoolConfig
      `Prelude.seq` Prelude.rnf wafWebAclArn
      `Prelude.seq` Prelude.rnf xrayEnabled

instance Data.ToJSON AwsAppSyncGraphQlApiDetails where
  toJSON AwsAppSyncGraphQlApiDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalAuthenticationProviders" Data..=)
              Prelude.<$> additionalAuthenticationProviders,
            ("ApiId" Data..=) Prelude.<$> apiId,
            ("Arn" Data..=) Prelude.<$> arn,
            ("AuthenticationType" Data..=)
              Prelude.<$> authenticationType,
            ("Id" Data..=) Prelude.<$> id,
            ("LambdaAuthorizerConfig" Data..=)
              Prelude.<$> lambdaAuthorizerConfig,
            ("LogConfig" Data..=) Prelude.<$> logConfig,
            ("Name" Data..=) Prelude.<$> name,
            ("OpenIdConnectConfig" Data..=)
              Prelude.<$> openIdConnectConfig,
            ("UserPoolConfig" Data..=)
              Prelude.<$> userPoolConfig,
            ("WafWebAclArn" Data..=) Prelude.<$> wafWebAclArn,
            ("XrayEnabled" Data..=) Prelude.<$> xrayEnabled
          ]
      )
