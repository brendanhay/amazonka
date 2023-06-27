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
-- Module      : Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the authorization configuration for using an Lambda function
-- with your AppSync GraphQL API endpoint.
--
-- /See:/ 'newAwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' smart constructor.
data AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails = AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails'
  { -- | The number of seconds a response should be cached for. The default is 5
    -- minutes (300 seconds).
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the Lambda function to be called for
    -- authorization. This can be a standard Lambda ARN, a version ARN
    -- (...\/v3), or an alias ARN.
    authorizerUri :: Prelude.Maybe Prelude.Text,
    -- | A regular expression for validation of tokens before the Lambda function
    -- is called.
    identityValidationExpression :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerResultTtlInSeconds', 'awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerResultTtlInSeconds' - The number of seconds a response should be cached for. The default is 5
-- minutes (300 seconds).
--
-- 'authorizerUri', 'awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerUri' - The Amazon Resource Name (ARN) of the Lambda function to be called for
-- authorization. This can be a standard Lambda ARN, a version ARN
-- (...\/v3), or an alias ARN.
--
-- 'identityValidationExpression', 'awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_identityValidationExpression' - A regular expression for validation of tokens before the Lambda function
-- is called.
newAwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails ::
  AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
newAwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails =
  AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails'
    { authorizerResultTtlInSeconds =
        Prelude.Nothing,
      authorizerUri =
        Prelude.Nothing,
      identityValidationExpression =
        Prelude.Nothing
    }

-- | The number of seconds a response should be cached for. The default is 5
-- minutes (300 seconds).
awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerResultTtlInSeconds :: Lens.Lens' AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails (Prelude.Maybe Prelude.Int)
awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerResultTtlInSeconds = Lens.lens (\AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {} a -> s {authorizerResultTtlInSeconds = a} :: AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails)

-- | The Amazon Resource Name (ARN) of the Lambda function to be called for
-- authorization. This can be a standard Lambda ARN, a version ARN
-- (...\/v3), or an alias ARN.
awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerUri :: Lens.Lens' AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_authorizerUri = Lens.lens (\AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {authorizerUri} -> authorizerUri) (\s@AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {} a -> s {authorizerUri = a} :: AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails)

-- | A regular expression for validation of tokens before the Lambda function
-- is called.
awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_identityValidationExpression :: Lens.Lens' AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails (Prelude.Maybe Prelude.Text)
awsAppSyncGraphQlApiLambdaAuthorizerConfigDetails_identityValidationExpression = Lens.lens (\AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {identityValidationExpression} -> identityValidationExpression) (\s@AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {} a -> s {identityValidationExpression = a} :: AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails)

instance
  Data.FromJSON
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
  where
  parseJSON =
    Data.withObject
      "AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails"
      ( \x ->
          AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails'
            Prelude.<$> (x Data..:? "AuthorizerResultTtlInSeconds")
            Prelude.<*> (x Data..:? "AuthorizerUri")
            Prelude.<*> (x Data..:? "IdentityValidationExpression")
      )

instance
  Prelude.Hashable
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
  where
  hashWithSalt
    _salt
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {..} =
      _salt
        `Prelude.hashWithSalt` authorizerResultTtlInSeconds
        `Prelude.hashWithSalt` authorizerUri
        `Prelude.hashWithSalt` identityValidationExpression

instance
  Prelude.NFData
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
  where
  rnf
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {..} =
      Prelude.rnf authorizerResultTtlInSeconds
        `Prelude.seq` Prelude.rnf authorizerUri
        `Prelude.seq` Prelude.rnf identityValidationExpression

instance
  Data.ToJSON
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails
  where
  toJSON
    AwsAppSyncGraphQlApiLambdaAuthorizerConfigDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AuthorizerResultTtlInSeconds" Data..=)
                Prelude.<$> authorizerResultTtlInSeconds,
              ("AuthorizerUri" Data..=) Prelude.<$> authorizerUri,
              ("IdentityValidationExpression" Data..=)
                Prelude.<$> identityValidationExpression
            ]
        )
