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
-- Module      : Amazonka.AppSync.Types.LambdaAuthorizerConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppSync.Types.LambdaAuthorizerConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A @LambdaAuthorizerConfig@ specifies how to authorize AppSync API access
-- when using the @AWS_LAMBDA@ authorizer mode. Be aware that an AppSync
-- API can have only one Lambda authorizer configured at a time.
--
-- /See:/ 'newLambdaAuthorizerConfig' smart constructor.
data LambdaAuthorizerConfig = LambdaAuthorizerConfig'
  { -- | The number of seconds a response should be cached for. The default is 5
    -- minutes (300 seconds). The Lambda function can override this by
    -- returning a @ttlOverride@ key in its response. A value of 0 disables
    -- caching of responses.
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A regular expression for validation of tokens before the Lambda function
    -- is called.
    identityValidationExpression :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Lambda function to be called for
    -- authorization. This can be a standard Lambda ARN, a version ARN
    -- (@...\/v3@), or an alias ARN.
    --
    -- __Note__: This Lambda function must have the following resource-based
    -- policy assigned to it. When configuring Lambda authorizers in the
    -- console, this is done for you. To use the Command Line Interface (CLI),
    -- run the following:
    --
    -- @aws lambda add-permission --function-name \"arn:aws:lambda:us-east-2:111122223333:function:my-function\" --statement-id \"appsync\" --principal appsync.amazonaws.com --action lambda:InvokeFunction@
    authorizerUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaAuthorizerConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerResultTtlInSeconds', 'lambdaAuthorizerConfig_authorizerResultTtlInSeconds' - The number of seconds a response should be cached for. The default is 5
-- minutes (300 seconds). The Lambda function can override this by
-- returning a @ttlOverride@ key in its response. A value of 0 disables
-- caching of responses.
--
-- 'identityValidationExpression', 'lambdaAuthorizerConfig_identityValidationExpression' - A regular expression for validation of tokens before the Lambda function
-- is called.
--
-- 'authorizerUri', 'lambdaAuthorizerConfig_authorizerUri' - The Amazon Resource Name (ARN) of the Lambda function to be called for
-- authorization. This can be a standard Lambda ARN, a version ARN
-- (@...\/v3@), or an alias ARN.
--
-- __Note__: This Lambda function must have the following resource-based
-- policy assigned to it. When configuring Lambda authorizers in the
-- console, this is done for you. To use the Command Line Interface (CLI),
-- run the following:
--
-- @aws lambda add-permission --function-name \"arn:aws:lambda:us-east-2:111122223333:function:my-function\" --statement-id \"appsync\" --principal appsync.amazonaws.com --action lambda:InvokeFunction@
newLambdaAuthorizerConfig ::
  -- | 'authorizerUri'
  Prelude.Text ->
  LambdaAuthorizerConfig
newLambdaAuthorizerConfig pAuthorizerUri_ =
  LambdaAuthorizerConfig'
    { authorizerResultTtlInSeconds =
        Prelude.Nothing,
      identityValidationExpression = Prelude.Nothing,
      authorizerUri = pAuthorizerUri_
    }

-- | The number of seconds a response should be cached for. The default is 5
-- minutes (300 seconds). The Lambda function can override this by
-- returning a @ttlOverride@ key in its response. A value of 0 disables
-- caching of responses.
lambdaAuthorizerConfig_authorizerResultTtlInSeconds :: Lens.Lens' LambdaAuthorizerConfig (Prelude.Maybe Prelude.Natural)
lambdaAuthorizerConfig_authorizerResultTtlInSeconds = Lens.lens (\LambdaAuthorizerConfig' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@LambdaAuthorizerConfig' {} a -> s {authorizerResultTtlInSeconds = a} :: LambdaAuthorizerConfig)

-- | A regular expression for validation of tokens before the Lambda function
-- is called.
lambdaAuthorizerConfig_identityValidationExpression :: Lens.Lens' LambdaAuthorizerConfig (Prelude.Maybe Prelude.Text)
lambdaAuthorizerConfig_identityValidationExpression = Lens.lens (\LambdaAuthorizerConfig' {identityValidationExpression} -> identityValidationExpression) (\s@LambdaAuthorizerConfig' {} a -> s {identityValidationExpression = a} :: LambdaAuthorizerConfig)

-- | The Amazon Resource Name (ARN) of the Lambda function to be called for
-- authorization. This can be a standard Lambda ARN, a version ARN
-- (@...\/v3@), or an alias ARN.
--
-- __Note__: This Lambda function must have the following resource-based
-- policy assigned to it. When configuring Lambda authorizers in the
-- console, this is done for you. To use the Command Line Interface (CLI),
-- run the following:
--
-- @aws lambda add-permission --function-name \"arn:aws:lambda:us-east-2:111122223333:function:my-function\" --statement-id \"appsync\" --principal appsync.amazonaws.com --action lambda:InvokeFunction@
lambdaAuthorizerConfig_authorizerUri :: Lens.Lens' LambdaAuthorizerConfig Prelude.Text
lambdaAuthorizerConfig_authorizerUri = Lens.lens (\LambdaAuthorizerConfig' {authorizerUri} -> authorizerUri) (\s@LambdaAuthorizerConfig' {} a -> s {authorizerUri = a} :: LambdaAuthorizerConfig)

instance Data.FromJSON LambdaAuthorizerConfig where
  parseJSON =
    Data.withObject
      "LambdaAuthorizerConfig"
      ( \x ->
          LambdaAuthorizerConfig'
            Prelude.<$> (x Data..:? "authorizerResultTtlInSeconds")
            Prelude.<*> (x Data..:? "identityValidationExpression")
            Prelude.<*> (x Data..: "authorizerUri")
      )

instance Prelude.Hashable LambdaAuthorizerConfig where
  hashWithSalt _salt LambdaAuthorizerConfig' {..} =
    _salt
      `Prelude.hashWithSalt` authorizerResultTtlInSeconds
      `Prelude.hashWithSalt` identityValidationExpression
      `Prelude.hashWithSalt` authorizerUri

instance Prelude.NFData LambdaAuthorizerConfig where
  rnf LambdaAuthorizerConfig' {..} =
    Prelude.rnf authorizerResultTtlInSeconds
      `Prelude.seq` Prelude.rnf identityValidationExpression
      `Prelude.seq` Prelude.rnf authorizerUri

instance Data.ToJSON LambdaAuthorizerConfig where
  toJSON LambdaAuthorizerConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorizerResultTtlInSeconds" Data..=)
              Prelude.<$> authorizerResultTtlInSeconds,
            ("identityValidationExpression" Data..=)
              Prelude.<$> identityValidationExpression,
            Prelude.Just
              ("authorizerUri" Data..= authorizerUri)
          ]
      )
