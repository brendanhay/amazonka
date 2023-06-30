{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lambda.UpdateFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the configuration for asynchronous invocation for a function,
-- version, or alias.
--
-- To configure options for asynchronous invocation, use
-- PutFunctionEventInvokeConfig.
module Amazonka.Lambda.UpdateFunctionEventInvokeConfig
  ( -- * Creating a Request
    UpdateFunctionEventInvokeConfig (..),
    newUpdateFunctionEventInvokeConfig,

    -- * Request Lenses
    updateFunctionEventInvokeConfig_destinationConfig,
    updateFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    updateFunctionEventInvokeConfig_maximumRetryAttempts,
    updateFunctionEventInvokeConfig_qualifier,
    updateFunctionEventInvokeConfig_functionName,

    -- * Destructuring the Response
    FunctionEventInvokeConfig (..),
    newFunctionEventInvokeConfig,

    -- * Response Lenses
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_lastModified,
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_maximumRetryAttempts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateFunctionEventInvokeConfig' smart constructor.
data UpdateFunctionEventInvokeConfig = UpdateFunctionEventInvokeConfig'
  { -- | A destination for events after they have been sent to a function for
    -- processing.
    --
    -- __Destinations__
    --
    -- -   __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
    --
    -- -   __Queue__ - The ARN of an SQS queue.
    --
    -- -   __Topic__ - The ARN of an SNS topic.
    --
    -- -   __Event Bus__ - The ARN of an Amazon EventBridge event bus.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | The maximum age of a request that Lambda sends to a function for
    -- processing.
    maximumEventAgeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Natural,
    -- | A version number or alias name.
    qualifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the Lambda function, version, or alias.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
    --     (with alias).
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
    --
    -- -   __Partial ARN__ - @123456789012:function:my-function@.
    --
    -- You can append a version number or alias to any of the formats. The
    -- length constraint applies only to the full ARN. If you specify only the
    -- function name, it is limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFunctionEventInvokeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationConfig', 'updateFunctionEventInvokeConfig_destinationConfig' - A destination for events after they have been sent to a function for
-- processing.
--
-- __Destinations__
--
-- -   __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
--
-- -   __Queue__ - The ARN of an SQS queue.
--
-- -   __Topic__ - The ARN of an SNS topic.
--
-- -   __Event Bus__ - The ARN of an Amazon EventBridge event bus.
--
-- 'maximumEventAgeInSeconds', 'updateFunctionEventInvokeConfig_maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for
-- processing.
--
-- 'maximumRetryAttempts', 'updateFunctionEventInvokeConfig_maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
--
-- 'qualifier', 'updateFunctionEventInvokeConfig_qualifier' - A version number or alias name.
--
-- 'functionName', 'updateFunctionEventInvokeConfig_functionName' - The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
newUpdateFunctionEventInvokeConfig ::
  -- | 'functionName'
  Prelude.Text ->
  UpdateFunctionEventInvokeConfig
newUpdateFunctionEventInvokeConfig pFunctionName_ =
  UpdateFunctionEventInvokeConfig'
    { destinationConfig =
        Prelude.Nothing,
      maximumEventAgeInSeconds = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      qualifier = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | A destination for events after they have been sent to a function for
-- processing.
--
-- __Destinations__
--
-- -   __Function__ - The Amazon Resource Name (ARN) of a Lambda function.
--
-- -   __Queue__ - The ARN of an SQS queue.
--
-- -   __Topic__ - The ARN of an SNS topic.
--
-- -   __Event Bus__ - The ARN of an Amazon EventBridge event bus.
updateFunctionEventInvokeConfig_destinationConfig :: Lens.Lens' UpdateFunctionEventInvokeConfig (Prelude.Maybe DestinationConfig)
updateFunctionEventInvokeConfig_destinationConfig = Lens.lens (\UpdateFunctionEventInvokeConfig' {destinationConfig} -> destinationConfig) (\s@UpdateFunctionEventInvokeConfig' {} a -> s {destinationConfig = a} :: UpdateFunctionEventInvokeConfig)

-- | The maximum age of a request that Lambda sends to a function for
-- processing.
updateFunctionEventInvokeConfig_maximumEventAgeInSeconds :: Lens.Lens' UpdateFunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
updateFunctionEventInvokeConfig_maximumEventAgeInSeconds = Lens.lens (\UpdateFunctionEventInvokeConfig' {maximumEventAgeInSeconds} -> maximumEventAgeInSeconds) (\s@UpdateFunctionEventInvokeConfig' {} a -> s {maximumEventAgeInSeconds = a} :: UpdateFunctionEventInvokeConfig)

-- | The maximum number of times to retry when the function returns an error.
updateFunctionEventInvokeConfig_maximumRetryAttempts :: Lens.Lens' UpdateFunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
updateFunctionEventInvokeConfig_maximumRetryAttempts = Lens.lens (\UpdateFunctionEventInvokeConfig' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@UpdateFunctionEventInvokeConfig' {} a -> s {maximumRetryAttempts = a} :: UpdateFunctionEventInvokeConfig)

-- | A version number or alias name.
updateFunctionEventInvokeConfig_qualifier :: Lens.Lens' UpdateFunctionEventInvokeConfig (Prelude.Maybe Prelude.Text)
updateFunctionEventInvokeConfig_qualifier = Lens.lens (\UpdateFunctionEventInvokeConfig' {qualifier} -> qualifier) (\s@UpdateFunctionEventInvokeConfig' {} a -> s {qualifier = a} :: UpdateFunctionEventInvokeConfig)

-- | The name of the Lambda function, version, or alias.
--
-- __Name formats__
--
-- -   __Function name__ - @my-function@ (name-only), @my-function:v1@
--     (with alias).
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:my-function@.
--
-- -   __Partial ARN__ - @123456789012:function:my-function@.
--
-- You can append a version number or alias to any of the formats. The
-- length constraint applies only to the full ARN. If you specify only the
-- function name, it is limited to 64 characters in length.
updateFunctionEventInvokeConfig_functionName :: Lens.Lens' UpdateFunctionEventInvokeConfig Prelude.Text
updateFunctionEventInvokeConfig_functionName = Lens.lens (\UpdateFunctionEventInvokeConfig' {functionName} -> functionName) (\s@UpdateFunctionEventInvokeConfig' {} a -> s {functionName = a} :: UpdateFunctionEventInvokeConfig)

instance
  Core.AWSRequest
    UpdateFunctionEventInvokeConfig
  where
  type
    AWSResponse UpdateFunctionEventInvokeConfig =
      FunctionEventInvokeConfig
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateFunctionEventInvokeConfig
  where
  hashWithSalt
    _salt
    UpdateFunctionEventInvokeConfig' {..} =
      _salt
        `Prelude.hashWithSalt` destinationConfig
        `Prelude.hashWithSalt` maximumEventAgeInSeconds
        `Prelude.hashWithSalt` maximumRetryAttempts
        `Prelude.hashWithSalt` qualifier
        `Prelude.hashWithSalt` functionName

instance
  Prelude.NFData
    UpdateFunctionEventInvokeConfig
  where
  rnf UpdateFunctionEventInvokeConfig' {..} =
    Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf maximumEventAgeInSeconds
      `Prelude.seq` Prelude.rnf maximumRetryAttempts
      `Prelude.seq` Prelude.rnf qualifier
      `Prelude.seq` Prelude.rnf functionName

instance
  Data.ToHeaders
    UpdateFunctionEventInvokeConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateFunctionEventInvokeConfig where
  toJSON UpdateFunctionEventInvokeConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DestinationConfig" Data..=)
              Prelude.<$> destinationConfig,
            ("MaximumEventAgeInSeconds" Data..=)
              Prelude.<$> maximumEventAgeInSeconds,
            ("MaximumRetryAttempts" Data..=)
              Prelude.<$> maximumRetryAttempts
          ]
      )

instance Data.ToPath UpdateFunctionEventInvokeConfig where
  toPath UpdateFunctionEventInvokeConfig' {..} =
    Prelude.mconcat
      [ "/2019-09-25/functions/",
        Data.toBS functionName,
        "/event-invoke-config"
      ]

instance Data.ToQuery UpdateFunctionEventInvokeConfig where
  toQuery UpdateFunctionEventInvokeConfig' {..} =
    Prelude.mconcat ["Qualifier" Data.=: qualifier]
