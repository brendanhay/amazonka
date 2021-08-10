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
-- Module      : Network.AWS.Lambda.PutFunctionEventInvokeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures options for
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html asynchronous invocation>
-- on a function, version, or alias. If a configuration already exists for
-- a function, version, or alias, this operation overwrites it. If you
-- exclude any settings, they are removed. To set one option without
-- affecting existing settings for other options, use
-- UpdateFunctionEventInvokeConfig.
--
-- By default, Lambda retries an asynchronous invocation twice if the
-- function returns an error. It retains events in a queue for up to six
-- hours. When an event fails all processing attempts or stays in the
-- asynchronous invocation queue for too long, Lambda discards it. To
-- retain discarded events, configure a dead-letter queue with
-- UpdateFunctionConfiguration.
--
-- To send an invocation record to a queue, topic, function, or event bus,
-- specify a
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#invocation-async-destinations destination>.
-- You can configure separate destinations for successful invocations
-- (on-success) and events that fail all processing attempts (on-failure).
-- You can configure destinations in addition to or instead of a
-- dead-letter queue.
module Network.AWS.Lambda.PutFunctionEventInvokeConfig
  ( -- * Creating a Request
    PutFunctionEventInvokeConfig (..),
    newPutFunctionEventInvokeConfig,

    -- * Request Lenses
    putFunctionEventInvokeConfig_maximumEventAgeInSeconds,
    putFunctionEventInvokeConfig_qualifier,
    putFunctionEventInvokeConfig_destinationConfig,
    putFunctionEventInvokeConfig_maximumRetryAttempts,
    putFunctionEventInvokeConfig_functionName,

    -- * Destructuring the Response
    FunctionEventInvokeConfig (..),
    newFunctionEventInvokeConfig,

    -- * Response Lenses
    functionEventInvokeConfig_maximumEventAgeInSeconds,
    functionEventInvokeConfig_functionArn,
    functionEventInvokeConfig_destinationConfig,
    functionEventInvokeConfig_maximumRetryAttempts,
    functionEventInvokeConfig_lastModified,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutFunctionEventInvokeConfig' smart constructor.
data PutFunctionEventInvokeConfig = PutFunctionEventInvokeConfig'
  { -- | The maximum age of a request that Lambda sends to a function for
    -- processing.
    maximumEventAgeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A version number or alias name.
    qualifier :: Prelude.Maybe Prelude.Text,
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
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Natural,
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
-- Create a value of 'PutFunctionEventInvokeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumEventAgeInSeconds', 'putFunctionEventInvokeConfig_maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for
-- processing.
--
-- 'qualifier', 'putFunctionEventInvokeConfig_qualifier' - A version number or alias name.
--
-- 'destinationConfig', 'putFunctionEventInvokeConfig_destinationConfig' - A destination for events after they have been sent to a function for
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
-- 'maximumRetryAttempts', 'putFunctionEventInvokeConfig_maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
--
-- 'functionName', 'putFunctionEventInvokeConfig_functionName' - The name of the Lambda function, version, or alias.
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
newPutFunctionEventInvokeConfig ::
  -- | 'functionName'
  Prelude.Text ->
  PutFunctionEventInvokeConfig
newPutFunctionEventInvokeConfig pFunctionName_ =
  PutFunctionEventInvokeConfig'
    { maximumEventAgeInSeconds =
        Prelude.Nothing,
      qualifier = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | The maximum age of a request that Lambda sends to a function for
-- processing.
putFunctionEventInvokeConfig_maximumEventAgeInSeconds :: Lens.Lens' PutFunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
putFunctionEventInvokeConfig_maximumEventAgeInSeconds = Lens.lens (\PutFunctionEventInvokeConfig' {maximumEventAgeInSeconds} -> maximumEventAgeInSeconds) (\s@PutFunctionEventInvokeConfig' {} a -> s {maximumEventAgeInSeconds = a} :: PutFunctionEventInvokeConfig)

-- | A version number or alias name.
putFunctionEventInvokeConfig_qualifier :: Lens.Lens' PutFunctionEventInvokeConfig (Prelude.Maybe Prelude.Text)
putFunctionEventInvokeConfig_qualifier = Lens.lens (\PutFunctionEventInvokeConfig' {qualifier} -> qualifier) (\s@PutFunctionEventInvokeConfig' {} a -> s {qualifier = a} :: PutFunctionEventInvokeConfig)

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
putFunctionEventInvokeConfig_destinationConfig :: Lens.Lens' PutFunctionEventInvokeConfig (Prelude.Maybe DestinationConfig)
putFunctionEventInvokeConfig_destinationConfig = Lens.lens (\PutFunctionEventInvokeConfig' {destinationConfig} -> destinationConfig) (\s@PutFunctionEventInvokeConfig' {} a -> s {destinationConfig = a} :: PutFunctionEventInvokeConfig)

-- | The maximum number of times to retry when the function returns an error.
putFunctionEventInvokeConfig_maximumRetryAttempts :: Lens.Lens' PutFunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
putFunctionEventInvokeConfig_maximumRetryAttempts = Lens.lens (\PutFunctionEventInvokeConfig' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@PutFunctionEventInvokeConfig' {} a -> s {maximumRetryAttempts = a} :: PutFunctionEventInvokeConfig)

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
putFunctionEventInvokeConfig_functionName :: Lens.Lens' PutFunctionEventInvokeConfig Prelude.Text
putFunctionEventInvokeConfig_functionName = Lens.lens (\PutFunctionEventInvokeConfig' {functionName} -> functionName) (\s@PutFunctionEventInvokeConfig' {} a -> s {functionName = a} :: PutFunctionEventInvokeConfig)

instance Core.AWSRequest PutFunctionEventInvokeConfig where
  type
    AWSResponse PutFunctionEventInvokeConfig =
      FunctionEventInvokeConfig
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    PutFunctionEventInvokeConfig

instance Prelude.NFData PutFunctionEventInvokeConfig

instance Core.ToHeaders PutFunctionEventInvokeConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PutFunctionEventInvokeConfig where
  toJSON PutFunctionEventInvokeConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaximumEventAgeInSeconds" Core..=)
              Prelude.<$> maximumEventAgeInSeconds,
            ("DestinationConfig" Core..=)
              Prelude.<$> destinationConfig,
            ("MaximumRetryAttempts" Core..=)
              Prelude.<$> maximumRetryAttempts
          ]
      )

instance Core.ToPath PutFunctionEventInvokeConfig where
  toPath PutFunctionEventInvokeConfig' {..} =
    Prelude.mconcat
      [ "/2019-09-25/functions/",
        Core.toBS functionName,
        "/event-invoke-config"
      ]

instance Core.ToQuery PutFunctionEventInvokeConfig where
  toQuery PutFunctionEventInvokeConfig' {..} =
    Prelude.mconcat ["Qualifier" Core.=: qualifier]
