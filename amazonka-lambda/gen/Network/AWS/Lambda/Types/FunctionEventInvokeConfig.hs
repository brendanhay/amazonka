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
-- Module      : Network.AWS.Lambda.Types.FunctionEventInvokeConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FunctionEventInvokeConfig where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.DestinationConfig
import qualified Network.AWS.Lens as Lens

-- | /See:/ 'newFunctionEventInvokeConfig' smart constructor.
data FunctionEventInvokeConfig = FunctionEventInvokeConfig'
  { -- | The maximum age of a request that Lambda sends to a function for
    -- processing.
    maximumEventAgeInSeconds :: Core.Maybe Core.Natural,
    -- | The Amazon Resource Name (ARN) of the function.
    functionArn :: Core.Maybe Core.Text,
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
    destinationConfig :: Core.Maybe DestinationConfig,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Core.Maybe Core.Natural,
    -- | The date and time that the configuration was last updated.
    lastModified :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FunctionEventInvokeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumEventAgeInSeconds', 'functionEventInvokeConfig_maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for
-- processing.
--
-- 'functionArn', 'functionEventInvokeConfig_functionArn' - The Amazon Resource Name (ARN) of the function.
--
-- 'destinationConfig', 'functionEventInvokeConfig_destinationConfig' - A destination for events after they have been sent to a function for
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
-- 'maximumRetryAttempts', 'functionEventInvokeConfig_maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
--
-- 'lastModified', 'functionEventInvokeConfig_lastModified' - The date and time that the configuration was last updated.
newFunctionEventInvokeConfig ::
  FunctionEventInvokeConfig
newFunctionEventInvokeConfig =
  FunctionEventInvokeConfig'
    { maximumEventAgeInSeconds =
        Core.Nothing,
      functionArn = Core.Nothing,
      destinationConfig = Core.Nothing,
      maximumRetryAttempts = Core.Nothing,
      lastModified = Core.Nothing
    }

-- | The maximum age of a request that Lambda sends to a function for
-- processing.
functionEventInvokeConfig_maximumEventAgeInSeconds :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Core.Natural)
functionEventInvokeConfig_maximumEventAgeInSeconds = Lens.lens (\FunctionEventInvokeConfig' {maximumEventAgeInSeconds} -> maximumEventAgeInSeconds) (\s@FunctionEventInvokeConfig' {} a -> s {maximumEventAgeInSeconds = a} :: FunctionEventInvokeConfig)

-- | The Amazon Resource Name (ARN) of the function.
functionEventInvokeConfig_functionArn :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Core.Text)
functionEventInvokeConfig_functionArn = Lens.lens (\FunctionEventInvokeConfig' {functionArn} -> functionArn) (\s@FunctionEventInvokeConfig' {} a -> s {functionArn = a} :: FunctionEventInvokeConfig)

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
functionEventInvokeConfig_destinationConfig :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe DestinationConfig)
functionEventInvokeConfig_destinationConfig = Lens.lens (\FunctionEventInvokeConfig' {destinationConfig} -> destinationConfig) (\s@FunctionEventInvokeConfig' {} a -> s {destinationConfig = a} :: FunctionEventInvokeConfig)

-- | The maximum number of times to retry when the function returns an error.
functionEventInvokeConfig_maximumRetryAttempts :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Core.Natural)
functionEventInvokeConfig_maximumRetryAttempts = Lens.lens (\FunctionEventInvokeConfig' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@FunctionEventInvokeConfig' {} a -> s {maximumRetryAttempts = a} :: FunctionEventInvokeConfig)

-- | The date and time that the configuration was last updated.
functionEventInvokeConfig_lastModified :: Lens.Lens' FunctionEventInvokeConfig (Core.Maybe Core.UTCTime)
functionEventInvokeConfig_lastModified = Lens.lens (\FunctionEventInvokeConfig' {lastModified} -> lastModified) (\s@FunctionEventInvokeConfig' {} a -> s {lastModified = a} :: FunctionEventInvokeConfig) Core.. Lens.mapping Core._Time

instance Core.FromJSON FunctionEventInvokeConfig where
  parseJSON =
    Core.withObject
      "FunctionEventInvokeConfig"
      ( \x ->
          FunctionEventInvokeConfig'
            Core.<$> (x Core..:? "MaximumEventAgeInSeconds")
            Core.<*> (x Core..:? "FunctionArn")
            Core.<*> (x Core..:? "DestinationConfig")
            Core.<*> (x Core..:? "MaximumRetryAttempts")
            Core.<*> (x Core..:? "LastModified")
      )

instance Core.Hashable FunctionEventInvokeConfig

instance Core.NFData FunctionEventInvokeConfig
