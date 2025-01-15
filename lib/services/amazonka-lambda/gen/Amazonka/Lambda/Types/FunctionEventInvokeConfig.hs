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
-- Module      : Amazonka.Lambda.Types.FunctionEventInvokeConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionEventInvokeConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.DestinationConfig
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newFunctionEventInvokeConfig' smart constructor.
data FunctionEventInvokeConfig = FunctionEventInvokeConfig'
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
    -- | The Amazon Resource Name (ARN) of the function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the configuration was last updated.
    lastModified :: Prelude.Maybe Data.POSIX,
    -- | The maximum age of a request that Lambda sends to a function for
    -- processing.
    maximumEventAgeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of times to retry when the function returns an error.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionEventInvokeConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'functionArn', 'functionEventInvokeConfig_functionArn' - The Amazon Resource Name (ARN) of the function.
--
-- 'lastModified', 'functionEventInvokeConfig_lastModified' - The date and time that the configuration was last updated.
--
-- 'maximumEventAgeInSeconds', 'functionEventInvokeConfig_maximumEventAgeInSeconds' - The maximum age of a request that Lambda sends to a function for
-- processing.
--
-- 'maximumRetryAttempts', 'functionEventInvokeConfig_maximumRetryAttempts' - The maximum number of times to retry when the function returns an error.
newFunctionEventInvokeConfig ::
  FunctionEventInvokeConfig
newFunctionEventInvokeConfig =
  FunctionEventInvokeConfig'
    { destinationConfig =
        Prelude.Nothing,
      functionArn = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      maximumEventAgeInSeconds = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing
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
functionEventInvokeConfig_destinationConfig :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe DestinationConfig)
functionEventInvokeConfig_destinationConfig = Lens.lens (\FunctionEventInvokeConfig' {destinationConfig} -> destinationConfig) (\s@FunctionEventInvokeConfig' {} a -> s {destinationConfig = a} :: FunctionEventInvokeConfig)

-- | The Amazon Resource Name (ARN) of the function.
functionEventInvokeConfig_functionArn :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.Text)
functionEventInvokeConfig_functionArn = Lens.lens (\FunctionEventInvokeConfig' {functionArn} -> functionArn) (\s@FunctionEventInvokeConfig' {} a -> s {functionArn = a} :: FunctionEventInvokeConfig)

-- | The date and time that the configuration was last updated.
functionEventInvokeConfig_lastModified :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.UTCTime)
functionEventInvokeConfig_lastModified = Lens.lens (\FunctionEventInvokeConfig' {lastModified} -> lastModified) (\s@FunctionEventInvokeConfig' {} a -> s {lastModified = a} :: FunctionEventInvokeConfig) Prelude.. Lens.mapping Data._Time

-- | The maximum age of a request that Lambda sends to a function for
-- processing.
functionEventInvokeConfig_maximumEventAgeInSeconds :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
functionEventInvokeConfig_maximumEventAgeInSeconds = Lens.lens (\FunctionEventInvokeConfig' {maximumEventAgeInSeconds} -> maximumEventAgeInSeconds) (\s@FunctionEventInvokeConfig' {} a -> s {maximumEventAgeInSeconds = a} :: FunctionEventInvokeConfig)

-- | The maximum number of times to retry when the function returns an error.
functionEventInvokeConfig_maximumRetryAttempts :: Lens.Lens' FunctionEventInvokeConfig (Prelude.Maybe Prelude.Natural)
functionEventInvokeConfig_maximumRetryAttempts = Lens.lens (\FunctionEventInvokeConfig' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@FunctionEventInvokeConfig' {} a -> s {maximumRetryAttempts = a} :: FunctionEventInvokeConfig)

instance Data.FromJSON FunctionEventInvokeConfig where
  parseJSON =
    Data.withObject
      "FunctionEventInvokeConfig"
      ( \x ->
          FunctionEventInvokeConfig'
            Prelude.<$> (x Data..:? "DestinationConfig")
            Prelude.<*> (x Data..:? "FunctionArn")
            Prelude.<*> (x Data..:? "LastModified")
            Prelude.<*> (x Data..:? "MaximumEventAgeInSeconds")
            Prelude.<*> (x Data..:? "MaximumRetryAttempts")
      )

instance Prelude.Hashable FunctionEventInvokeConfig where
  hashWithSalt _salt FunctionEventInvokeConfig' {..} =
    _salt
      `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` functionArn
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` maximumEventAgeInSeconds
      `Prelude.hashWithSalt` maximumRetryAttempts

instance Prelude.NFData FunctionEventInvokeConfig where
  rnf FunctionEventInvokeConfig' {..} =
    Prelude.rnf destinationConfig `Prelude.seq`
      Prelude.rnf functionArn `Prelude.seq`
        Prelude.rnf lastModified `Prelude.seq`
          Prelude.rnf maximumEventAgeInSeconds `Prelude.seq`
            Prelude.rnf maximumRetryAttempts
