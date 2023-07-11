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
-- Module      : Amazonka.GreengrassV2.Types.LambdaExecutionParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.LambdaExecutionParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types.LambdaEventSource
import Amazonka.GreengrassV2.Types.LambdaInputPayloadEncodingType
import Amazonka.GreengrassV2.Types.LambdaLinuxProcessParams
import qualified Amazonka.Prelude as Prelude

-- | Contains parameters for a Lambda function that runs on IoT Greengrass.
--
-- /See:/ 'newLambdaExecutionParameters' smart constructor.
data LambdaExecutionParameters = LambdaExecutionParameters'
  { -- | The map of environment variables that are available to the Lambda
    -- function when it runs.
    environmentVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The list of event sources to which to subscribe to receive work
    -- messages. The Lambda function runs when it receives a message from an
    -- event source. You can subscribe this function to local
    -- publish\/subscribe messages and Amazon Web Services IoT Core MQTT
    -- messages.
    eventSources :: Prelude.Maybe [LambdaEventSource],
    -- | The list of arguments to pass to the Lambda function when it runs.
    execArgs :: Prelude.Maybe [Prelude.Text],
    -- | The encoding type that the Lambda function supports.
    --
    -- Default: @json@
    inputPayloadEncodingType :: Prelude.Maybe LambdaInputPayloadEncodingType,
    -- | The parameters for the Linux process that contains the Lambda function.
    linuxProcessParams :: Prelude.Maybe LambdaLinuxProcessParams,
    -- | The maximum amount of time in seconds that a non-pinned Lambda function
    -- can idle before the IoT Greengrass Core software stops its process.
    maxIdleTimeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of instances that a non-pinned Lambda function can
    -- run at the same time.
    maxInstancesCount :: Prelude.Maybe Prelude.Int,
    -- | The maximum size of the message queue for the Lambda function component.
    -- The IoT Greengrass core stores messages in a FIFO (first-in-first-out)
    -- queue until it can run the Lambda function to consume each message.
    maxQueueSize :: Prelude.Maybe Prelude.Int,
    -- | Whether or not the Lambda function is pinned, or long-lived.
    --
    -- -   A pinned Lambda function starts when IoT Greengrass starts and keeps
    --     running in its own container.
    --
    -- -   A non-pinned Lambda function starts only when it receives a work
    --     item and exists after it idles for @maxIdleTimeInSeconds@. If the
    --     function has multiple work items, the IoT Greengrass Core software
    --     creates multiple instances of the function.
    --
    -- Default: @true@
    pinned :: Prelude.Maybe Prelude.Bool,
    -- | The interval in seconds at which a pinned (also known as long-lived)
    -- Lambda function component sends status updates to the Lambda manager
    -- component.
    statusTimeoutInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The maximum amount of time in seconds that the Lambda function can
    -- process a work item.
    timeoutInSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaExecutionParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentVariables', 'lambdaExecutionParameters_environmentVariables' - The map of environment variables that are available to the Lambda
-- function when it runs.
--
-- 'eventSources', 'lambdaExecutionParameters_eventSources' - The list of event sources to which to subscribe to receive work
-- messages. The Lambda function runs when it receives a message from an
-- event source. You can subscribe this function to local
-- publish\/subscribe messages and Amazon Web Services IoT Core MQTT
-- messages.
--
-- 'execArgs', 'lambdaExecutionParameters_execArgs' - The list of arguments to pass to the Lambda function when it runs.
--
-- 'inputPayloadEncodingType', 'lambdaExecutionParameters_inputPayloadEncodingType' - The encoding type that the Lambda function supports.
--
-- Default: @json@
--
-- 'linuxProcessParams', 'lambdaExecutionParameters_linuxProcessParams' - The parameters for the Linux process that contains the Lambda function.
--
-- 'maxIdleTimeInSeconds', 'lambdaExecutionParameters_maxIdleTimeInSeconds' - The maximum amount of time in seconds that a non-pinned Lambda function
-- can idle before the IoT Greengrass Core software stops its process.
--
-- 'maxInstancesCount', 'lambdaExecutionParameters_maxInstancesCount' - The maximum number of instances that a non-pinned Lambda function can
-- run at the same time.
--
-- 'maxQueueSize', 'lambdaExecutionParameters_maxQueueSize' - The maximum size of the message queue for the Lambda function component.
-- The IoT Greengrass core stores messages in a FIFO (first-in-first-out)
-- queue until it can run the Lambda function to consume each message.
--
-- 'pinned', 'lambdaExecutionParameters_pinned' - Whether or not the Lambda function is pinned, or long-lived.
--
-- -   A pinned Lambda function starts when IoT Greengrass starts and keeps
--     running in its own container.
--
-- -   A non-pinned Lambda function starts only when it receives a work
--     item and exists after it idles for @maxIdleTimeInSeconds@. If the
--     function has multiple work items, the IoT Greengrass Core software
--     creates multiple instances of the function.
--
-- Default: @true@
--
-- 'statusTimeoutInSeconds', 'lambdaExecutionParameters_statusTimeoutInSeconds' - The interval in seconds at which a pinned (also known as long-lived)
-- Lambda function component sends status updates to the Lambda manager
-- component.
--
-- 'timeoutInSeconds', 'lambdaExecutionParameters_timeoutInSeconds' - The maximum amount of time in seconds that the Lambda function can
-- process a work item.
newLambdaExecutionParameters ::
  LambdaExecutionParameters
newLambdaExecutionParameters =
  LambdaExecutionParameters'
    { environmentVariables =
        Prelude.Nothing,
      eventSources = Prelude.Nothing,
      execArgs = Prelude.Nothing,
      inputPayloadEncodingType = Prelude.Nothing,
      linuxProcessParams = Prelude.Nothing,
      maxIdleTimeInSeconds = Prelude.Nothing,
      maxInstancesCount = Prelude.Nothing,
      maxQueueSize = Prelude.Nothing,
      pinned = Prelude.Nothing,
      statusTimeoutInSeconds = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing
    }

-- | The map of environment variables that are available to the Lambda
-- function when it runs.
lambdaExecutionParameters_environmentVariables :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
lambdaExecutionParameters_environmentVariables = Lens.lens (\LambdaExecutionParameters' {environmentVariables} -> environmentVariables) (\s@LambdaExecutionParameters' {} a -> s {environmentVariables = a} :: LambdaExecutionParameters) Prelude.. Lens.mapping Lens.coerced

-- | The list of event sources to which to subscribe to receive work
-- messages. The Lambda function runs when it receives a message from an
-- event source. You can subscribe this function to local
-- publish\/subscribe messages and Amazon Web Services IoT Core MQTT
-- messages.
lambdaExecutionParameters_eventSources :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe [LambdaEventSource])
lambdaExecutionParameters_eventSources = Lens.lens (\LambdaExecutionParameters' {eventSources} -> eventSources) (\s@LambdaExecutionParameters' {} a -> s {eventSources = a} :: LambdaExecutionParameters) Prelude.. Lens.mapping Lens.coerced

-- | The list of arguments to pass to the Lambda function when it runs.
lambdaExecutionParameters_execArgs :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe [Prelude.Text])
lambdaExecutionParameters_execArgs = Lens.lens (\LambdaExecutionParameters' {execArgs} -> execArgs) (\s@LambdaExecutionParameters' {} a -> s {execArgs = a} :: LambdaExecutionParameters) Prelude.. Lens.mapping Lens.coerced

-- | The encoding type that the Lambda function supports.
--
-- Default: @json@
lambdaExecutionParameters_inputPayloadEncodingType :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe LambdaInputPayloadEncodingType)
lambdaExecutionParameters_inputPayloadEncodingType = Lens.lens (\LambdaExecutionParameters' {inputPayloadEncodingType} -> inputPayloadEncodingType) (\s@LambdaExecutionParameters' {} a -> s {inputPayloadEncodingType = a} :: LambdaExecutionParameters)

-- | The parameters for the Linux process that contains the Lambda function.
lambdaExecutionParameters_linuxProcessParams :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe LambdaLinuxProcessParams)
lambdaExecutionParameters_linuxProcessParams = Lens.lens (\LambdaExecutionParameters' {linuxProcessParams} -> linuxProcessParams) (\s@LambdaExecutionParameters' {} a -> s {linuxProcessParams = a} :: LambdaExecutionParameters)

-- | The maximum amount of time in seconds that a non-pinned Lambda function
-- can idle before the IoT Greengrass Core software stops its process.
lambdaExecutionParameters_maxIdleTimeInSeconds :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe Prelude.Int)
lambdaExecutionParameters_maxIdleTimeInSeconds = Lens.lens (\LambdaExecutionParameters' {maxIdleTimeInSeconds} -> maxIdleTimeInSeconds) (\s@LambdaExecutionParameters' {} a -> s {maxIdleTimeInSeconds = a} :: LambdaExecutionParameters)

-- | The maximum number of instances that a non-pinned Lambda function can
-- run at the same time.
lambdaExecutionParameters_maxInstancesCount :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe Prelude.Int)
lambdaExecutionParameters_maxInstancesCount = Lens.lens (\LambdaExecutionParameters' {maxInstancesCount} -> maxInstancesCount) (\s@LambdaExecutionParameters' {} a -> s {maxInstancesCount = a} :: LambdaExecutionParameters)

-- | The maximum size of the message queue for the Lambda function component.
-- The IoT Greengrass core stores messages in a FIFO (first-in-first-out)
-- queue until it can run the Lambda function to consume each message.
lambdaExecutionParameters_maxQueueSize :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe Prelude.Int)
lambdaExecutionParameters_maxQueueSize = Lens.lens (\LambdaExecutionParameters' {maxQueueSize} -> maxQueueSize) (\s@LambdaExecutionParameters' {} a -> s {maxQueueSize = a} :: LambdaExecutionParameters)

-- | Whether or not the Lambda function is pinned, or long-lived.
--
-- -   A pinned Lambda function starts when IoT Greengrass starts and keeps
--     running in its own container.
--
-- -   A non-pinned Lambda function starts only when it receives a work
--     item and exists after it idles for @maxIdleTimeInSeconds@. If the
--     function has multiple work items, the IoT Greengrass Core software
--     creates multiple instances of the function.
--
-- Default: @true@
lambdaExecutionParameters_pinned :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe Prelude.Bool)
lambdaExecutionParameters_pinned = Lens.lens (\LambdaExecutionParameters' {pinned} -> pinned) (\s@LambdaExecutionParameters' {} a -> s {pinned = a} :: LambdaExecutionParameters)

-- | The interval in seconds at which a pinned (also known as long-lived)
-- Lambda function component sends status updates to the Lambda manager
-- component.
lambdaExecutionParameters_statusTimeoutInSeconds :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe Prelude.Int)
lambdaExecutionParameters_statusTimeoutInSeconds = Lens.lens (\LambdaExecutionParameters' {statusTimeoutInSeconds} -> statusTimeoutInSeconds) (\s@LambdaExecutionParameters' {} a -> s {statusTimeoutInSeconds = a} :: LambdaExecutionParameters)

-- | The maximum amount of time in seconds that the Lambda function can
-- process a work item.
lambdaExecutionParameters_timeoutInSeconds :: Lens.Lens' LambdaExecutionParameters (Prelude.Maybe Prelude.Int)
lambdaExecutionParameters_timeoutInSeconds = Lens.lens (\LambdaExecutionParameters' {timeoutInSeconds} -> timeoutInSeconds) (\s@LambdaExecutionParameters' {} a -> s {timeoutInSeconds = a} :: LambdaExecutionParameters)

instance Prelude.Hashable LambdaExecutionParameters where
  hashWithSalt _salt LambdaExecutionParameters' {..} =
    _salt
      `Prelude.hashWithSalt` environmentVariables
      `Prelude.hashWithSalt` eventSources
      `Prelude.hashWithSalt` execArgs
      `Prelude.hashWithSalt` inputPayloadEncodingType
      `Prelude.hashWithSalt` linuxProcessParams
      `Prelude.hashWithSalt` maxIdleTimeInSeconds
      `Prelude.hashWithSalt` maxInstancesCount
      `Prelude.hashWithSalt` maxQueueSize
      `Prelude.hashWithSalt` pinned
      `Prelude.hashWithSalt` statusTimeoutInSeconds
      `Prelude.hashWithSalt` timeoutInSeconds

instance Prelude.NFData LambdaExecutionParameters where
  rnf LambdaExecutionParameters' {..} =
    Prelude.rnf environmentVariables
      `Prelude.seq` Prelude.rnf eventSources
      `Prelude.seq` Prelude.rnf execArgs
      `Prelude.seq` Prelude.rnf inputPayloadEncodingType
      `Prelude.seq` Prelude.rnf linuxProcessParams
      `Prelude.seq` Prelude.rnf maxIdleTimeInSeconds
      `Prelude.seq` Prelude.rnf maxInstancesCount
      `Prelude.seq` Prelude.rnf maxQueueSize
      `Prelude.seq` Prelude.rnf pinned
      `Prelude.seq` Prelude.rnf statusTimeoutInSeconds
      `Prelude.seq` Prelude.rnf timeoutInSeconds

instance Data.ToJSON LambdaExecutionParameters where
  toJSON LambdaExecutionParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("environmentVariables" Data..=)
              Prelude.<$> environmentVariables,
            ("eventSources" Data..=) Prelude.<$> eventSources,
            ("execArgs" Data..=) Prelude.<$> execArgs,
            ("inputPayloadEncodingType" Data..=)
              Prelude.<$> inputPayloadEncodingType,
            ("linuxProcessParams" Data..=)
              Prelude.<$> linuxProcessParams,
            ("maxIdleTimeInSeconds" Data..=)
              Prelude.<$> maxIdleTimeInSeconds,
            ("maxInstancesCount" Data..=)
              Prelude.<$> maxInstancesCount,
            ("maxQueueSize" Data..=) Prelude.<$> maxQueueSize,
            ("pinned" Data..=) Prelude.<$> pinned,
            ("statusTimeoutInSeconds" Data..=)
              Prelude.<$> statusTimeoutInSeconds,
            ("timeoutInSeconds" Data..=)
              Prelude.<$> timeoutInSeconds
          ]
      )
