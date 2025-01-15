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
-- Module      : Amazonka.StepFunctions.Types.LambdaFunctionScheduledEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LambdaFunctionScheduledEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails
import Amazonka.StepFunctions.Types.TaskCredentials

-- | Contains details about a Lambda function scheduled during an execution.
--
-- /See:/ 'newLambdaFunctionScheduledEventDetails' smart constructor.
data LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails'
  { -- | The JSON data input to the Lambda function. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Contains details about input for an execution history event.
    inputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The credentials that Step Functions uses for the task.
    taskCredentials :: Prelude.Maybe TaskCredentials,
    -- | The maximum allowed duration of the Lambda function.
    timeoutInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the scheduled Lambda function.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionScheduledEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'input', 'lambdaFunctionScheduledEventDetails_input' - The JSON data input to the Lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'inputDetails', 'lambdaFunctionScheduledEventDetails_inputDetails' - Contains details about input for an execution history event.
--
-- 'taskCredentials', 'lambdaFunctionScheduledEventDetails_taskCredentials' - The credentials that Step Functions uses for the task.
--
-- 'timeoutInSeconds', 'lambdaFunctionScheduledEventDetails_timeoutInSeconds' - The maximum allowed duration of the Lambda function.
--
-- 'resource', 'lambdaFunctionScheduledEventDetails_resource' - The Amazon Resource Name (ARN) of the scheduled Lambda function.
newLambdaFunctionScheduledEventDetails ::
  -- | 'resource'
  Prelude.Text ->
  LambdaFunctionScheduledEventDetails
newLambdaFunctionScheduledEventDetails pResource_ =
  LambdaFunctionScheduledEventDetails'
    { input =
        Prelude.Nothing,
      inputDetails = Prelude.Nothing,
      taskCredentials = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      resource = pResource_
    }

-- | The JSON data input to the Lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
lambdaFunctionScheduledEventDetails_input :: Lens.Lens' LambdaFunctionScheduledEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduledEventDetails_input = Lens.lens (\LambdaFunctionScheduledEventDetails' {input} -> input) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {input = a} :: LambdaFunctionScheduledEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | Contains details about input for an execution history event.
lambdaFunctionScheduledEventDetails_inputDetails :: Lens.Lens' LambdaFunctionScheduledEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
lambdaFunctionScheduledEventDetails_inputDetails = Lens.lens (\LambdaFunctionScheduledEventDetails' {inputDetails} -> inputDetails) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {inputDetails = a} :: LambdaFunctionScheduledEventDetails)

-- | The credentials that Step Functions uses for the task.
lambdaFunctionScheduledEventDetails_taskCredentials :: Lens.Lens' LambdaFunctionScheduledEventDetails (Prelude.Maybe TaskCredentials)
lambdaFunctionScheduledEventDetails_taskCredentials = Lens.lens (\LambdaFunctionScheduledEventDetails' {taskCredentials} -> taskCredentials) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {taskCredentials = a} :: LambdaFunctionScheduledEventDetails)

-- | The maximum allowed duration of the Lambda function.
lambdaFunctionScheduledEventDetails_timeoutInSeconds :: Lens.Lens' LambdaFunctionScheduledEventDetails (Prelude.Maybe Prelude.Integer)
lambdaFunctionScheduledEventDetails_timeoutInSeconds = Lens.lens (\LambdaFunctionScheduledEventDetails' {timeoutInSeconds} -> timeoutInSeconds) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {timeoutInSeconds = a} :: LambdaFunctionScheduledEventDetails)

-- | The Amazon Resource Name (ARN) of the scheduled Lambda function.
lambdaFunctionScheduledEventDetails_resource :: Lens.Lens' LambdaFunctionScheduledEventDetails Prelude.Text
lambdaFunctionScheduledEventDetails_resource = Lens.lens (\LambdaFunctionScheduledEventDetails' {resource} -> resource) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {resource = a} :: LambdaFunctionScheduledEventDetails)

instance
  Data.FromJSON
    LambdaFunctionScheduledEventDetails
  where
  parseJSON =
    Data.withObject
      "LambdaFunctionScheduledEventDetails"
      ( \x ->
          LambdaFunctionScheduledEventDetails'
            Prelude.<$> (x Data..:? "input")
            Prelude.<*> (x Data..:? "inputDetails")
            Prelude.<*> (x Data..:? "taskCredentials")
            Prelude.<*> (x Data..:? "timeoutInSeconds")
            Prelude.<*> (x Data..: "resource")
      )

instance
  Prelude.Hashable
    LambdaFunctionScheduledEventDetails
  where
  hashWithSalt
    _salt
    LambdaFunctionScheduledEventDetails' {..} =
      _salt
        `Prelude.hashWithSalt` input
        `Prelude.hashWithSalt` inputDetails
        `Prelude.hashWithSalt` taskCredentials
        `Prelude.hashWithSalt` timeoutInSeconds
        `Prelude.hashWithSalt` resource

instance
  Prelude.NFData
    LambdaFunctionScheduledEventDetails
  where
  rnf LambdaFunctionScheduledEventDetails' {..} =
    Prelude.rnf input `Prelude.seq`
      Prelude.rnf inputDetails `Prelude.seq`
        Prelude.rnf taskCredentials `Prelude.seq`
          Prelude.rnf timeoutInSeconds `Prelude.seq`
            Prelude.rnf resource
