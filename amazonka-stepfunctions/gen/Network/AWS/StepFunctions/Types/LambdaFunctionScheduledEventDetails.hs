{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.LambdaFunctionScheduledEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about a lambda function scheduled during an execution.
--
-- /See:/ 'newLambdaFunctionScheduledEventDetails' smart constructor.
data LambdaFunctionScheduledEventDetails = LambdaFunctionScheduledEventDetails'
  { -- | Contains details about input for an execution history event.
    inputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The JSON data input to the lambda function. Length constraints apply to
    -- the payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The maximum allowed duration of the lambda function.
    timeoutInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the scheduled lambda function.
    resource :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionScheduledEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDetails', 'lambdaFunctionScheduledEventDetails_inputDetails' - Contains details about input for an execution history event.
--
-- 'input', 'lambdaFunctionScheduledEventDetails_input' - The JSON data input to the lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'timeoutInSeconds', 'lambdaFunctionScheduledEventDetails_timeoutInSeconds' - The maximum allowed duration of the lambda function.
--
-- 'resource', 'lambdaFunctionScheduledEventDetails_resource' - The Amazon Resource Name (ARN) of the scheduled lambda function.
newLambdaFunctionScheduledEventDetails ::
  -- | 'resource'
  Prelude.Text ->
  LambdaFunctionScheduledEventDetails
newLambdaFunctionScheduledEventDetails pResource_ =
  LambdaFunctionScheduledEventDetails'
    { inputDetails =
        Prelude.Nothing,
      input = Prelude.Nothing,
      timeoutInSeconds = Prelude.Nothing,
      resource = pResource_
    }

-- | Contains details about input for an execution history event.
lambdaFunctionScheduledEventDetails_inputDetails :: Lens.Lens' LambdaFunctionScheduledEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
lambdaFunctionScheduledEventDetails_inputDetails = Lens.lens (\LambdaFunctionScheduledEventDetails' {inputDetails} -> inputDetails) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {inputDetails = a} :: LambdaFunctionScheduledEventDetails)

-- | The JSON data input to the lambda function. Length constraints apply to
-- the payload size, and are expressed as bytes in UTF-8 encoding.
lambdaFunctionScheduledEventDetails_input :: Lens.Lens' LambdaFunctionScheduledEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionScheduledEventDetails_input = Lens.lens (\LambdaFunctionScheduledEventDetails' {input} -> input) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {input = a} :: LambdaFunctionScheduledEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The maximum allowed duration of the lambda function.
lambdaFunctionScheduledEventDetails_timeoutInSeconds :: Lens.Lens' LambdaFunctionScheduledEventDetails (Prelude.Maybe Prelude.Integer)
lambdaFunctionScheduledEventDetails_timeoutInSeconds = Lens.lens (\LambdaFunctionScheduledEventDetails' {timeoutInSeconds} -> timeoutInSeconds) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {timeoutInSeconds = a} :: LambdaFunctionScheduledEventDetails)

-- | The Amazon Resource Name (ARN) of the scheduled lambda function.
lambdaFunctionScheduledEventDetails_resource :: Lens.Lens' LambdaFunctionScheduledEventDetails Prelude.Text
lambdaFunctionScheduledEventDetails_resource = Lens.lens (\LambdaFunctionScheduledEventDetails' {resource} -> resource) (\s@LambdaFunctionScheduledEventDetails' {} a -> s {resource = a} :: LambdaFunctionScheduledEventDetails)

instance
  Prelude.FromJSON
    LambdaFunctionScheduledEventDetails
  where
  parseJSON =
    Prelude.withObject
      "LambdaFunctionScheduledEventDetails"
      ( \x ->
          LambdaFunctionScheduledEventDetails'
            Prelude.<$> (x Prelude..:? "inputDetails")
            Prelude.<*> (x Prelude..:? "input")
            Prelude.<*> (x Prelude..:? "timeoutInSeconds")
            Prelude.<*> (x Prelude..: "resource")
      )

instance
  Prelude.Hashable
    LambdaFunctionScheduledEventDetails

instance
  Prelude.NFData
    LambdaFunctionScheduledEventDetails
