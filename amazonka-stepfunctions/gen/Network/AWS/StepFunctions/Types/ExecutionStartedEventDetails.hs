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
-- Module      : Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionStartedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the start of the execution.
--
-- /See:/ 'newExecutionStartedEventDetails' smart constructor.
data ExecutionStartedEventDetails = ExecutionStartedEventDetails'
  { -- | Contains details about the input for an execution history event.
    inputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS
    -- Lambda tasks.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON data input to the execution. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExecutionStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDetails', 'executionStartedEventDetails_inputDetails' - Contains details about the input for an execution history event.
--
-- 'roleArn', 'executionStartedEventDetails_roleArn' - The Amazon Resource Name (ARN) of the IAM role used for executing AWS
-- Lambda tasks.
--
-- 'input', 'executionStartedEventDetails_input' - The JSON data input to the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
newExecutionStartedEventDetails ::
  ExecutionStartedEventDetails
newExecutionStartedEventDetails =
  ExecutionStartedEventDetails'
    { inputDetails =
        Prelude.Nothing,
      roleArn = Prelude.Nothing,
      input = Prelude.Nothing
    }

-- | Contains details about the input for an execution history event.
executionStartedEventDetails_inputDetails :: Lens.Lens' ExecutionStartedEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
executionStartedEventDetails_inputDetails = Lens.lens (\ExecutionStartedEventDetails' {inputDetails} -> inputDetails) (\s@ExecutionStartedEventDetails' {} a -> s {inputDetails = a} :: ExecutionStartedEventDetails)

-- | The Amazon Resource Name (ARN) of the IAM role used for executing AWS
-- Lambda tasks.
executionStartedEventDetails_roleArn :: Lens.Lens' ExecutionStartedEventDetails (Prelude.Maybe Prelude.Text)
executionStartedEventDetails_roleArn = Lens.lens (\ExecutionStartedEventDetails' {roleArn} -> roleArn) (\s@ExecutionStartedEventDetails' {} a -> s {roleArn = a} :: ExecutionStartedEventDetails)

-- | The JSON data input to the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
executionStartedEventDetails_input :: Lens.Lens' ExecutionStartedEventDetails (Prelude.Maybe Prelude.Text)
executionStartedEventDetails_input = Lens.lens (\ExecutionStartedEventDetails' {input} -> input) (\s@ExecutionStartedEventDetails' {} a -> s {input = a} :: ExecutionStartedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

instance
  Prelude.FromJSON
    ExecutionStartedEventDetails
  where
  parseJSON =
    Prelude.withObject
      "ExecutionStartedEventDetails"
      ( \x ->
          ExecutionStartedEventDetails'
            Prelude.<$> (x Prelude..:? "inputDetails")
            Prelude.<*> (x Prelude..:? "roleArn")
            Prelude.<*> (x Prelude..:? "input")
      )

instance
  Prelude.Hashable
    ExecutionStartedEventDetails

instance Prelude.NFData ExecutionStartedEventDetails
