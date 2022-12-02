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
-- Module      : Amazonka.StepFunctions.Types.ExecutionStartedEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ExecutionStartedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the start of the execution.
--
-- /See:/ 'newExecutionStartedEventDetails' smart constructor.
data ExecutionStartedEventDetails = ExecutionStartedEventDetails'
  { -- | The Amazon Resource Name (ARN) of the IAM role used for executing Lambda
    -- tasks.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Contains details about the input for an execution history event.
    inputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails,
    -- | The JSON data input to the execution. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    input :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionStartedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'executionStartedEventDetails_roleArn' - The Amazon Resource Name (ARN) of the IAM role used for executing Lambda
-- tasks.
--
-- 'inputDetails', 'executionStartedEventDetails_inputDetails' - Contains details about the input for an execution history event.
--
-- 'input', 'executionStartedEventDetails_input' - The JSON data input to the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
newExecutionStartedEventDetails ::
  ExecutionStartedEventDetails
newExecutionStartedEventDetails =
  ExecutionStartedEventDetails'
    { roleArn =
        Prelude.Nothing,
      inputDetails = Prelude.Nothing,
      input = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role used for executing Lambda
-- tasks.
executionStartedEventDetails_roleArn :: Lens.Lens' ExecutionStartedEventDetails (Prelude.Maybe Prelude.Text)
executionStartedEventDetails_roleArn = Lens.lens (\ExecutionStartedEventDetails' {roleArn} -> roleArn) (\s@ExecutionStartedEventDetails' {} a -> s {roleArn = a} :: ExecutionStartedEventDetails)

-- | Contains details about the input for an execution history event.
executionStartedEventDetails_inputDetails :: Lens.Lens' ExecutionStartedEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
executionStartedEventDetails_inputDetails = Lens.lens (\ExecutionStartedEventDetails' {inputDetails} -> inputDetails) (\s@ExecutionStartedEventDetails' {} a -> s {inputDetails = a} :: ExecutionStartedEventDetails)

-- | The JSON data input to the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
executionStartedEventDetails_input :: Lens.Lens' ExecutionStartedEventDetails (Prelude.Maybe Prelude.Text)
executionStartedEventDetails_input = Lens.lens (\ExecutionStartedEventDetails' {input} -> input) (\s@ExecutionStartedEventDetails' {} a -> s {input = a} :: ExecutionStartedEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ExecutionStartedEventDetails where
  parseJSON =
    Data.withObject
      "ExecutionStartedEventDetails"
      ( \x ->
          ExecutionStartedEventDetails'
            Prelude.<$> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "inputDetails")
            Prelude.<*> (x Data..:? "input")
      )

instance
  Prelude.Hashable
    ExecutionStartedEventDetails
  where
  hashWithSalt _salt ExecutionStartedEventDetails' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` inputDetails
      `Prelude.hashWithSalt` input

instance Prelude.NFData ExecutionStartedEventDetails where
  rnf ExecutionStartedEventDetails' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf inputDetails
      `Prelude.seq` Prelude.rnf input
