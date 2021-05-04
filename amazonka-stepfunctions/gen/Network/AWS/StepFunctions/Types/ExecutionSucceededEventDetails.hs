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
-- Module      : Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionSucceededEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails

-- | Contains details about the successful termination of the execution.
--
-- /See:/ 'newExecutionSucceededEventDetails' smart constructor.
data ExecutionSucceededEventDetails = ExecutionSucceededEventDetails'
  { -- | The JSON data output by the execution. Length constraints apply to the
    -- payload size, and are expressed as bytes in UTF-8 encoding.
    output :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Contains details about the output of an execution history event.
    outputDetails :: Prelude.Maybe HistoryEventExecutionDataDetails
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExecutionSucceededEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'output', 'executionSucceededEventDetails_output' - The JSON data output by the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
--
-- 'outputDetails', 'executionSucceededEventDetails_outputDetails' - Contains details about the output of an execution history event.
newExecutionSucceededEventDetails ::
  ExecutionSucceededEventDetails
newExecutionSucceededEventDetails =
  ExecutionSucceededEventDetails'
    { output =
        Prelude.Nothing,
      outputDetails = Prelude.Nothing
    }

-- | The JSON data output by the execution. Length constraints apply to the
-- payload size, and are expressed as bytes in UTF-8 encoding.
executionSucceededEventDetails_output :: Lens.Lens' ExecutionSucceededEventDetails (Prelude.Maybe Prelude.Text)
executionSucceededEventDetails_output = Lens.lens (\ExecutionSucceededEventDetails' {output} -> output) (\s@ExecutionSucceededEventDetails' {} a -> s {output = a} :: ExecutionSucceededEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | Contains details about the output of an execution history event.
executionSucceededEventDetails_outputDetails :: Lens.Lens' ExecutionSucceededEventDetails (Prelude.Maybe HistoryEventExecutionDataDetails)
executionSucceededEventDetails_outputDetails = Lens.lens (\ExecutionSucceededEventDetails' {outputDetails} -> outputDetails) (\s@ExecutionSucceededEventDetails' {} a -> s {outputDetails = a} :: ExecutionSucceededEventDetails)

instance
  Prelude.FromJSON
    ExecutionSucceededEventDetails
  where
  parseJSON =
    Prelude.withObject
      "ExecutionSucceededEventDetails"
      ( \x ->
          ExecutionSucceededEventDetails'
            Prelude.<$> (x Prelude..:? "output")
            Prelude.<*> (x Prelude..:? "outputDetails")
      )

instance
  Prelude.Hashable
    ExecutionSucceededEventDetails

instance
  Prelude.NFData
    ExecutionSucceededEventDetails
