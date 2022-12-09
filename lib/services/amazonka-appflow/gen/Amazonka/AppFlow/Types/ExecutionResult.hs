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
-- Module      : Amazonka.AppFlow.Types.ExecutionResult
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ExecutionResult where

import Amazonka.AppFlow.Types.ErrorInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the end result of the flow run.
--
-- /See:/ 'newExecutionResult' smart constructor.
data ExecutionResult = ExecutionResult'
  { -- | The total number of bytes processed by the flow run.
    bytesProcessed :: Prelude.Maybe Prelude.Integer,
    -- | The total number of bytes written as a result of the flow run.
    bytesWritten :: Prelude.Maybe Prelude.Integer,
    -- | Provides any error message information related to the flow run.
    errorInfo :: Prelude.Maybe ErrorInfo,
    -- | The number of records processed in the flow run.
    recordsProcessed :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesProcessed', 'executionResult_bytesProcessed' - The total number of bytes processed by the flow run.
--
-- 'bytesWritten', 'executionResult_bytesWritten' - The total number of bytes written as a result of the flow run.
--
-- 'errorInfo', 'executionResult_errorInfo' - Provides any error message information related to the flow run.
--
-- 'recordsProcessed', 'executionResult_recordsProcessed' - The number of records processed in the flow run.
newExecutionResult ::
  ExecutionResult
newExecutionResult =
  ExecutionResult'
    { bytesProcessed = Prelude.Nothing,
      bytesWritten = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      recordsProcessed = Prelude.Nothing
    }

-- | The total number of bytes processed by the flow run.
executionResult_bytesProcessed :: Lens.Lens' ExecutionResult (Prelude.Maybe Prelude.Integer)
executionResult_bytesProcessed = Lens.lens (\ExecutionResult' {bytesProcessed} -> bytesProcessed) (\s@ExecutionResult' {} a -> s {bytesProcessed = a} :: ExecutionResult)

-- | The total number of bytes written as a result of the flow run.
executionResult_bytesWritten :: Lens.Lens' ExecutionResult (Prelude.Maybe Prelude.Integer)
executionResult_bytesWritten = Lens.lens (\ExecutionResult' {bytesWritten} -> bytesWritten) (\s@ExecutionResult' {} a -> s {bytesWritten = a} :: ExecutionResult)

-- | Provides any error message information related to the flow run.
executionResult_errorInfo :: Lens.Lens' ExecutionResult (Prelude.Maybe ErrorInfo)
executionResult_errorInfo = Lens.lens (\ExecutionResult' {errorInfo} -> errorInfo) (\s@ExecutionResult' {} a -> s {errorInfo = a} :: ExecutionResult)

-- | The number of records processed in the flow run.
executionResult_recordsProcessed :: Lens.Lens' ExecutionResult (Prelude.Maybe Prelude.Integer)
executionResult_recordsProcessed = Lens.lens (\ExecutionResult' {recordsProcessed} -> recordsProcessed) (\s@ExecutionResult' {} a -> s {recordsProcessed = a} :: ExecutionResult)

instance Data.FromJSON ExecutionResult where
  parseJSON =
    Data.withObject
      "ExecutionResult"
      ( \x ->
          ExecutionResult'
            Prelude.<$> (x Data..:? "bytesProcessed")
            Prelude.<*> (x Data..:? "bytesWritten")
            Prelude.<*> (x Data..:? "errorInfo")
            Prelude.<*> (x Data..:? "recordsProcessed")
      )

instance Prelude.Hashable ExecutionResult where
  hashWithSalt _salt ExecutionResult' {..} =
    _salt `Prelude.hashWithSalt` bytesProcessed
      `Prelude.hashWithSalt` bytesWritten
      `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` recordsProcessed

instance Prelude.NFData ExecutionResult where
  rnf ExecutionResult' {..} =
    Prelude.rnf bytesProcessed
      `Prelude.seq` Prelude.rnf bytesWritten
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf recordsProcessed
