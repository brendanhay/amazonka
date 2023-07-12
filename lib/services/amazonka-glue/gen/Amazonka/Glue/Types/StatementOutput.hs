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
-- Module      : Amazonka.Glue.Types.StatementOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StatementOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.StatementOutputData
import Amazonka.Glue.Types.StatementState
import qualified Amazonka.Prelude as Prelude

-- | The code execution output in JSON format.
--
-- /See:/ 'newStatementOutput' smart constructor.
data StatementOutput = StatementOutput'
  { -- | The code execution output.
    data' :: Prelude.Maybe StatementOutputData,
    -- | The name of the error in the output.
    errorName :: Prelude.Maybe Prelude.Text,
    -- | The error value of the output.
    errorValue :: Prelude.Maybe Prelude.Text,
    -- | The execution count of the output.
    executionCount :: Prelude.Maybe Prelude.Int,
    -- | The status of the code execution output.
    status :: Prelude.Maybe StatementState,
    -- | The traceback of the output.
    traceback :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatementOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'statementOutput_data' - The code execution output.
--
-- 'errorName', 'statementOutput_errorName' - The name of the error in the output.
--
-- 'errorValue', 'statementOutput_errorValue' - The error value of the output.
--
-- 'executionCount', 'statementOutput_executionCount' - The execution count of the output.
--
-- 'status', 'statementOutput_status' - The status of the code execution output.
--
-- 'traceback', 'statementOutput_traceback' - The traceback of the output.
newStatementOutput ::
  StatementOutput
newStatementOutput =
  StatementOutput'
    { data' = Prelude.Nothing,
      errorName = Prelude.Nothing,
      errorValue = Prelude.Nothing,
      executionCount = Prelude.Nothing,
      status = Prelude.Nothing,
      traceback = Prelude.Nothing
    }

-- | The code execution output.
statementOutput_data :: Lens.Lens' StatementOutput (Prelude.Maybe StatementOutputData)
statementOutput_data = Lens.lens (\StatementOutput' {data'} -> data') (\s@StatementOutput' {} a -> s {data' = a} :: StatementOutput)

-- | The name of the error in the output.
statementOutput_errorName :: Lens.Lens' StatementOutput (Prelude.Maybe Prelude.Text)
statementOutput_errorName = Lens.lens (\StatementOutput' {errorName} -> errorName) (\s@StatementOutput' {} a -> s {errorName = a} :: StatementOutput)

-- | The error value of the output.
statementOutput_errorValue :: Lens.Lens' StatementOutput (Prelude.Maybe Prelude.Text)
statementOutput_errorValue = Lens.lens (\StatementOutput' {errorValue} -> errorValue) (\s@StatementOutput' {} a -> s {errorValue = a} :: StatementOutput)

-- | The execution count of the output.
statementOutput_executionCount :: Lens.Lens' StatementOutput (Prelude.Maybe Prelude.Int)
statementOutput_executionCount = Lens.lens (\StatementOutput' {executionCount} -> executionCount) (\s@StatementOutput' {} a -> s {executionCount = a} :: StatementOutput)

-- | The status of the code execution output.
statementOutput_status :: Lens.Lens' StatementOutput (Prelude.Maybe StatementState)
statementOutput_status = Lens.lens (\StatementOutput' {status} -> status) (\s@StatementOutput' {} a -> s {status = a} :: StatementOutput)

-- | The traceback of the output.
statementOutput_traceback :: Lens.Lens' StatementOutput (Prelude.Maybe [Prelude.Text])
statementOutput_traceback = Lens.lens (\StatementOutput' {traceback} -> traceback) (\s@StatementOutput' {} a -> s {traceback = a} :: StatementOutput) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON StatementOutput where
  parseJSON =
    Data.withObject
      "StatementOutput"
      ( \x ->
          StatementOutput'
            Prelude.<$> (x Data..:? "Data")
            Prelude.<*> (x Data..:? "ErrorName")
            Prelude.<*> (x Data..:? "ErrorValue")
            Prelude.<*> (x Data..:? "ExecutionCount")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Traceback" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StatementOutput where
  hashWithSalt _salt StatementOutput' {..} =
    _salt
      `Prelude.hashWithSalt` data'
      `Prelude.hashWithSalt` errorName
      `Prelude.hashWithSalt` errorValue
      `Prelude.hashWithSalt` executionCount
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` traceback

instance Prelude.NFData StatementOutput where
  rnf StatementOutput' {..} =
    Prelude.rnf data'
      `Prelude.seq` Prelude.rnf errorName
      `Prelude.seq` Prelude.rnf errorValue
      `Prelude.seq` Prelude.rnf executionCount
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf traceback
