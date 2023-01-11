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
-- Module      : Amazonka.StepFunctions.Types.ExecutionTimedOutEventDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ExecutionTimedOutEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the execution timeout that occurred during the
-- execution.
--
-- /See:/ 'newExecutionTimedOutEventDetails' smart constructor.
data ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails'
  { -- | A more detailed explanation of the cause of the timeout.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'executionTimedOutEventDetails_cause' - A more detailed explanation of the cause of the timeout.
--
-- 'error', 'executionTimedOutEventDetails_error' - The error code of the failure.
newExecutionTimedOutEventDetails ::
  ExecutionTimedOutEventDetails
newExecutionTimedOutEventDetails =
  ExecutionTimedOutEventDetails'
    { cause =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the timeout.
executionTimedOutEventDetails_cause :: Lens.Lens' ExecutionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
executionTimedOutEventDetails_cause = Lens.lens (\ExecutionTimedOutEventDetails' {cause} -> cause) (\s@ExecutionTimedOutEventDetails' {} a -> s {cause = a} :: ExecutionTimedOutEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the failure.
executionTimedOutEventDetails_error :: Lens.Lens' ExecutionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
executionTimedOutEventDetails_error = Lens.lens (\ExecutionTimedOutEventDetails' {error} -> error) (\s@ExecutionTimedOutEventDetails' {} a -> s {error = a} :: ExecutionTimedOutEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ExecutionTimedOutEventDetails where
  parseJSON =
    Data.withObject
      "ExecutionTimedOutEventDetails"
      ( \x ->
          ExecutionTimedOutEventDetails'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "error")
      )

instance
  Prelude.Hashable
    ExecutionTimedOutEventDetails
  where
  hashWithSalt _salt ExecutionTimedOutEventDetails' {..} =
    _salt `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error

instance Prelude.NFData ExecutionTimedOutEventDetails where
  rnf ExecutionTimedOutEventDetails' {..} =
    Prelude.rnf cause `Prelude.seq` Prelude.rnf error
