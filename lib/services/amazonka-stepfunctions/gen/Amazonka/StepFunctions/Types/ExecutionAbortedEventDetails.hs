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
-- Module      : Amazonka.StepFunctions.Types.ExecutionAbortedEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ExecutionAbortedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an abort of an execution.
--
-- /See:/ 'newExecutionAbortedEventDetails' smart constructor.
data ExecutionAbortedEventDetails = ExecutionAbortedEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionAbortedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'executionAbortedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'executionAbortedEventDetails_cause' - A more detailed explanation of the cause of the failure.
newExecutionAbortedEventDetails ::
  ExecutionAbortedEventDetails
newExecutionAbortedEventDetails =
  ExecutionAbortedEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
executionAbortedEventDetails_error :: Lens.Lens' ExecutionAbortedEventDetails (Prelude.Maybe Prelude.Text)
executionAbortedEventDetails_error = Lens.lens (\ExecutionAbortedEventDetails' {error} -> error) (\s@ExecutionAbortedEventDetails' {} a -> s {error = a} :: ExecutionAbortedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
executionAbortedEventDetails_cause :: Lens.Lens' ExecutionAbortedEventDetails (Prelude.Maybe Prelude.Text)
executionAbortedEventDetails_cause = Lens.lens (\ExecutionAbortedEventDetails' {cause} -> cause) (\s@ExecutionAbortedEventDetails' {} a -> s {cause = a} :: ExecutionAbortedEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON ExecutionAbortedEventDetails where
  parseJSON =
    Core.withObject
      "ExecutionAbortedEventDetails"
      ( \x ->
          ExecutionAbortedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance
  Prelude.Hashable
    ExecutionAbortedEventDetails
  where
  hashWithSalt _salt ExecutionAbortedEventDetails' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` cause

instance Prelude.NFData ExecutionAbortedEventDetails where
  rnf ExecutionAbortedEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause
