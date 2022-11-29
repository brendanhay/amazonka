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
-- Module      : Amazonka.StepFunctions.Types.ExecutionFailedEventDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ExecutionFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an execution failure event.
--
-- /See:/ 'newExecutionFailedEventDetails' smart constructor.
data ExecutionFailedEventDetails = ExecutionFailedEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'executionFailedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'executionFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
newExecutionFailedEventDetails ::
  ExecutionFailedEventDetails
newExecutionFailedEventDetails =
  ExecutionFailedEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
executionFailedEventDetails_error :: Lens.Lens' ExecutionFailedEventDetails (Prelude.Maybe Prelude.Text)
executionFailedEventDetails_error = Lens.lens (\ExecutionFailedEventDetails' {error} -> error) (\s@ExecutionFailedEventDetails' {} a -> s {error = a} :: ExecutionFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
executionFailedEventDetails_cause :: Lens.Lens' ExecutionFailedEventDetails (Prelude.Maybe Prelude.Text)
executionFailedEventDetails_cause = Lens.lens (\ExecutionFailedEventDetails' {cause} -> cause) (\s@ExecutionFailedEventDetails' {} a -> s {cause = a} :: ExecutionFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON ExecutionFailedEventDetails where
  parseJSON =
    Core.withObject
      "ExecutionFailedEventDetails"
      ( \x ->
          ExecutionFailedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance Prelude.Hashable ExecutionFailedEventDetails where
  hashWithSalt _salt ExecutionFailedEventDetails' {..} =
    _salt `Prelude.hashWithSalt` error
      `Prelude.hashWithSalt` cause

instance Prelude.NFData ExecutionFailedEventDetails where
  rnf ExecutionFailedEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause
