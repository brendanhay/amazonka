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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an execution failure event.
--
-- /See:/ 'newExecutionFailedEventDetails' smart constructor.
data ExecutionFailedEventDetails = ExecutionFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Data.Sensitive Prelude.Text)
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
-- 'cause', 'executionFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'executionFailedEventDetails_error' - The error code of the failure.
newExecutionFailedEventDetails ::
  ExecutionFailedEventDetails
newExecutionFailedEventDetails =
  ExecutionFailedEventDetails'
    { cause =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
executionFailedEventDetails_cause :: Lens.Lens' ExecutionFailedEventDetails (Prelude.Maybe Prelude.Text)
executionFailedEventDetails_cause = Lens.lens (\ExecutionFailedEventDetails' {cause} -> cause) (\s@ExecutionFailedEventDetails' {} a -> s {cause = a} :: ExecutionFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

-- | The error code of the failure.
executionFailedEventDetails_error :: Lens.Lens' ExecutionFailedEventDetails (Prelude.Maybe Prelude.Text)
executionFailedEventDetails_error = Lens.lens (\ExecutionFailedEventDetails' {error} -> error) (\s@ExecutionFailedEventDetails' {} a -> s {error = a} :: ExecutionFailedEventDetails) Prelude.. Lens.mapping Data._Sensitive

instance Data.FromJSON ExecutionFailedEventDetails where
  parseJSON =
    Data.withObject
      "ExecutionFailedEventDetails"
      ( \x ->
          ExecutionFailedEventDetails'
            Prelude.<$> (x Data..:? "cause")
            Prelude.<*> (x Data..:? "error")
      )

instance Prelude.Hashable ExecutionFailedEventDetails where
  hashWithSalt _salt ExecutionFailedEventDetails' {..} =
    _salt `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error

instance Prelude.NFData ExecutionFailedEventDetails where
  rnf ExecutionFailedEventDetails' {..} =
    Prelude.rnf cause `Prelude.seq` Prelude.rnf error
