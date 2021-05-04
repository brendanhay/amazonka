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
-- Module      : Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionAbortedEventDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains details about an abort of an execution.
--
-- /See:/ 'newExecutionAbortedEventDetails' smart constructor.
data ExecutionAbortedEventDetails = ExecutionAbortedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The error code of the failure.
    error :: Prelude.Maybe (Prelude.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExecutionAbortedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cause', 'executionAbortedEventDetails_cause' - A more detailed explanation of the cause of the failure.
--
-- 'error', 'executionAbortedEventDetails_error' - The error code of the failure.
newExecutionAbortedEventDetails ::
  ExecutionAbortedEventDetails
newExecutionAbortedEventDetails =
  ExecutionAbortedEventDetails'
    { cause =
        Prelude.Nothing,
      error = Prelude.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
executionAbortedEventDetails_cause :: Lens.Lens' ExecutionAbortedEventDetails (Prelude.Maybe Prelude.Text)
executionAbortedEventDetails_cause = Lens.lens (\ExecutionAbortedEventDetails' {cause} -> cause) (\s@ExecutionAbortedEventDetails' {} a -> s {cause = a} :: ExecutionAbortedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

-- | The error code of the failure.
executionAbortedEventDetails_error :: Lens.Lens' ExecutionAbortedEventDetails (Prelude.Maybe Prelude.Text)
executionAbortedEventDetails_error = Lens.lens (\ExecutionAbortedEventDetails' {error} -> error) (\s@ExecutionAbortedEventDetails' {} a -> s {error = a} :: ExecutionAbortedEventDetails) Prelude.. Lens.mapping Prelude._Sensitive

instance
  Prelude.FromJSON
    ExecutionAbortedEventDetails
  where
  parseJSON =
    Prelude.withObject
      "ExecutionAbortedEventDetails"
      ( \x ->
          ExecutionAbortedEventDetails'
            Prelude.<$> (x Prelude..:? "cause")
            Prelude.<*> (x Prelude..:? "error")
      )

instance
  Prelude.Hashable
    ExecutionAbortedEventDetails

instance Prelude.NFData ExecutionAbortedEventDetails
