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
-- Module      : Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionTimedOutEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about the execution timeout that occurred during the
-- execution.
--
-- /See:/ 'newExecutionTimedOutEventDetails' smart constructor.
data ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails'
  { -- | A more detailed explanation of the cause of the timeout.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
        Core.Nothing,
      error = Core.Nothing
    }

-- | A more detailed explanation of the cause of the timeout.
executionTimedOutEventDetails_cause :: Lens.Lens' ExecutionTimedOutEventDetails (Core.Maybe Core.Text)
executionTimedOutEventDetails_cause = Lens.lens (\ExecutionTimedOutEventDetails' {cause} -> cause) (\s@ExecutionTimedOutEventDetails' {} a -> s {cause = a} :: ExecutionTimedOutEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
executionTimedOutEventDetails_error :: Lens.Lens' ExecutionTimedOutEventDetails (Core.Maybe Core.Text)
executionTimedOutEventDetails_error = Lens.lens (\ExecutionTimedOutEventDetails' {error} -> error) (\s@ExecutionTimedOutEventDetails' {} a -> s {error = a} :: ExecutionTimedOutEventDetails) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON ExecutionTimedOutEventDetails where
  parseJSON =
    Core.withObject
      "ExecutionTimedOutEventDetails"
      ( \x ->
          ExecutionTimedOutEventDetails'
            Core.<$> (x Core..:? "cause") Core.<*> (x Core..:? "error")
      )

instance Core.Hashable ExecutionTimedOutEventDetails

instance Core.NFData ExecutionTimedOutEventDetails
