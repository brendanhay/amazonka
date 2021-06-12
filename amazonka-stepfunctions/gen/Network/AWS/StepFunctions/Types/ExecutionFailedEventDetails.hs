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
-- Module      : Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.ExecutionFailedEventDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about an execution failure event.
--
-- /See:/ 'newExecutionFailedEventDetails' smart constructor.
data ExecutionFailedEventDetails = ExecutionFailedEventDetails'
  { -- | A more detailed explanation of the cause of the failure.
    cause :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The error code of the failure.
    error :: Core.Maybe (Core.Sensitive Core.Text)
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
    { cause = Core.Nothing,
      error = Core.Nothing
    }

-- | A more detailed explanation of the cause of the failure.
executionFailedEventDetails_cause :: Lens.Lens' ExecutionFailedEventDetails (Core.Maybe Core.Text)
executionFailedEventDetails_cause = Lens.lens (\ExecutionFailedEventDetails' {cause} -> cause) (\s@ExecutionFailedEventDetails' {} a -> s {cause = a} :: ExecutionFailedEventDetails) Core.. Lens.mapping Core._Sensitive

-- | The error code of the failure.
executionFailedEventDetails_error :: Lens.Lens' ExecutionFailedEventDetails (Core.Maybe Core.Text)
executionFailedEventDetails_error = Lens.lens (\ExecutionFailedEventDetails' {error} -> error) (\s@ExecutionFailedEventDetails' {} a -> s {error = a} :: ExecutionFailedEventDetails) Core.. Lens.mapping Core._Sensitive

instance Core.FromJSON ExecutionFailedEventDetails where
  parseJSON =
    Core.withObject
      "ExecutionFailedEventDetails"
      ( \x ->
          ExecutionFailedEventDetails'
            Core.<$> (x Core..:? "cause") Core.<*> (x Core..:? "error")
      )

instance Core.Hashable ExecutionFailedEventDetails

instance Core.NFData ExecutionFailedEventDetails
