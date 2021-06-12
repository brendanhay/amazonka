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
-- Module      : Network.AWS.CodePipeline.Types.StopExecutionTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StopExecutionTrigger where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The interaction that stopped a pipeline execution.
--
-- /See:/ 'newStopExecutionTrigger' smart constructor.
data StopExecutionTrigger = StopExecutionTrigger'
  { -- | The user-specified reason the pipeline was stopped.
    reason :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopExecutionTrigger' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'stopExecutionTrigger_reason' - The user-specified reason the pipeline was stopped.
newStopExecutionTrigger ::
  StopExecutionTrigger
newStopExecutionTrigger =
  StopExecutionTrigger' {reason = Core.Nothing}

-- | The user-specified reason the pipeline was stopped.
stopExecutionTrigger_reason :: Lens.Lens' StopExecutionTrigger (Core.Maybe Core.Text)
stopExecutionTrigger_reason = Lens.lens (\StopExecutionTrigger' {reason} -> reason) (\s@StopExecutionTrigger' {} a -> s {reason = a} :: StopExecutionTrigger)

instance Core.FromJSON StopExecutionTrigger where
  parseJSON =
    Core.withObject
      "StopExecutionTrigger"
      ( \x ->
          StopExecutionTrigger' Core.<$> (x Core..:? "reason")
      )

instance Core.Hashable StopExecutionTrigger

instance Core.NFData StopExecutionTrigger
