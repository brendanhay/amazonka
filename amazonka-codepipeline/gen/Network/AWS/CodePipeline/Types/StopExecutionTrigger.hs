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
-- Module      : Network.AWS.CodePipeline.Types.StopExecutionTrigger
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StopExecutionTrigger where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The interaction that stopped a pipeline execution.
--
-- /See:/ 'newStopExecutionTrigger' smart constructor.
data StopExecutionTrigger = StopExecutionTrigger'
  { -- | The user-specified reason the pipeline was stopped.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  StopExecutionTrigger' {reason = Prelude.Nothing}

-- | The user-specified reason the pipeline was stopped.
stopExecutionTrigger_reason :: Lens.Lens' StopExecutionTrigger (Prelude.Maybe Prelude.Text)
stopExecutionTrigger_reason = Lens.lens (\StopExecutionTrigger' {reason} -> reason) (\s@StopExecutionTrigger' {} a -> s {reason = a} :: StopExecutionTrigger)

instance Prelude.FromJSON StopExecutionTrigger where
  parseJSON =
    Prelude.withObject
      "StopExecutionTrigger"
      ( \x ->
          StopExecutionTrigger'
            Prelude.<$> (x Prelude..:? "reason")
      )

instance Prelude.Hashable StopExecutionTrigger

instance Prelude.NFData StopExecutionTrigger
