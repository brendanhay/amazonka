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
-- Module      : Amazonka.MechanicalTurk.Types.WorkerBlock
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MechanicalTurk.Types.WorkerBlock where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The WorkerBlock data structure represents a Worker who has been blocked.
-- It has two elements: the WorkerId and the Reason for the block.
--
-- /See:/ 'newWorkerBlock' smart constructor.
data WorkerBlock = WorkerBlock'
  { -- | The ID of the Worker who accepted the HIT.
    workerId :: Prelude.Maybe Prelude.Text,
    -- | A message explaining the reason the Worker was blocked.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workerId', 'workerBlock_workerId' - The ID of the Worker who accepted the HIT.
--
-- 'reason', 'workerBlock_reason' - A message explaining the reason the Worker was blocked.
newWorkerBlock ::
  WorkerBlock
newWorkerBlock =
  WorkerBlock'
    { workerId = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The ID of the Worker who accepted the HIT.
workerBlock_workerId :: Lens.Lens' WorkerBlock (Prelude.Maybe Prelude.Text)
workerBlock_workerId = Lens.lens (\WorkerBlock' {workerId} -> workerId) (\s@WorkerBlock' {} a -> s {workerId = a} :: WorkerBlock)

-- | A message explaining the reason the Worker was blocked.
workerBlock_reason :: Lens.Lens' WorkerBlock (Prelude.Maybe Prelude.Text)
workerBlock_reason = Lens.lens (\WorkerBlock' {reason} -> reason) (\s@WorkerBlock' {} a -> s {reason = a} :: WorkerBlock)

instance Core.FromJSON WorkerBlock where
  parseJSON =
    Core.withObject
      "WorkerBlock"
      ( \x ->
          WorkerBlock'
            Prelude.<$> (x Core..:? "WorkerId")
            Prelude.<*> (x Core..:? "Reason")
      )

instance Prelude.Hashable WorkerBlock where
  hashWithSalt _salt WorkerBlock' {..} =
    _salt `Prelude.hashWithSalt` workerId
      `Prelude.hashWithSalt` reason

instance Prelude.NFData WorkerBlock where
  rnf WorkerBlock' {..} =
    Prelude.rnf workerId
      `Prelude.seq` Prelude.rnf reason
