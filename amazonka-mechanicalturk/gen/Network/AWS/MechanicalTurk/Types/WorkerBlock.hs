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
-- Module      : Network.AWS.MechanicalTurk.Types.WorkerBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.WorkerBlock where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The WorkerBlock data structure represents a Worker who has been blocked.
-- It has two elements: the WorkerId and the Reason for the block.
--
-- /See:/ 'newWorkerBlock' smart constructor.
data WorkerBlock = WorkerBlock'
  { -- | A message explaining the reason the Worker was blocked.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Worker who accepted the HIT.
    workerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WorkerBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'workerBlock_reason' - A message explaining the reason the Worker was blocked.
--
-- 'workerId', 'workerBlock_workerId' - The ID of the Worker who accepted the HIT.
newWorkerBlock ::
  WorkerBlock
newWorkerBlock =
  WorkerBlock'
    { reason = Prelude.Nothing,
      workerId = Prelude.Nothing
    }

-- | A message explaining the reason the Worker was blocked.
workerBlock_reason :: Lens.Lens' WorkerBlock (Prelude.Maybe Prelude.Text)
workerBlock_reason = Lens.lens (\WorkerBlock' {reason} -> reason) (\s@WorkerBlock' {} a -> s {reason = a} :: WorkerBlock)

-- | The ID of the Worker who accepted the HIT.
workerBlock_workerId :: Lens.Lens' WorkerBlock (Prelude.Maybe Prelude.Text)
workerBlock_workerId = Lens.lens (\WorkerBlock' {workerId} -> workerId) (\s@WorkerBlock' {} a -> s {workerId = a} :: WorkerBlock)

instance Prelude.FromJSON WorkerBlock where
  parseJSON =
    Prelude.withObject
      "WorkerBlock"
      ( \x ->
          WorkerBlock'
            Prelude.<$> (x Prelude..:? "Reason")
            Prelude.<*> (x Prelude..:? "WorkerId")
      )

instance Prelude.Hashable WorkerBlock

instance Prelude.NFData WorkerBlock
