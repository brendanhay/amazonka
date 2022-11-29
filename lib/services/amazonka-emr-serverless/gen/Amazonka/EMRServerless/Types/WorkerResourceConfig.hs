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
-- Module      : Amazonka.EMRServerless.Types.WorkerResourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.WorkerResourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The cumulative configuration requirements for every worker instance of
-- the worker type.
--
-- /See:/ 'newWorkerResourceConfig' smart constructor.
data WorkerResourceConfig = WorkerResourceConfig'
  { -- | The disk requirements for every worker instance of the worker type.
    disk :: Prelude.Maybe Prelude.Text,
    -- | The CPU requirements for every worker instance of the worker type.
    cpu :: Prelude.Text,
    -- | The memory requirements for every worker instance of the worker type.
    memory :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkerResourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disk', 'workerResourceConfig_disk' - The disk requirements for every worker instance of the worker type.
--
-- 'cpu', 'workerResourceConfig_cpu' - The CPU requirements for every worker instance of the worker type.
--
-- 'memory', 'workerResourceConfig_memory' - The memory requirements for every worker instance of the worker type.
newWorkerResourceConfig ::
  -- | 'cpu'
  Prelude.Text ->
  -- | 'memory'
  Prelude.Text ->
  WorkerResourceConfig
newWorkerResourceConfig pCpu_ pMemory_ =
  WorkerResourceConfig'
    { disk = Prelude.Nothing,
      cpu = pCpu_,
      memory = pMemory_
    }

-- | The disk requirements for every worker instance of the worker type.
workerResourceConfig_disk :: Lens.Lens' WorkerResourceConfig (Prelude.Maybe Prelude.Text)
workerResourceConfig_disk = Lens.lens (\WorkerResourceConfig' {disk} -> disk) (\s@WorkerResourceConfig' {} a -> s {disk = a} :: WorkerResourceConfig)

-- | The CPU requirements for every worker instance of the worker type.
workerResourceConfig_cpu :: Lens.Lens' WorkerResourceConfig Prelude.Text
workerResourceConfig_cpu = Lens.lens (\WorkerResourceConfig' {cpu} -> cpu) (\s@WorkerResourceConfig' {} a -> s {cpu = a} :: WorkerResourceConfig)

-- | The memory requirements for every worker instance of the worker type.
workerResourceConfig_memory :: Lens.Lens' WorkerResourceConfig Prelude.Text
workerResourceConfig_memory = Lens.lens (\WorkerResourceConfig' {memory} -> memory) (\s@WorkerResourceConfig' {} a -> s {memory = a} :: WorkerResourceConfig)

instance Core.FromJSON WorkerResourceConfig where
  parseJSON =
    Core.withObject
      "WorkerResourceConfig"
      ( \x ->
          WorkerResourceConfig'
            Prelude.<$> (x Core..:? "disk")
            Prelude.<*> (x Core..: "cpu")
            Prelude.<*> (x Core..: "memory")
      )

instance Prelude.Hashable WorkerResourceConfig where
  hashWithSalt _salt WorkerResourceConfig' {..} =
    _salt `Prelude.hashWithSalt` disk
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memory

instance Prelude.NFData WorkerResourceConfig where
  rnf WorkerResourceConfig' {..} =
    Prelude.rnf disk
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memory

instance Core.ToJSON WorkerResourceConfig where
  toJSON WorkerResourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("disk" Core..=) Prelude.<$> disk,
            Prelude.Just ("cpu" Core..= cpu),
            Prelude.Just ("memory" Core..= memory)
          ]
      )
