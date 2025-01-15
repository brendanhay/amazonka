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
-- Module      : Amazonka.ComputeOptimizer.Types.MemorySizeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.MemorySizeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The memory size configurations of a container.
--
-- /See:/ 'newMemorySizeConfiguration' smart constructor.
data MemorySizeConfiguration = MemorySizeConfiguration'
  { -- | The amount of memory in the container.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The limit of memory reserve for the container.
    memoryReservation :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemorySizeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memory', 'memorySizeConfiguration_memory' - The amount of memory in the container.
--
-- 'memoryReservation', 'memorySizeConfiguration_memoryReservation' - The limit of memory reserve for the container.
newMemorySizeConfiguration ::
  MemorySizeConfiguration
newMemorySizeConfiguration =
  MemorySizeConfiguration'
    { memory = Prelude.Nothing,
      memoryReservation = Prelude.Nothing
    }

-- | The amount of memory in the container.
memorySizeConfiguration_memory :: Lens.Lens' MemorySizeConfiguration (Prelude.Maybe Prelude.Int)
memorySizeConfiguration_memory = Lens.lens (\MemorySizeConfiguration' {memory} -> memory) (\s@MemorySizeConfiguration' {} a -> s {memory = a} :: MemorySizeConfiguration)

-- | The limit of memory reserve for the container.
memorySizeConfiguration_memoryReservation :: Lens.Lens' MemorySizeConfiguration (Prelude.Maybe Prelude.Int)
memorySizeConfiguration_memoryReservation = Lens.lens (\MemorySizeConfiguration' {memoryReservation} -> memoryReservation) (\s@MemorySizeConfiguration' {} a -> s {memoryReservation = a} :: MemorySizeConfiguration)

instance Data.FromJSON MemorySizeConfiguration where
  parseJSON =
    Data.withObject
      "MemorySizeConfiguration"
      ( \x ->
          MemorySizeConfiguration'
            Prelude.<$> (x Data..:? "memory")
            Prelude.<*> (x Data..:? "memoryReservation")
      )

instance Prelude.Hashable MemorySizeConfiguration where
  hashWithSalt _salt MemorySizeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` memoryReservation

instance Prelude.NFData MemorySizeConfiguration where
  rnf MemorySizeConfiguration' {..} =
    Prelude.rnf memory `Prelude.seq`
      Prelude.rnf memoryReservation
