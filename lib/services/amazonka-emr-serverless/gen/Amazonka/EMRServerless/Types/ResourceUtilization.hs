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
-- Module      : Amazonka.EMRServerless.Types.ResourceUtilization
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.ResourceUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The resource utilization for memory, storage, and vCPU for jobs.
--
-- /See:/ 'newResourceUtilization' smart constructor.
data ResourceUtilization = ResourceUtilization'
  { -- | The aggregated memory used per hour from the time the job starts
    -- executing until the job is terminated.
    memoryGBHour :: Prelude.Maybe Prelude.Double,
    -- | The aggregated storage used per hour from the time the job starts
    -- executing until the job is terminated.
    storageGBHour :: Prelude.Maybe Prelude.Double,
    -- | The aggregated vCPU used per hour from the time the job starts executing
    -- until the job is terminated.
    vCPUHour :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memoryGBHour', 'resourceUtilization_memoryGBHour' - The aggregated memory used per hour from the time the job starts
-- executing until the job is terminated.
--
-- 'storageGBHour', 'resourceUtilization_storageGBHour' - The aggregated storage used per hour from the time the job starts
-- executing until the job is terminated.
--
-- 'vCPUHour', 'resourceUtilization_vCPUHour' - The aggregated vCPU used per hour from the time the job starts executing
-- until the job is terminated.
newResourceUtilization ::
  ResourceUtilization
newResourceUtilization =
  ResourceUtilization'
    { memoryGBHour =
        Prelude.Nothing,
      storageGBHour = Prelude.Nothing,
      vCPUHour = Prelude.Nothing
    }

-- | The aggregated memory used per hour from the time the job starts
-- executing until the job is terminated.
resourceUtilization_memoryGBHour :: Lens.Lens' ResourceUtilization (Prelude.Maybe Prelude.Double)
resourceUtilization_memoryGBHour = Lens.lens (\ResourceUtilization' {memoryGBHour} -> memoryGBHour) (\s@ResourceUtilization' {} a -> s {memoryGBHour = a} :: ResourceUtilization)

-- | The aggregated storage used per hour from the time the job starts
-- executing until the job is terminated.
resourceUtilization_storageGBHour :: Lens.Lens' ResourceUtilization (Prelude.Maybe Prelude.Double)
resourceUtilization_storageGBHour = Lens.lens (\ResourceUtilization' {storageGBHour} -> storageGBHour) (\s@ResourceUtilization' {} a -> s {storageGBHour = a} :: ResourceUtilization)

-- | The aggregated vCPU used per hour from the time the job starts executing
-- until the job is terminated.
resourceUtilization_vCPUHour :: Lens.Lens' ResourceUtilization (Prelude.Maybe Prelude.Double)
resourceUtilization_vCPUHour = Lens.lens (\ResourceUtilization' {vCPUHour} -> vCPUHour) (\s@ResourceUtilization' {} a -> s {vCPUHour = a} :: ResourceUtilization)

instance Data.FromJSON ResourceUtilization where
  parseJSON =
    Data.withObject
      "ResourceUtilization"
      ( \x ->
          ResourceUtilization'
            Prelude.<$> (x Data..:? "memoryGBHour")
            Prelude.<*> (x Data..:? "storageGBHour")
            Prelude.<*> (x Data..:? "vCPUHour")
      )

instance Prelude.Hashable ResourceUtilization where
  hashWithSalt _salt ResourceUtilization' {..} =
    _salt
      `Prelude.hashWithSalt` memoryGBHour
      `Prelude.hashWithSalt` storageGBHour
      `Prelude.hashWithSalt` vCPUHour

instance Prelude.NFData ResourceUtilization where
  rnf ResourceUtilization' {..} =
    Prelude.rnf memoryGBHour
      `Prelude.seq` Prelude.rnf storageGBHour
      `Prelude.seq` Prelude.rnf vCPUHour
