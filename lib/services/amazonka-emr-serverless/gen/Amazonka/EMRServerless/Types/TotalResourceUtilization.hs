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
-- Module      : Amazonka.EMRServerless.Types.TotalResourceUtilization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.TotalResourceUtilization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The aggregate vCPU, memory, and storage resources used from the time job
-- start executing till the time job is terminated, rounded up to the
-- nearest second.
--
-- /See:/ 'newTotalResourceUtilization' smart constructor.
data TotalResourceUtilization = TotalResourceUtilization'
  { -- | The aggregated memory used per hour from the time job start executing
    -- till the time job is terminated.
    memoryGBHour :: Prelude.Maybe Prelude.Double,
    -- | The aggregated storage used per hour from the time job start executing
    -- till the time job is terminated.
    storageGBHour :: Prelude.Maybe Prelude.Double,
    -- | The aggregated vCPU used per hour from the time job start executing till
    -- the time job is terminated.
    vCPUHour :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TotalResourceUtilization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memoryGBHour', 'totalResourceUtilization_memoryGBHour' - The aggregated memory used per hour from the time job start executing
-- till the time job is terminated.
--
-- 'storageGBHour', 'totalResourceUtilization_storageGBHour' - The aggregated storage used per hour from the time job start executing
-- till the time job is terminated.
--
-- 'vCPUHour', 'totalResourceUtilization_vCPUHour' - The aggregated vCPU used per hour from the time job start executing till
-- the time job is terminated.
newTotalResourceUtilization ::
  TotalResourceUtilization
newTotalResourceUtilization =
  TotalResourceUtilization'
    { memoryGBHour =
        Prelude.Nothing,
      storageGBHour = Prelude.Nothing,
      vCPUHour = Prelude.Nothing
    }

-- | The aggregated memory used per hour from the time job start executing
-- till the time job is terminated.
totalResourceUtilization_memoryGBHour :: Lens.Lens' TotalResourceUtilization (Prelude.Maybe Prelude.Double)
totalResourceUtilization_memoryGBHour = Lens.lens (\TotalResourceUtilization' {memoryGBHour} -> memoryGBHour) (\s@TotalResourceUtilization' {} a -> s {memoryGBHour = a} :: TotalResourceUtilization)

-- | The aggregated storage used per hour from the time job start executing
-- till the time job is terminated.
totalResourceUtilization_storageGBHour :: Lens.Lens' TotalResourceUtilization (Prelude.Maybe Prelude.Double)
totalResourceUtilization_storageGBHour = Lens.lens (\TotalResourceUtilization' {storageGBHour} -> storageGBHour) (\s@TotalResourceUtilization' {} a -> s {storageGBHour = a} :: TotalResourceUtilization)

-- | The aggregated vCPU used per hour from the time job start executing till
-- the time job is terminated.
totalResourceUtilization_vCPUHour :: Lens.Lens' TotalResourceUtilization (Prelude.Maybe Prelude.Double)
totalResourceUtilization_vCPUHour = Lens.lens (\TotalResourceUtilization' {vCPUHour} -> vCPUHour) (\s@TotalResourceUtilization' {} a -> s {vCPUHour = a} :: TotalResourceUtilization)

instance Data.FromJSON TotalResourceUtilization where
  parseJSON =
    Data.withObject
      "TotalResourceUtilization"
      ( \x ->
          TotalResourceUtilization'
            Prelude.<$> (x Data..:? "memoryGBHour")
            Prelude.<*> (x Data..:? "storageGBHour")
            Prelude.<*> (x Data..:? "vCPUHour")
      )

instance Prelude.Hashable TotalResourceUtilization where
  hashWithSalt _salt TotalResourceUtilization' {..} =
    _salt `Prelude.hashWithSalt` memoryGBHour
      `Prelude.hashWithSalt` storageGBHour
      `Prelude.hashWithSalt` vCPUHour

instance Prelude.NFData TotalResourceUtilization where
  rnf TotalResourceUtilization' {..} =
    Prelude.rnf memoryGBHour
      `Prelude.seq` Prelude.rnf storageGBHour
      `Prelude.seq` Prelude.rnf vCPUHour
