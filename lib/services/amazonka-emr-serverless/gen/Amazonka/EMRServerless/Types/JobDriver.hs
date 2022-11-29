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
-- Module      : Amazonka.EMRServerless.Types.JobDriver
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRServerless.Types.JobDriver where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMRServerless.Types.Hive
import Amazonka.EMRServerless.Types.SparkSubmit
import qualified Amazonka.Prelude as Prelude

-- | The driver that the job runs on.
--
-- /See:/ 'newJobDriver' smart constructor.
data JobDriver = JobDriver'
  { -- | The job driver parameters specified for Hive.
    hive :: Prelude.Maybe Hive,
    -- | The job driver parameters specified for Spark.
    sparkSubmit :: Prelude.Maybe SparkSubmit
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDriver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hive', 'jobDriver_hive' - The job driver parameters specified for Hive.
--
-- 'sparkSubmit', 'jobDriver_sparkSubmit' - The job driver parameters specified for Spark.
newJobDriver ::
  JobDriver
newJobDriver =
  JobDriver'
    { hive = Prelude.Nothing,
      sparkSubmit = Prelude.Nothing
    }

-- | The job driver parameters specified for Hive.
jobDriver_hive :: Lens.Lens' JobDriver (Prelude.Maybe Hive)
jobDriver_hive = Lens.lens (\JobDriver' {hive} -> hive) (\s@JobDriver' {} a -> s {hive = a} :: JobDriver)

-- | The job driver parameters specified for Spark.
jobDriver_sparkSubmit :: Lens.Lens' JobDriver (Prelude.Maybe SparkSubmit)
jobDriver_sparkSubmit = Lens.lens (\JobDriver' {sparkSubmit} -> sparkSubmit) (\s@JobDriver' {} a -> s {sparkSubmit = a} :: JobDriver)

instance Core.FromJSON JobDriver where
  parseJSON =
    Core.withObject
      "JobDriver"
      ( \x ->
          JobDriver'
            Prelude.<$> (x Core..:? "hive")
            Prelude.<*> (x Core..:? "sparkSubmit")
      )

instance Prelude.Hashable JobDriver where
  hashWithSalt _salt JobDriver' {..} =
    _salt `Prelude.hashWithSalt` hive
      `Prelude.hashWithSalt` sparkSubmit

instance Prelude.NFData JobDriver where
  rnf JobDriver' {..} =
    Prelude.rnf hive
      `Prelude.seq` Prelude.rnf sparkSubmit

instance Core.ToJSON JobDriver where
  toJSON JobDriver' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("hive" Core..=) Prelude.<$> hive,
            ("sparkSubmit" Core..=) Prelude.<$> sparkSubmit
          ]
      )
