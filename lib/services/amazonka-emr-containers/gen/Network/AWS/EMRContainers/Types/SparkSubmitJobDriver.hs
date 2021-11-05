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
-- Module      : Amazonka.EMRContainers.Types.SparkSubmitJobDriver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.SparkSubmitJobDriver where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information about job driver for Spark submit.
--
-- /See:/ 'newSparkSubmitJobDriver' smart constructor.
data SparkSubmitJobDriver = SparkSubmitJobDriver'
  { -- | The Spark submit parameters that are used for job runs.
    sparkSubmitParameters :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The arguments for job application.
    entryPointArguments :: Prelude.Maybe [Core.Sensitive Prelude.Text],
    -- | The entry point of job application.
    entryPoint :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SparkSubmitJobDriver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sparkSubmitParameters', 'sparkSubmitJobDriver_sparkSubmitParameters' - The Spark submit parameters that are used for job runs.
--
-- 'entryPointArguments', 'sparkSubmitJobDriver_entryPointArguments' - The arguments for job application.
--
-- 'entryPoint', 'sparkSubmitJobDriver_entryPoint' - The entry point of job application.
newSparkSubmitJobDriver ::
  -- | 'entryPoint'
  Prelude.Text ->
  SparkSubmitJobDriver
newSparkSubmitJobDriver pEntryPoint_ =
  SparkSubmitJobDriver'
    { sparkSubmitParameters =
        Prelude.Nothing,
      entryPointArguments = Prelude.Nothing,
      entryPoint = Core._Sensitive Lens.# pEntryPoint_
    }

-- | The Spark submit parameters that are used for job runs.
sparkSubmitJobDriver_sparkSubmitParameters :: Lens.Lens' SparkSubmitJobDriver (Prelude.Maybe Prelude.Text)
sparkSubmitJobDriver_sparkSubmitParameters = Lens.lens (\SparkSubmitJobDriver' {sparkSubmitParameters} -> sparkSubmitParameters) (\s@SparkSubmitJobDriver' {} a -> s {sparkSubmitParameters = a} :: SparkSubmitJobDriver) Prelude.. Lens.mapping Core._Sensitive

-- | The arguments for job application.
sparkSubmitJobDriver_entryPointArguments :: Lens.Lens' SparkSubmitJobDriver (Prelude.Maybe [Prelude.Text])
sparkSubmitJobDriver_entryPointArguments = Lens.lens (\SparkSubmitJobDriver' {entryPointArguments} -> entryPointArguments) (\s@SparkSubmitJobDriver' {} a -> s {entryPointArguments = a} :: SparkSubmitJobDriver) Prelude.. Lens.mapping Lens.coerced

-- | The entry point of job application.
sparkSubmitJobDriver_entryPoint :: Lens.Lens' SparkSubmitJobDriver Prelude.Text
sparkSubmitJobDriver_entryPoint = Lens.lens (\SparkSubmitJobDriver' {entryPoint} -> entryPoint) (\s@SparkSubmitJobDriver' {} a -> s {entryPoint = a} :: SparkSubmitJobDriver) Prelude.. Core._Sensitive

instance Core.FromJSON SparkSubmitJobDriver where
  parseJSON =
    Core.withObject
      "SparkSubmitJobDriver"
      ( \x ->
          SparkSubmitJobDriver'
            Prelude.<$> (x Core..:? "sparkSubmitParameters")
            Prelude.<*> ( x Core..:? "entryPointArguments"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..: "entryPoint")
      )

instance Prelude.Hashable SparkSubmitJobDriver

instance Prelude.NFData SparkSubmitJobDriver

instance Core.ToJSON SparkSubmitJobDriver where
  toJSON SparkSubmitJobDriver' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sparkSubmitParameters" Core..=)
              Prelude.<$> sparkSubmitParameters,
            ("entryPointArguments" Core..=)
              Prelude.<$> entryPointArguments,
            Prelude.Just ("entryPoint" Core..= entryPoint)
          ]
      )
