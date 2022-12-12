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
-- Module      : Amazonka.ECS.Types.RuntimePlatform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.RuntimePlatform where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.CPUArchitecture
import Amazonka.ECS.Types.OSFamily
import qualified Amazonka.Prelude as Prelude

-- | Information about the platform for the Amazon ECS service or task.
--
-- For more information about @RuntimePlatform@, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task_definition_parameters.html#runtime-platform RuntimePlatform>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newRuntimePlatform' smart constructor.
data RuntimePlatform = RuntimePlatform'
  { -- | The CPU architecture.
    --
    -- You can run your Linux tasks on an ARM-based platform by setting the
    -- value to @ARM64@. This option is available for tasks that run on Linux
    -- Amazon EC2 instance or Linux containers on Fargate.
    cpuArchitecture :: Prelude.Maybe CPUArchitecture,
    -- | The operating system.
    operatingSystemFamily :: Prelude.Maybe OSFamily
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimePlatform' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuArchitecture', 'runtimePlatform_cpuArchitecture' - The CPU architecture.
--
-- You can run your Linux tasks on an ARM-based platform by setting the
-- value to @ARM64@. This option is available for tasks that run on Linux
-- Amazon EC2 instance or Linux containers on Fargate.
--
-- 'operatingSystemFamily', 'runtimePlatform_operatingSystemFamily' - The operating system.
newRuntimePlatform ::
  RuntimePlatform
newRuntimePlatform =
  RuntimePlatform'
    { cpuArchitecture = Prelude.Nothing,
      operatingSystemFamily = Prelude.Nothing
    }

-- | The CPU architecture.
--
-- You can run your Linux tasks on an ARM-based platform by setting the
-- value to @ARM64@. This option is available for tasks that run on Linux
-- Amazon EC2 instance or Linux containers on Fargate.
runtimePlatform_cpuArchitecture :: Lens.Lens' RuntimePlatform (Prelude.Maybe CPUArchitecture)
runtimePlatform_cpuArchitecture = Lens.lens (\RuntimePlatform' {cpuArchitecture} -> cpuArchitecture) (\s@RuntimePlatform' {} a -> s {cpuArchitecture = a} :: RuntimePlatform)

-- | The operating system.
runtimePlatform_operatingSystemFamily :: Lens.Lens' RuntimePlatform (Prelude.Maybe OSFamily)
runtimePlatform_operatingSystemFamily = Lens.lens (\RuntimePlatform' {operatingSystemFamily} -> operatingSystemFamily) (\s@RuntimePlatform' {} a -> s {operatingSystemFamily = a} :: RuntimePlatform)

instance Data.FromJSON RuntimePlatform where
  parseJSON =
    Data.withObject
      "RuntimePlatform"
      ( \x ->
          RuntimePlatform'
            Prelude.<$> (x Data..:? "cpuArchitecture")
            Prelude.<*> (x Data..:? "operatingSystemFamily")
      )

instance Prelude.Hashable RuntimePlatform where
  hashWithSalt _salt RuntimePlatform' {..} =
    _salt `Prelude.hashWithSalt` cpuArchitecture
      `Prelude.hashWithSalt` operatingSystemFamily

instance Prelude.NFData RuntimePlatform where
  rnf RuntimePlatform' {..} =
    Prelude.rnf cpuArchitecture
      `Prelude.seq` Prelude.rnf operatingSystemFamily

instance Data.ToJSON RuntimePlatform where
  toJSON RuntimePlatform' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cpuArchitecture" Data..=)
              Prelude.<$> cpuArchitecture,
            ("operatingSystemFamily" Data..=)
              Prelude.<$> operatingSystemFamily
          ]
      )
