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
-- Module      : Amazonka.ComputeOptimizer.Types.ContainerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ContainerConfiguration where

import Amazonka.ComputeOptimizer.Types.MemorySizeConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the container configurations within the tasks of your Amazon
-- ECS service.
--
-- /See:/ 'newContainerConfiguration' smart constructor.
data ContainerConfiguration = ContainerConfiguration'
  { -- | The name of the container.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The number of CPU units reserved for the container.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The memory size configurations for the container.
    memorySizeConfiguration :: Prelude.Maybe MemorySizeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'containerConfiguration_containerName' - The name of the container.
--
-- 'cpu', 'containerConfiguration_cpu' - The number of CPU units reserved for the container.
--
-- 'memorySizeConfiguration', 'containerConfiguration_memorySizeConfiguration' - The memory size configurations for the container.
newContainerConfiguration ::
  ContainerConfiguration
newContainerConfiguration =
  ContainerConfiguration'
    { containerName =
        Prelude.Nothing,
      cpu = Prelude.Nothing,
      memorySizeConfiguration = Prelude.Nothing
    }

-- | The name of the container.
containerConfiguration_containerName :: Lens.Lens' ContainerConfiguration (Prelude.Maybe Prelude.Text)
containerConfiguration_containerName = Lens.lens (\ContainerConfiguration' {containerName} -> containerName) (\s@ContainerConfiguration' {} a -> s {containerName = a} :: ContainerConfiguration)

-- | The number of CPU units reserved for the container.
containerConfiguration_cpu :: Lens.Lens' ContainerConfiguration (Prelude.Maybe Prelude.Int)
containerConfiguration_cpu = Lens.lens (\ContainerConfiguration' {cpu} -> cpu) (\s@ContainerConfiguration' {} a -> s {cpu = a} :: ContainerConfiguration)

-- | The memory size configurations for the container.
containerConfiguration_memorySizeConfiguration :: Lens.Lens' ContainerConfiguration (Prelude.Maybe MemorySizeConfiguration)
containerConfiguration_memorySizeConfiguration = Lens.lens (\ContainerConfiguration' {memorySizeConfiguration} -> memorySizeConfiguration) (\s@ContainerConfiguration' {} a -> s {memorySizeConfiguration = a} :: ContainerConfiguration)

instance Data.FromJSON ContainerConfiguration where
  parseJSON =
    Data.withObject
      "ContainerConfiguration"
      ( \x ->
          ContainerConfiguration'
            Prelude.<$> (x Data..:? "containerName")
            Prelude.<*> (x Data..:? "cpu")
            Prelude.<*> (x Data..:? "memorySizeConfiguration")
      )

instance Prelude.Hashable ContainerConfiguration where
  hashWithSalt _salt ContainerConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memorySizeConfiguration

instance Prelude.NFData ContainerConfiguration where
  rnf ContainerConfiguration' {..} =
    Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memorySizeConfiguration
