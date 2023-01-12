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
-- Module      : Amazonka.ComputeOptimizer.Types.ContainerRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ContainerRecommendation where

import Amazonka.ComputeOptimizer.Types.MemorySizeConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The CPU and memory recommendations for a container within the tasks of
-- your Amazon ECS service.
--
-- /See:/ 'newContainerRecommendation' smart constructor.
data ContainerRecommendation = ContainerRecommendation'
  { -- | The name of the container.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The recommended number of CPU units reserved for the container.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The recommended memory size configurations for the container.
    memorySizeConfiguration :: Prelude.Maybe MemorySizeConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'containerRecommendation_containerName' - The name of the container.
--
-- 'cpu', 'containerRecommendation_cpu' - The recommended number of CPU units reserved for the container.
--
-- 'memorySizeConfiguration', 'containerRecommendation_memorySizeConfiguration' - The recommended memory size configurations for the container.
newContainerRecommendation ::
  ContainerRecommendation
newContainerRecommendation =
  ContainerRecommendation'
    { containerName =
        Prelude.Nothing,
      cpu = Prelude.Nothing,
      memorySizeConfiguration = Prelude.Nothing
    }

-- | The name of the container.
containerRecommendation_containerName :: Lens.Lens' ContainerRecommendation (Prelude.Maybe Prelude.Text)
containerRecommendation_containerName = Lens.lens (\ContainerRecommendation' {containerName} -> containerName) (\s@ContainerRecommendation' {} a -> s {containerName = a} :: ContainerRecommendation)

-- | The recommended number of CPU units reserved for the container.
containerRecommendation_cpu :: Lens.Lens' ContainerRecommendation (Prelude.Maybe Prelude.Int)
containerRecommendation_cpu = Lens.lens (\ContainerRecommendation' {cpu} -> cpu) (\s@ContainerRecommendation' {} a -> s {cpu = a} :: ContainerRecommendation)

-- | The recommended memory size configurations for the container.
containerRecommendation_memorySizeConfiguration :: Lens.Lens' ContainerRecommendation (Prelude.Maybe MemorySizeConfiguration)
containerRecommendation_memorySizeConfiguration = Lens.lens (\ContainerRecommendation' {memorySizeConfiguration} -> memorySizeConfiguration) (\s@ContainerRecommendation' {} a -> s {memorySizeConfiguration = a} :: ContainerRecommendation)

instance Data.FromJSON ContainerRecommendation where
  parseJSON =
    Data.withObject
      "ContainerRecommendation"
      ( \x ->
          ContainerRecommendation'
            Prelude.<$> (x Data..:? "containerName")
            Prelude.<*> (x Data..:? "cpu")
            Prelude.<*> (x Data..:? "memorySizeConfiguration")
      )

instance Prelude.Hashable ContainerRecommendation where
  hashWithSalt _salt ContainerRecommendation' {..} =
    _salt `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memorySizeConfiguration

instance Prelude.NFData ContainerRecommendation where
  rnf ContainerRecommendation' {..} =
    Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memorySizeConfiguration
