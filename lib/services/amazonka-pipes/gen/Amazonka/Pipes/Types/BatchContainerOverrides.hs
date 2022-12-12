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
-- Module      : Amazonka.Pipes.Types.BatchContainerOverrides
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.BatchContainerOverrides where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.BatchEnvironmentVariable
import Amazonka.Pipes.Types.BatchResourceRequirement
import qualified Amazonka.Prelude as Prelude

-- | The overrides that are sent to a container.
--
-- /See:/ 'newBatchContainerOverrides' smart constructor.
data BatchContainerOverrides = BatchContainerOverrides'
  { -- | The command to send to the container that overrides the default command
    -- from the Docker image or the task definition.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the task definition.
    --
    -- Environment variables cannot start with \"@Batch@\". This naming
    -- convention is reserved for variables that Batch sets.
    environment :: Prelude.Maybe [BatchEnvironmentVariable],
    -- | The instance type to use for a multi-node parallel job.
    --
    -- This parameter isn\'t applicable to single-node container jobs or jobs
    -- that run on Fargate resources, and shouldn\'t be provided.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The type and amount of resources to assign to a container. This
    -- overrides the settings in the job definition. The supported resources
    -- include @GPU@, @MEMORY@, and @VCPU@.
    resourceRequirements :: Prelude.Maybe [BatchResourceRequirement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchContainerOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'batchContainerOverrides_command' - The command to send to the container that overrides the default command
-- from the Docker image or the task definition.
--
-- 'environment', 'batchContainerOverrides_environment' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition.
--
-- Environment variables cannot start with \"@Batch@\". This naming
-- convention is reserved for variables that Batch sets.
--
-- 'instanceType', 'batchContainerOverrides_instanceType' - The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
--
-- 'resourceRequirements', 'batchContainerOverrides_resourceRequirements' - The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
newBatchContainerOverrides ::
  BatchContainerOverrides
newBatchContainerOverrides =
  BatchContainerOverrides'
    { command = Prelude.Nothing,
      environment = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing
    }

-- | The command to send to the container that overrides the default command
-- from the Docker image or the task definition.
batchContainerOverrides_command :: Lens.Lens' BatchContainerOverrides (Prelude.Maybe [Prelude.Text])
batchContainerOverrides_command = Lens.lens (\BatchContainerOverrides' {command} -> command) (\s@BatchContainerOverrides' {} a -> s {command = a} :: BatchContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition.
--
-- Environment variables cannot start with \"@Batch@\". This naming
-- convention is reserved for variables that Batch sets.
batchContainerOverrides_environment :: Lens.Lens' BatchContainerOverrides (Prelude.Maybe [BatchEnvironmentVariable])
batchContainerOverrides_environment = Lens.lens (\BatchContainerOverrides' {environment} -> environment) (\s@BatchContainerOverrides' {} a -> s {environment = a} :: BatchContainerOverrides) Prelude.. Lens.mapping Lens.coerced

-- | The instance type to use for a multi-node parallel job.
--
-- This parameter isn\'t applicable to single-node container jobs or jobs
-- that run on Fargate resources, and shouldn\'t be provided.
batchContainerOverrides_instanceType :: Lens.Lens' BatchContainerOverrides (Prelude.Maybe Prelude.Text)
batchContainerOverrides_instanceType = Lens.lens (\BatchContainerOverrides' {instanceType} -> instanceType) (\s@BatchContainerOverrides' {} a -> s {instanceType = a} :: BatchContainerOverrides)

-- | The type and amount of resources to assign to a container. This
-- overrides the settings in the job definition. The supported resources
-- include @GPU@, @MEMORY@, and @VCPU@.
batchContainerOverrides_resourceRequirements :: Lens.Lens' BatchContainerOverrides (Prelude.Maybe [BatchResourceRequirement])
batchContainerOverrides_resourceRequirements = Lens.lens (\BatchContainerOverrides' {resourceRequirements} -> resourceRequirements) (\s@BatchContainerOverrides' {} a -> s {resourceRequirements = a} :: BatchContainerOverrides) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON BatchContainerOverrides where
  parseJSON =
    Data.withObject
      "BatchContainerOverrides"
      ( \x ->
          BatchContainerOverrides'
            Prelude.<$> (x Data..:? "Command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> ( x Data..:? "ResourceRequirements"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchContainerOverrides where
  hashWithSalt _salt BatchContainerOverrides' {..} =
    _salt `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` resourceRequirements

instance Prelude.NFData BatchContainerOverrides where
  rnf BatchContainerOverrides' {..} =
    Prelude.rnf command
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf resourceRequirements

instance Data.ToJSON BatchContainerOverrides where
  toJSON BatchContainerOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Command" Data..=) Prelude.<$> command,
            ("Environment" Data..=) Prelude.<$> environment,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("ResourceRequirements" Data..=)
              Prelude.<$> resourceRequirements
          ]
      )
