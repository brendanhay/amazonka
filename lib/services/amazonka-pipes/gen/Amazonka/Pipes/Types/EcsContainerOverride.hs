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
-- Module      : Amazonka.Pipes.Types.EcsContainerOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.EcsContainerOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.EcsEnvironmentFile
import Amazonka.Pipes.Types.EcsEnvironmentVariable
import Amazonka.Pipes.Types.EcsResourceRequirement
import qualified Amazonka.Prelude as Prelude

-- | The overrides that are sent to a container. An empty container override
-- can be passed in. An example of an empty container override is
-- @{\"containerOverrides\": [ ] }@. If a non-empty container override is
-- specified, the @name@ parameter must be included.
--
-- /See:/ 'newEcsContainerOverride' smart constructor.
data EcsContainerOverride = EcsContainerOverride'
  { -- | The command to send to the container that overrides the default command
    -- from the Docker image or the task definition. You must also specify a
    -- container name.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The number of @cpu@ units reserved for the container, instead of the
    -- default value from the task definition. You must also specify a
    -- container name.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the task definition. You must also specify a container name.
    environment :: Prelude.Maybe [EcsEnvironmentVariable],
    -- | A list of files containing the environment variables to pass to a
    -- container, instead of the value from the container definition.
    environmentFiles :: Prelude.Maybe [EcsEnvironmentFile],
    -- | The hard limit (in MiB) of memory to present to the container, instead
    -- of the default value from the task definition. If your container
    -- attempts to exceed the memory specified here, the container is killed.
    -- You must also specify a container name.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The soft limit (in MiB) of memory to reserve for the container, instead
    -- of the default value from the task definition. You must also specify a
    -- container name.
    memoryReservation :: Prelude.Maybe Prelude.Int,
    -- | The name of the container that receives the override. This parameter is
    -- required if any override is specified.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type and amount of a resource to assign to a container, instead of
    -- the default value from the task definition. The only supported resource
    -- is a GPU.
    resourceRequirements :: Prelude.Maybe [EcsResourceRequirement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EcsContainerOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'ecsContainerOverride_command' - The command to send to the container that overrides the default command
-- from the Docker image or the task definition. You must also specify a
-- container name.
--
-- 'cpu', 'ecsContainerOverride_cpu' - The number of @cpu@ units reserved for the container, instead of the
-- default value from the task definition. You must also specify a
-- container name.
--
-- 'environment', 'ecsContainerOverride_environment' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
--
-- 'environmentFiles', 'ecsContainerOverride_environmentFiles' - A list of files containing the environment variables to pass to a
-- container, instead of the value from the container definition.
--
-- 'memory', 'ecsContainerOverride_memory' - The hard limit (in MiB) of memory to present to the container, instead
-- of the default value from the task definition. If your container
-- attempts to exceed the memory specified here, the container is killed.
-- You must also specify a container name.
--
-- 'memoryReservation', 'ecsContainerOverride_memoryReservation' - The soft limit (in MiB) of memory to reserve for the container, instead
-- of the default value from the task definition. You must also specify a
-- container name.
--
-- 'name', 'ecsContainerOverride_name' - The name of the container that receives the override. This parameter is
-- required if any override is specified.
--
-- 'resourceRequirements', 'ecsContainerOverride_resourceRequirements' - The type and amount of a resource to assign to a container, instead of
-- the default value from the task definition. The only supported resource
-- is a GPU.
newEcsContainerOverride ::
  EcsContainerOverride
newEcsContainerOverride =
  EcsContainerOverride'
    { command = Prelude.Nothing,
      cpu = Prelude.Nothing,
      environment = Prelude.Nothing,
      environmentFiles = Prelude.Nothing,
      memory = Prelude.Nothing,
      memoryReservation = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing
    }

-- | The command to send to the container that overrides the default command
-- from the Docker image or the task definition. You must also specify a
-- container name.
ecsContainerOverride_command :: Lens.Lens' EcsContainerOverride (Prelude.Maybe [Prelude.Text])
ecsContainerOverride_command = Lens.lens (\EcsContainerOverride' {command} -> command) (\s@EcsContainerOverride' {} a -> s {command = a} :: EcsContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | The number of @cpu@ units reserved for the container, instead of the
-- default value from the task definition. You must also specify a
-- container name.
ecsContainerOverride_cpu :: Lens.Lens' EcsContainerOverride (Prelude.Maybe Prelude.Int)
ecsContainerOverride_cpu = Lens.lens (\EcsContainerOverride' {cpu} -> cpu) (\s@EcsContainerOverride' {} a -> s {cpu = a} :: EcsContainerOverride)

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
ecsContainerOverride_environment :: Lens.Lens' EcsContainerOverride (Prelude.Maybe [EcsEnvironmentVariable])
ecsContainerOverride_environment = Lens.lens (\EcsContainerOverride' {environment} -> environment) (\s@EcsContainerOverride' {} a -> s {environment = a} :: EcsContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | A list of files containing the environment variables to pass to a
-- container, instead of the value from the container definition.
ecsContainerOverride_environmentFiles :: Lens.Lens' EcsContainerOverride (Prelude.Maybe [EcsEnvironmentFile])
ecsContainerOverride_environmentFiles = Lens.lens (\EcsContainerOverride' {environmentFiles} -> environmentFiles) (\s@EcsContainerOverride' {} a -> s {environmentFiles = a} :: EcsContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | The hard limit (in MiB) of memory to present to the container, instead
-- of the default value from the task definition. If your container
-- attempts to exceed the memory specified here, the container is killed.
-- You must also specify a container name.
ecsContainerOverride_memory :: Lens.Lens' EcsContainerOverride (Prelude.Maybe Prelude.Int)
ecsContainerOverride_memory = Lens.lens (\EcsContainerOverride' {memory} -> memory) (\s@EcsContainerOverride' {} a -> s {memory = a} :: EcsContainerOverride)

-- | The soft limit (in MiB) of memory to reserve for the container, instead
-- of the default value from the task definition. You must also specify a
-- container name.
ecsContainerOverride_memoryReservation :: Lens.Lens' EcsContainerOverride (Prelude.Maybe Prelude.Int)
ecsContainerOverride_memoryReservation = Lens.lens (\EcsContainerOverride' {memoryReservation} -> memoryReservation) (\s@EcsContainerOverride' {} a -> s {memoryReservation = a} :: EcsContainerOverride)

-- | The name of the container that receives the override. This parameter is
-- required if any override is specified.
ecsContainerOverride_name :: Lens.Lens' EcsContainerOverride (Prelude.Maybe Prelude.Text)
ecsContainerOverride_name = Lens.lens (\EcsContainerOverride' {name} -> name) (\s@EcsContainerOverride' {} a -> s {name = a} :: EcsContainerOverride)

-- | The type and amount of a resource to assign to a container, instead of
-- the default value from the task definition. The only supported resource
-- is a GPU.
ecsContainerOverride_resourceRequirements :: Lens.Lens' EcsContainerOverride (Prelude.Maybe [EcsResourceRequirement])
ecsContainerOverride_resourceRequirements = Lens.lens (\EcsContainerOverride' {resourceRequirements} -> resourceRequirements) (\s@EcsContainerOverride' {} a -> s {resourceRequirements = a} :: EcsContainerOverride) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON EcsContainerOverride where
  parseJSON =
    Data.withObject
      "EcsContainerOverride"
      ( \x ->
          EcsContainerOverride'
            Prelude.<$> (x Data..:? "Command" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Cpu")
            Prelude.<*> (x Data..:? "Environment" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "EnvironmentFiles"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Memory")
            Prelude.<*> (x Data..:? "MemoryReservation")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> ( x
                            Data..:? "ResourceRequirements"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EcsContainerOverride where
  hashWithSalt _salt EcsContainerOverride' {..} =
    _salt
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` environmentFiles
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` memoryReservation
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resourceRequirements

instance Prelude.NFData EcsContainerOverride where
  rnf EcsContainerOverride' {..} =
    Prelude.rnf command `Prelude.seq`
      Prelude.rnf cpu `Prelude.seq`
        Prelude.rnf environment `Prelude.seq`
          Prelude.rnf environmentFiles `Prelude.seq`
            Prelude.rnf memory `Prelude.seq`
              Prelude.rnf memoryReservation `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf resourceRequirements

instance Data.ToJSON EcsContainerOverride where
  toJSON EcsContainerOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Command" Data..=) Prelude.<$> command,
            ("Cpu" Data..=) Prelude.<$> cpu,
            ("Environment" Data..=) Prelude.<$> environment,
            ("EnvironmentFiles" Data..=)
              Prelude.<$> environmentFiles,
            ("Memory" Data..=) Prelude.<$> memory,
            ("MemoryReservation" Data..=)
              Prelude.<$> memoryReservation,
            ("Name" Data..=) Prelude.<$> name,
            ("ResourceRequirements" Data..=)
              Prelude.<$> resourceRequirements
          ]
      )
