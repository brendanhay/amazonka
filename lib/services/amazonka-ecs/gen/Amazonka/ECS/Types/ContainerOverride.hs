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
-- Module      : Amazonka.ECS.Types.ContainerOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ContainerOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types.EnvironmentFile
import Amazonka.ECS.Types.KeyValuePair
import Amazonka.ECS.Types.ResourceRequirement
import qualified Amazonka.Prelude as Prelude

-- | The overrides that are sent to a container. An empty container override
-- can be passed in. An example of an empty container override is
-- @{\"containerOverrides\": [ ] }@. If a non-empty container override is
-- specified, the @name@ parameter must be included.
--
-- /See:/ 'newContainerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
  { -- | The name of the container that receives the override. This parameter is
    -- required if any override is specified.
    name :: Prelude.Maybe Prelude.Text,
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the task definition. You must also specify a container name.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The type and amount of a resource to assign to a container, instead of
    -- the default value from the task definition. The only supported resource
    -- is a GPU.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement],
    -- | The hard limit (in MiB) of memory to present to the container, instead
    -- of the default value from the task definition. If your container
    -- attempts to exceed the memory specified here, the container is killed.
    -- You must also specify a container name.
    memory :: Prelude.Maybe Prelude.Int,
    -- | The number of @cpu@ units reserved for the container, instead of the
    -- default value from the task definition. You must also specify a
    -- container name.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The soft limit (in MiB) of memory to reserve for the container, instead
    -- of the default value from the task definition. You must also specify a
    -- container name.
    memoryReservation :: Prelude.Maybe Prelude.Int,
    -- | The command to send to the container that overrides the default command
    -- from the Docker image or the task definition. You must also specify a
    -- container name.
    command :: Prelude.Maybe [Prelude.Text],
    -- | A list of files containing the environment variables to pass to a
    -- container, instead of the value from the container definition.
    environmentFiles :: Prelude.Maybe [EnvironmentFile]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'containerOverride_name' - The name of the container that receives the override. This parameter is
-- required if any override is specified.
--
-- 'environment', 'containerOverride_environment' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
--
-- 'resourceRequirements', 'containerOverride_resourceRequirements' - The type and amount of a resource to assign to a container, instead of
-- the default value from the task definition. The only supported resource
-- is a GPU.
--
-- 'memory', 'containerOverride_memory' - The hard limit (in MiB) of memory to present to the container, instead
-- of the default value from the task definition. If your container
-- attempts to exceed the memory specified here, the container is killed.
-- You must also specify a container name.
--
-- 'cpu', 'containerOverride_cpu' - The number of @cpu@ units reserved for the container, instead of the
-- default value from the task definition. You must also specify a
-- container name.
--
-- 'memoryReservation', 'containerOverride_memoryReservation' - The soft limit (in MiB) of memory to reserve for the container, instead
-- of the default value from the task definition. You must also specify a
-- container name.
--
-- 'command', 'containerOverride_command' - The command to send to the container that overrides the default command
-- from the Docker image or the task definition. You must also specify a
-- container name.
--
-- 'environmentFiles', 'containerOverride_environmentFiles' - A list of files containing the environment variables to pass to a
-- container, instead of the value from the container definition.
newContainerOverride ::
  ContainerOverride
newContainerOverride =
  ContainerOverride'
    { name = Prelude.Nothing,
      environment = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing,
      memory = Prelude.Nothing,
      cpu = Prelude.Nothing,
      memoryReservation = Prelude.Nothing,
      command = Prelude.Nothing,
      environmentFiles = Prelude.Nothing
    }

-- | The name of the container that receives the override. This parameter is
-- required if any override is specified.
containerOverride_name :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Text)
containerOverride_name = Lens.lens (\ContainerOverride' {name} -> name) (\s@ContainerOverride' {} a -> s {name = a} :: ContainerOverride)

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
containerOverride_environment :: Lens.Lens' ContainerOverride (Prelude.Maybe [KeyValuePair])
containerOverride_environment = Lens.lens (\ContainerOverride' {environment} -> environment) (\s@ContainerOverride' {} a -> s {environment = a} :: ContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | The type and amount of a resource to assign to a container, instead of
-- the default value from the task definition. The only supported resource
-- is a GPU.
containerOverride_resourceRequirements :: Lens.Lens' ContainerOverride (Prelude.Maybe [ResourceRequirement])
containerOverride_resourceRequirements = Lens.lens (\ContainerOverride' {resourceRequirements} -> resourceRequirements) (\s@ContainerOverride' {} a -> s {resourceRequirements = a} :: ContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | The hard limit (in MiB) of memory to present to the container, instead
-- of the default value from the task definition. If your container
-- attempts to exceed the memory specified here, the container is killed.
-- You must also specify a container name.
containerOverride_memory :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Int)
containerOverride_memory = Lens.lens (\ContainerOverride' {memory} -> memory) (\s@ContainerOverride' {} a -> s {memory = a} :: ContainerOverride)

-- | The number of @cpu@ units reserved for the container, instead of the
-- default value from the task definition. You must also specify a
-- container name.
containerOverride_cpu :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Int)
containerOverride_cpu = Lens.lens (\ContainerOverride' {cpu} -> cpu) (\s@ContainerOverride' {} a -> s {cpu = a} :: ContainerOverride)

-- | The soft limit (in MiB) of memory to reserve for the container, instead
-- of the default value from the task definition. You must also specify a
-- container name.
containerOverride_memoryReservation :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Int)
containerOverride_memoryReservation = Lens.lens (\ContainerOverride' {memoryReservation} -> memoryReservation) (\s@ContainerOverride' {} a -> s {memoryReservation = a} :: ContainerOverride)

-- | The command to send to the container that overrides the default command
-- from the Docker image or the task definition. You must also specify a
-- container name.
containerOverride_command :: Lens.Lens' ContainerOverride (Prelude.Maybe [Prelude.Text])
containerOverride_command = Lens.lens (\ContainerOverride' {command} -> command) (\s@ContainerOverride' {} a -> s {command = a} :: ContainerOverride) Prelude.. Lens.mapping Lens.coerced

-- | A list of files containing the environment variables to pass to a
-- container, instead of the value from the container definition.
containerOverride_environmentFiles :: Lens.Lens' ContainerOverride (Prelude.Maybe [EnvironmentFile])
containerOverride_environmentFiles = Lens.lens (\ContainerOverride' {environmentFiles} -> environmentFiles) (\s@ContainerOverride' {} a -> s {environmentFiles = a} :: ContainerOverride) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ContainerOverride where
  parseJSON =
    Core.withObject
      "ContainerOverride"
      ( \x ->
          ContainerOverride'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "environment" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "resourceRequirements"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "cpu")
            Prelude.<*> (x Core..:? "memoryReservation")
            Prelude.<*> (x Core..:? "command" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "environmentFiles"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ContainerOverride where
  hashWithSalt _salt ContainerOverride' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` resourceRequirements
      `Prelude.hashWithSalt` memory
      `Prelude.hashWithSalt` cpu
      `Prelude.hashWithSalt` memoryReservation
      `Prelude.hashWithSalt` command
      `Prelude.hashWithSalt` environmentFiles

instance Prelude.NFData ContainerOverride where
  rnf ContainerOverride' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf resourceRequirements
      `Prelude.seq` Prelude.rnf memory
      `Prelude.seq` Prelude.rnf cpu
      `Prelude.seq` Prelude.rnf memoryReservation
      `Prelude.seq` Prelude.rnf command
      `Prelude.seq` Prelude.rnf environmentFiles

instance Core.ToJSON ContainerOverride where
  toJSON ContainerOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("name" Core..=) Prelude.<$> name,
            ("environment" Core..=) Prelude.<$> environment,
            ("resourceRequirements" Core..=)
              Prelude.<$> resourceRequirements,
            ("memory" Core..=) Prelude.<$> memory,
            ("cpu" Core..=) Prelude.<$> cpu,
            ("memoryReservation" Core..=)
              Prelude.<$> memoryReservation,
            ("command" Core..=) Prelude.<$> command,
            ("environmentFiles" Core..=)
              Prelude.<$> environmentFiles
          ]
      )
