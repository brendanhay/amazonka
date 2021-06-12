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
-- Module      : Network.AWS.ECS.Types.ContainerOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerOverride where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types.EnvironmentFile
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.ResourceRequirement
import qualified Network.AWS.Lens as Lens

-- | The overrides that should be sent to a container. An empty container
-- override can be passed in. An example of an empty container override
-- would be @{\"containerOverrides\": [ ] }@. If a non-empty container
-- override is specified, the @name@ parameter must be included.
--
-- /See:/ 'newContainerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
  { -- | The hard limit (in MiB) of memory to present to the container, instead
    -- of the default value from the task definition. If your container
    -- attempts to exceed the memory specified here, the container is killed.
    -- You must also specify a container name.
    memory :: Core.Maybe Core.Int,
    -- | The soft limit (in MiB) of memory to reserve for the container, instead
    -- of the default value from the task definition. You must also specify a
    -- container name.
    memoryReservation :: Core.Maybe Core.Int,
    -- | A list of files containing the environment variables to pass to a
    -- container, instead of the value from the container definition.
    environmentFiles :: Core.Maybe [EnvironmentFile],
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the task definition. You must also specify a container name.
    environment :: Core.Maybe [KeyValuePair],
    -- | The name of the container that receives the override. This parameter is
    -- required if any override is specified.
    name :: Core.Maybe Core.Text,
    -- | The command to send to the container that overrides the default command
    -- from the Docker image or the task definition. You must also specify a
    -- container name.
    command :: Core.Maybe [Core.Text],
    -- | The number of @cpu@ units reserved for the container, instead of the
    -- default value from the task definition. You must also specify a
    -- container name.
    cpu :: Core.Maybe Core.Int,
    -- | The type and amount of a resource to assign to a container, instead of
    -- the default value from the task definition. The only supported resource
    -- is a GPU.
    resourceRequirements :: Core.Maybe [ResourceRequirement]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ContainerOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memory', 'containerOverride_memory' - The hard limit (in MiB) of memory to present to the container, instead
-- of the default value from the task definition. If your container
-- attempts to exceed the memory specified here, the container is killed.
-- You must also specify a container name.
--
-- 'memoryReservation', 'containerOverride_memoryReservation' - The soft limit (in MiB) of memory to reserve for the container, instead
-- of the default value from the task definition. You must also specify a
-- container name.
--
-- 'environmentFiles', 'containerOverride_environmentFiles' - A list of files containing the environment variables to pass to a
-- container, instead of the value from the container definition.
--
-- 'environment', 'containerOverride_environment' - The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
--
-- 'name', 'containerOverride_name' - The name of the container that receives the override. This parameter is
-- required if any override is specified.
--
-- 'command', 'containerOverride_command' - The command to send to the container that overrides the default command
-- from the Docker image or the task definition. You must also specify a
-- container name.
--
-- 'cpu', 'containerOverride_cpu' - The number of @cpu@ units reserved for the container, instead of the
-- default value from the task definition. You must also specify a
-- container name.
--
-- 'resourceRequirements', 'containerOverride_resourceRequirements' - The type and amount of a resource to assign to a container, instead of
-- the default value from the task definition. The only supported resource
-- is a GPU.
newContainerOverride ::
  ContainerOverride
newContainerOverride =
  ContainerOverride'
    { memory = Core.Nothing,
      memoryReservation = Core.Nothing,
      environmentFiles = Core.Nothing,
      environment = Core.Nothing,
      name = Core.Nothing,
      command = Core.Nothing,
      cpu = Core.Nothing,
      resourceRequirements = Core.Nothing
    }

-- | The hard limit (in MiB) of memory to present to the container, instead
-- of the default value from the task definition. If your container
-- attempts to exceed the memory specified here, the container is killed.
-- You must also specify a container name.
containerOverride_memory :: Lens.Lens' ContainerOverride (Core.Maybe Core.Int)
containerOverride_memory = Lens.lens (\ContainerOverride' {memory} -> memory) (\s@ContainerOverride' {} a -> s {memory = a} :: ContainerOverride)

-- | The soft limit (in MiB) of memory to reserve for the container, instead
-- of the default value from the task definition. You must also specify a
-- container name.
containerOverride_memoryReservation :: Lens.Lens' ContainerOverride (Core.Maybe Core.Int)
containerOverride_memoryReservation = Lens.lens (\ContainerOverride' {memoryReservation} -> memoryReservation) (\s@ContainerOverride' {} a -> s {memoryReservation = a} :: ContainerOverride)

-- | A list of files containing the environment variables to pass to a
-- container, instead of the value from the container definition.
containerOverride_environmentFiles :: Lens.Lens' ContainerOverride (Core.Maybe [EnvironmentFile])
containerOverride_environmentFiles = Lens.lens (\ContainerOverride' {environmentFiles} -> environmentFiles) (\s@ContainerOverride' {} a -> s {environmentFiles = a} :: ContainerOverride) Core.. Lens.mapping Lens._Coerce

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
containerOverride_environment :: Lens.Lens' ContainerOverride (Core.Maybe [KeyValuePair])
containerOverride_environment = Lens.lens (\ContainerOverride' {environment} -> environment) (\s@ContainerOverride' {} a -> s {environment = a} :: ContainerOverride) Core.. Lens.mapping Lens._Coerce

-- | The name of the container that receives the override. This parameter is
-- required if any override is specified.
containerOverride_name :: Lens.Lens' ContainerOverride (Core.Maybe Core.Text)
containerOverride_name = Lens.lens (\ContainerOverride' {name} -> name) (\s@ContainerOverride' {} a -> s {name = a} :: ContainerOverride)

-- | The command to send to the container that overrides the default command
-- from the Docker image or the task definition. You must also specify a
-- container name.
containerOverride_command :: Lens.Lens' ContainerOverride (Core.Maybe [Core.Text])
containerOverride_command = Lens.lens (\ContainerOverride' {command} -> command) (\s@ContainerOverride' {} a -> s {command = a} :: ContainerOverride) Core.. Lens.mapping Lens._Coerce

-- | The number of @cpu@ units reserved for the container, instead of the
-- default value from the task definition. You must also specify a
-- container name.
containerOverride_cpu :: Lens.Lens' ContainerOverride (Core.Maybe Core.Int)
containerOverride_cpu = Lens.lens (\ContainerOverride' {cpu} -> cpu) (\s@ContainerOverride' {} a -> s {cpu = a} :: ContainerOverride)

-- | The type and amount of a resource to assign to a container, instead of
-- the default value from the task definition. The only supported resource
-- is a GPU.
containerOverride_resourceRequirements :: Lens.Lens' ContainerOverride (Core.Maybe [ResourceRequirement])
containerOverride_resourceRequirements = Lens.lens (\ContainerOverride' {resourceRequirements} -> resourceRequirements) (\s@ContainerOverride' {} a -> s {resourceRequirements = a} :: ContainerOverride) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON ContainerOverride where
  parseJSON =
    Core.withObject
      "ContainerOverride"
      ( \x ->
          ContainerOverride'
            Core.<$> (x Core..:? "memory")
            Core.<*> (x Core..:? "memoryReservation")
            Core.<*> (x Core..:? "environmentFiles" Core..!= Core.mempty)
            Core.<*> (x Core..:? "environment" Core..!= Core.mempty)
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "command" Core..!= Core.mempty)
            Core.<*> (x Core..:? "cpu")
            Core.<*> ( x Core..:? "resourceRequirements"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable ContainerOverride

instance Core.NFData ContainerOverride

instance Core.ToJSON ContainerOverride where
  toJSON ContainerOverride' {..} =
    Core.object
      ( Core.catMaybes
          [ ("memory" Core..=) Core.<$> memory,
            ("memoryReservation" Core..=)
              Core.<$> memoryReservation,
            ("environmentFiles" Core..=)
              Core.<$> environmentFiles,
            ("environment" Core..=) Core.<$> environment,
            ("name" Core..=) Core.<$> name,
            ("command" Core..=) Core.<$> command,
            ("cpu" Core..=) Core.<$> cpu,
            ("resourceRequirements" Core..=)
              Core.<$> resourceRequirements
          ]
      )
