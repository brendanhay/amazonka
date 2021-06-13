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
import qualified Network.AWS.Prelude as Prelude

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
    memory :: Prelude.Maybe Prelude.Int,
    -- | The soft limit (in MiB) of memory to reserve for the container, instead
    -- of the default value from the task definition. You must also specify a
    -- container name.
    memoryReservation :: Prelude.Maybe Prelude.Int,
    -- | A list of files containing the environment variables to pass to a
    -- container, instead of the value from the container definition.
    environmentFiles :: Prelude.Maybe [EnvironmentFile],
    -- | The environment variables to send to the container. You can add new
    -- environment variables, which are added to the container at launch, or
    -- you can override the existing environment variables from the Docker
    -- image or the task definition. You must also specify a container name.
    environment :: Prelude.Maybe [KeyValuePair],
    -- | The name of the container that receives the override. This parameter is
    -- required if any override is specified.
    name :: Prelude.Maybe Prelude.Text,
    -- | The command to send to the container that overrides the default command
    -- from the Docker image or the task definition. You must also specify a
    -- container name.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The number of @cpu@ units reserved for the container, instead of the
    -- default value from the task definition. You must also specify a
    -- container name.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | The type and amount of a resource to assign to a container, instead of
    -- the default value from the task definition. The only supported resource
    -- is a GPU.
    resourceRequirements :: Prelude.Maybe [ResourceRequirement]
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
    { memory = Prelude.Nothing,
      memoryReservation = Prelude.Nothing,
      environmentFiles = Prelude.Nothing,
      environment = Prelude.Nothing,
      name = Prelude.Nothing,
      command = Prelude.Nothing,
      cpu = Prelude.Nothing,
      resourceRequirements = Prelude.Nothing
    }

-- | The hard limit (in MiB) of memory to present to the container, instead
-- of the default value from the task definition. If your container
-- attempts to exceed the memory specified here, the container is killed.
-- You must also specify a container name.
containerOverride_memory :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Int)
containerOverride_memory = Lens.lens (\ContainerOverride' {memory} -> memory) (\s@ContainerOverride' {} a -> s {memory = a} :: ContainerOverride)

-- | The soft limit (in MiB) of memory to reserve for the container, instead
-- of the default value from the task definition. You must also specify a
-- container name.
containerOverride_memoryReservation :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Int)
containerOverride_memoryReservation = Lens.lens (\ContainerOverride' {memoryReservation} -> memoryReservation) (\s@ContainerOverride' {} a -> s {memoryReservation = a} :: ContainerOverride)

-- | A list of files containing the environment variables to pass to a
-- container, instead of the value from the container definition.
containerOverride_environmentFiles :: Lens.Lens' ContainerOverride (Prelude.Maybe [EnvironmentFile])
containerOverride_environmentFiles = Lens.lens (\ContainerOverride' {environmentFiles} -> environmentFiles) (\s@ContainerOverride' {} a -> s {environmentFiles = a} :: ContainerOverride) Prelude.. Lens.mapping Lens._Coerce

-- | The environment variables to send to the container. You can add new
-- environment variables, which are added to the container at launch, or
-- you can override the existing environment variables from the Docker
-- image or the task definition. You must also specify a container name.
containerOverride_environment :: Lens.Lens' ContainerOverride (Prelude.Maybe [KeyValuePair])
containerOverride_environment = Lens.lens (\ContainerOverride' {environment} -> environment) (\s@ContainerOverride' {} a -> s {environment = a} :: ContainerOverride) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the container that receives the override. This parameter is
-- required if any override is specified.
containerOverride_name :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Text)
containerOverride_name = Lens.lens (\ContainerOverride' {name} -> name) (\s@ContainerOverride' {} a -> s {name = a} :: ContainerOverride)

-- | The command to send to the container that overrides the default command
-- from the Docker image or the task definition. You must also specify a
-- container name.
containerOverride_command :: Lens.Lens' ContainerOverride (Prelude.Maybe [Prelude.Text])
containerOverride_command = Lens.lens (\ContainerOverride' {command} -> command) (\s@ContainerOverride' {} a -> s {command = a} :: ContainerOverride) Prelude.. Lens.mapping Lens._Coerce

-- | The number of @cpu@ units reserved for the container, instead of the
-- default value from the task definition. You must also specify a
-- container name.
containerOverride_cpu :: Lens.Lens' ContainerOverride (Prelude.Maybe Prelude.Int)
containerOverride_cpu = Lens.lens (\ContainerOverride' {cpu} -> cpu) (\s@ContainerOverride' {} a -> s {cpu = a} :: ContainerOverride)

-- | The type and amount of a resource to assign to a container, instead of
-- the default value from the task definition. The only supported resource
-- is a GPU.
containerOverride_resourceRequirements :: Lens.Lens' ContainerOverride (Prelude.Maybe [ResourceRequirement])
containerOverride_resourceRequirements = Lens.lens (\ContainerOverride' {resourceRequirements} -> resourceRequirements) (\s@ContainerOverride' {} a -> s {resourceRequirements = a} :: ContainerOverride) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON ContainerOverride where
  parseJSON =
    Core.withObject
      "ContainerOverride"
      ( \x ->
          ContainerOverride'
            Prelude.<$> (x Core..:? "memory")
            Prelude.<*> (x Core..:? "memoryReservation")
            Prelude.<*> ( x Core..:? "environmentFiles"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "environment" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "command" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "cpu")
            Prelude.<*> ( x Core..:? "resourceRequirements"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ContainerOverride

instance Prelude.NFData ContainerOverride

instance Core.ToJSON ContainerOverride where
  toJSON ContainerOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("memory" Core..=) Prelude.<$> memory,
            ("memoryReservation" Core..=)
              Prelude.<$> memoryReservation,
            ("environmentFiles" Core..=)
              Prelude.<$> environmentFiles,
            ("environment" Core..=) Prelude.<$> environment,
            ("name" Core..=) Prelude.<$> name,
            ("command" Core..=) Prelude.<$> command,
            ("cpu" Core..=) Prelude.<$> cpu,
            ("resourceRequirements" Core..=)
              Prelude.<$> resourceRequirements
          ]
      )
