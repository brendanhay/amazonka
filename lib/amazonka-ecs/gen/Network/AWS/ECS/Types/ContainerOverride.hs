{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerOverride
  ( ContainerOverride (..),

    -- * Smart constructor
    mkContainerOverride,

    -- * Lenses
    coCommand,
    coEnvironment,
    coEnvironmentFiles,
    coResourceRequirements,
    coMemory,
    coName,
    coCpu,
    coMemoryReservation,
  )
where

import Network.AWS.ECS.Types.EnvironmentFile
import Network.AWS.ECS.Types.KeyValuePair
import Network.AWS.ECS.Types.ResourceRequirement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The overrides that should be sent to a container. An empty container override can be passed in. An example of an empty container override would be @{"containerOverrides": [ ] }@ . If a non-empty container override is specified, the @name@ parameter must be included.
--
-- /See:/ 'mkContainerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
  { -- | The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
    command :: Lude.Maybe [Lude.Text],
    -- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
    environment :: Lude.Maybe [KeyValuePair],
    -- | A list of files containing the environment variables to pass to a container, instead of the value from the container definition.
    environmentFiles :: Lude.Maybe [EnvironmentFile],
    -- | The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
    resourceRequirements :: Lude.Maybe [ResourceRequirement],
    -- | The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
    memory :: Lude.Maybe Lude.Int,
    -- | The name of the container that receives the override. This parameter is required if any override is specified.
    name :: Lude.Maybe Lude.Text,
    -- | The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
    cpu :: Lude.Maybe Lude.Int,
    -- | The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
    memoryReservation :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerOverride' with the minimum fields required to make a request.
--
-- * 'command' - The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
-- * 'environment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
-- * 'environmentFiles' - A list of files containing the environment variables to pass to a container, instead of the value from the container definition.
-- * 'resourceRequirements' - The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
-- * 'memory' - The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
-- * 'name' - The name of the container that receives the override. This parameter is required if any override is specified.
-- * 'cpu' - The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
-- * 'memoryReservation' - The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
mkContainerOverride ::
  ContainerOverride
mkContainerOverride =
  ContainerOverride'
    { command = Lude.Nothing,
      environment = Lude.Nothing,
      environmentFiles = Lude.Nothing,
      resourceRequirements = Lude.Nothing,
      memory = Lude.Nothing,
      name = Lude.Nothing,
      cpu = Lude.Nothing,
      memoryReservation = Lude.Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCommand :: Lens.Lens' ContainerOverride (Lude.Maybe [Lude.Text])
coCommand = Lens.lens (command :: ContainerOverride -> Lude.Maybe [Lude.Text]) (\s a -> s {command = a} :: ContainerOverride)
{-# DEPRECATED coCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnvironment :: Lens.Lens' ContainerOverride (Lude.Maybe [KeyValuePair])
coEnvironment = Lens.lens (environment :: ContainerOverride -> Lude.Maybe [KeyValuePair]) (\s a -> s {environment = a} :: ContainerOverride)
{-# DEPRECATED coEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | A list of files containing the environment variables to pass to a container, instead of the value from the container definition.
--
-- /Note:/ Consider using 'environmentFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnvironmentFiles :: Lens.Lens' ContainerOverride (Lude.Maybe [EnvironmentFile])
coEnvironmentFiles = Lens.lens (environmentFiles :: ContainerOverride -> Lude.Maybe [EnvironmentFile]) (\s a -> s {environmentFiles = a} :: ContainerOverride)
{-# DEPRECATED coEnvironmentFiles "Use generic-lens or generic-optics with 'environmentFiles' instead." #-}

-- | The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coResourceRequirements :: Lens.Lens' ContainerOverride (Lude.Maybe [ResourceRequirement])
coResourceRequirements = Lens.lens (resourceRequirements :: ContainerOverride -> Lude.Maybe [ResourceRequirement]) (\s a -> s {resourceRequirements = a} :: ContainerOverride)
{-# DEPRECATED coResourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead." #-}

-- | The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMemory :: Lens.Lens' ContainerOverride (Lude.Maybe Lude.Int)
coMemory = Lens.lens (memory :: ContainerOverride -> Lude.Maybe Lude.Int) (\s a -> s {memory = a} :: ContainerOverride)
{-# DEPRECATED coMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The name of the container that receives the override. This parameter is required if any override is specified.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coName :: Lens.Lens' ContainerOverride (Lude.Maybe Lude.Text)
coName = Lens.lens (name :: ContainerOverride -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ContainerOverride)
{-# DEPRECATED coName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCpu :: Lens.Lens' ContainerOverride (Lude.Maybe Lude.Int)
coCpu = Lens.lens (cpu :: ContainerOverride -> Lude.Maybe Lude.Int) (\s a -> s {cpu = a} :: ContainerOverride)
{-# DEPRECATED coCpu "Use generic-lens or generic-optics with 'cpu' instead." #-}

-- | The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'memoryReservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMemoryReservation :: Lens.Lens' ContainerOverride (Lude.Maybe Lude.Int)
coMemoryReservation = Lens.lens (memoryReservation :: ContainerOverride -> Lude.Maybe Lude.Int) (\s a -> s {memoryReservation = a} :: ContainerOverride)
{-# DEPRECATED coMemoryReservation "Use generic-lens or generic-optics with 'memoryReservation' instead." #-}

instance Lude.FromJSON ContainerOverride where
  parseJSON =
    Lude.withObject
      "ContainerOverride"
      ( \x ->
          ContainerOverride'
            Lude.<$> (x Lude..:? "command" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "environment" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "environmentFiles" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "resourceRequirements" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "memory")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "cpu")
            Lude.<*> (x Lude..:? "memoryReservation")
      )

instance Lude.ToJSON ContainerOverride where
  toJSON ContainerOverride' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("command" Lude..=) Lude.<$> command,
            ("environment" Lude..=) Lude.<$> environment,
            ("environmentFiles" Lude..=) Lude.<$> environmentFiles,
            ("resourceRequirements" Lude..=) Lude.<$> resourceRequirements,
            ("memory" Lude..=) Lude.<$> memory,
            ("name" Lude..=) Lude.<$> name,
            ("cpu" Lude..=) Lude.<$> cpu,
            ("memoryReservation" Lude..=) Lude.<$> memoryReservation
          ]
      )
