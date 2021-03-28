{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerOverride
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.ContainerOverride
  ( ContainerOverride (..)
  -- * Smart constructor
  , mkContainerOverride
  -- * Lenses
  , coCommand
  , coCpu
  , coEnvironment
  , coEnvironmentFiles
  , coMemory
  , coMemoryReservation
  , coName
  , coResourceRequirements
  ) where

import qualified Network.AWS.ECS.Types.EnvironmentFile as Types
import qualified Network.AWS.ECS.Types.KeyValuePair as Types
import qualified Network.AWS.ECS.Types.ResourceRequirement as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The overrides that should be sent to a container. An empty container override can be passed in. An example of an empty container override would be @{"containerOverrides": [ ] }@ . If a non-empty container override is specified, the @name@ parameter must be included.
--
-- /See:/ 'mkContainerOverride' smart constructor.
data ContainerOverride = ContainerOverride'
  { command :: Core.Maybe [Core.Text]
    -- ^ The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
  , cpu :: Core.Maybe Core.Int
    -- ^ The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
  , environment :: Core.Maybe [Types.KeyValuePair]
    -- ^ The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
  , environmentFiles :: Core.Maybe [Types.EnvironmentFile]
    -- ^ A list of files containing the environment variables to pass to a container, instead of the value from the container definition.
  , memory :: Core.Maybe Core.Int
    -- ^ The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
  , memoryReservation :: Core.Maybe Core.Int
    -- ^ The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the container that receives the override. This parameter is required if any override is specified.
  , resourceRequirements :: Core.Maybe [Types.ResourceRequirement]
    -- ^ The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerOverride' value with any optional fields omitted.
mkContainerOverride
    :: ContainerOverride
mkContainerOverride
  = ContainerOverride'{command = Core.Nothing, cpu = Core.Nothing,
                       environment = Core.Nothing, environmentFiles = Core.Nothing,
                       memory = Core.Nothing, memoryReservation = Core.Nothing,
                       name = Core.Nothing, resourceRequirements = Core.Nothing}

-- | The command to send to the container that overrides the default command from the Docker image or the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCommand :: Lens.Lens' ContainerOverride (Core.Maybe [Core.Text])
coCommand = Lens.field @"command"
{-# INLINEABLE coCommand #-}
{-# DEPRECATED command "Use generic-lens or generic-optics with 'command' instead"  #-}

-- | The number of @cpu@ units reserved for the container, instead of the default value from the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'cpu' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCpu :: Lens.Lens' ContainerOverride (Core.Maybe Core.Int)
coCpu = Lens.field @"cpu"
{-# INLINEABLE coCpu #-}
{-# DEPRECATED cpu "Use generic-lens or generic-optics with 'cpu' instead"  #-}

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnvironment :: Lens.Lens' ContainerOverride (Core.Maybe [Types.KeyValuePair])
coEnvironment = Lens.field @"environment"
{-# INLINEABLE coEnvironment #-}
{-# DEPRECATED environment "Use generic-lens or generic-optics with 'environment' instead"  #-}

-- | A list of files containing the environment variables to pass to a container, instead of the value from the container definition.
--
-- /Note:/ Consider using 'environmentFiles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnvironmentFiles :: Lens.Lens' ContainerOverride (Core.Maybe [Types.EnvironmentFile])
coEnvironmentFiles = Lens.field @"environmentFiles"
{-# INLINEABLE coEnvironmentFiles #-}
{-# DEPRECATED environmentFiles "Use generic-lens or generic-optics with 'environmentFiles' instead"  #-}

-- | The hard limit (in MiB) of memory to present to the container, instead of the default value from the task definition. If your container attempts to exceed the memory specified here, the container is killed. You must also specify a container name.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMemory :: Lens.Lens' ContainerOverride (Core.Maybe Core.Int)
coMemory = Lens.field @"memory"
{-# INLINEABLE coMemory #-}
{-# DEPRECATED memory "Use generic-lens or generic-optics with 'memory' instead"  #-}

-- | The soft limit (in MiB) of memory to reserve for the container, instead of the default value from the task definition. You must also specify a container name.
--
-- /Note:/ Consider using 'memoryReservation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMemoryReservation :: Lens.Lens' ContainerOverride (Core.Maybe Core.Int)
coMemoryReservation = Lens.field @"memoryReservation"
{-# INLINEABLE coMemoryReservation #-}
{-# DEPRECATED memoryReservation "Use generic-lens or generic-optics with 'memoryReservation' instead"  #-}

-- | The name of the container that receives the override. This parameter is required if any override is specified.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coName :: Lens.Lens' ContainerOverride (Core.Maybe Core.Text)
coName = Lens.field @"name"
{-# INLINEABLE coName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type and amount of a resource to assign to a container, instead of the default value from the task definition. The only supported resource is a GPU.
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coResourceRequirements :: Lens.Lens' ContainerOverride (Core.Maybe [Types.ResourceRequirement])
coResourceRequirements = Lens.field @"resourceRequirements"
{-# INLINEABLE coResourceRequirements #-}
{-# DEPRECATED resourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead"  #-}

instance Core.FromJSON ContainerOverride where
        toJSON ContainerOverride{..}
          = Core.object
              (Core.catMaybes
                 [("command" Core..=) Core.<$> command,
                  ("cpu" Core..=) Core.<$> cpu,
                  ("environment" Core..=) Core.<$> environment,
                  ("environmentFiles" Core..=) Core.<$> environmentFiles,
                  ("memory" Core..=) Core.<$> memory,
                  ("memoryReservation" Core..=) Core.<$> memoryReservation,
                  ("name" Core..=) Core.<$> name,
                  ("resourceRequirements" Core..=) Core.<$> resourceRequirements])

instance Core.FromJSON ContainerOverride where
        parseJSON
          = Core.withObject "ContainerOverride" Core.$
              \ x ->
                ContainerOverride' Core.<$>
                  (x Core..:? "command") Core.<*> x Core..:? "cpu" Core.<*>
                    x Core..:? "environment"
                    Core.<*> x Core..:? "environmentFiles"
                    Core.<*> x Core..:? "memory"
                    Core.<*> x Core..:? "memoryReservation"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "resourceRequirements"
