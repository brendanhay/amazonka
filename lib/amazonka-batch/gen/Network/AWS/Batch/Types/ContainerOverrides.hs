{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerOverrides
  ( ContainerOverrides (..),

    -- * Smart constructor
    mkContainerOverrides,

    -- * Lenses
    coCommand,
    coEnvironment,
    coInstanceType,
    coMemory,
    coResourceRequirements,
    coVcpus,
  )
where

import qualified Network.AWS.Batch.Types.KeyValuePair as Types
import qualified Network.AWS.Batch.Types.ResourceRequirement as Types
import qualified Network.AWS.Batch.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The overrides that should be sent to a container.
--
-- /See:/ 'mkContainerOverrides' smart constructor.
data ContainerOverrides = ContainerOverrides'
  { -- | The command to send to the container that overrides the default command from the Docker image or the job definition.
    command :: Core.Maybe [Types.String],
    -- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
    environment :: Core.Maybe [Types.KeyValuePair],
    -- | The instance type to use for a multi-node parallel job. This parameter is not valid for single-node container jobs.
    instanceType :: Core.Maybe Types.String,
    -- | The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
    memory :: Core.Maybe Core.Int,
    -- | The type and amount of a resource to assign to a container. This value overrides the value set in the job definition. Currently, the only supported resource is @GPU@ .
    resourceRequirements :: Core.Maybe [Types.ResourceRequirement],
    -- | The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
    vcpus :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContainerOverrides' value with any optional fields omitted.
mkContainerOverrides ::
  ContainerOverrides
mkContainerOverrides =
  ContainerOverrides'
    { command = Core.Nothing,
      environment = Core.Nothing,
      instanceType = Core.Nothing,
      memory = Core.Nothing,
      resourceRequirements = Core.Nothing,
      vcpus = Core.Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the job definition.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCommand :: Lens.Lens' ContainerOverrides (Core.Maybe [Types.String])
coCommand = Lens.field @"command"
{-# DEPRECATED coCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnvironment :: Lens.Lens' ContainerOverrides (Core.Maybe [Types.KeyValuePair])
coEnvironment = Lens.field @"environment"
{-# DEPRECATED coEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The instance type to use for a multi-node parallel job. This parameter is not valid for single-node container jobs.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coInstanceType :: Lens.Lens' ContainerOverrides (Core.Maybe Types.String)
coInstanceType = Lens.field @"instanceType"
{-# DEPRECATED coInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMemory :: Lens.Lens' ContainerOverrides (Core.Maybe Core.Int)
coMemory = Lens.field @"memory"
{-# DEPRECATED coMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The type and amount of a resource to assign to a container. This value overrides the value set in the job definition. Currently, the only supported resource is @GPU@ .
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coResourceRequirements :: Lens.Lens' ContainerOverrides (Core.Maybe [Types.ResourceRequirement])
coResourceRequirements = Lens.field @"resourceRequirements"
{-# DEPRECATED coResourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead." #-}

-- | The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
--
-- /Note:/ Consider using 'vcpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coVcpus :: Lens.Lens' ContainerOverrides (Core.Maybe Core.Int)
coVcpus = Lens.field @"vcpus"
{-# DEPRECATED coVcpus "Use generic-lens or generic-optics with 'vcpus' instead." #-}

instance Core.FromJSON ContainerOverrides where
  toJSON ContainerOverrides {..} =
    Core.object
      ( Core.catMaybes
          [ ("command" Core..=) Core.<$> command,
            ("environment" Core..=) Core.<$> environment,
            ("instanceType" Core..=) Core.<$> instanceType,
            ("memory" Core..=) Core.<$> memory,
            ("resourceRequirements" Core..=) Core.<$> resourceRequirements,
            ("vcpus" Core..=) Core.<$> vcpus
          ]
      )
