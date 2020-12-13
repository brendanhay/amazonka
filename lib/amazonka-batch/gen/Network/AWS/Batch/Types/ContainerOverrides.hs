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
    coResourceRequirements,
    coInstanceType,
    coMemory,
    coVcpus,
  )
where

import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.ResourceRequirement
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The overrides that should be sent to a container.
--
-- /See:/ 'mkContainerOverrides' smart constructor.
data ContainerOverrides = ContainerOverrides'
  { -- | The command to send to the container that overrides the default command from the Docker image or the job definition.
    command :: Lude.Maybe [Lude.Text],
    -- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
    environment :: Lude.Maybe [KeyValuePair],
    -- | The type and amount of a resource to assign to a container. This value overrides the value set in the job definition. Currently, the only supported resource is @GPU@ .
    resourceRequirements :: Lude.Maybe [ResourceRequirement],
    -- | The instance type to use for a multi-node parallel job. This parameter is not valid for single-node container jobs.
    instanceType :: Lude.Maybe Lude.Text,
    -- | The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
    memory :: Lude.Maybe Lude.Int,
    -- | The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
    vcpus :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ContainerOverrides' with the minimum fields required to make a request.
--
-- * 'command' - The command to send to the container that overrides the default command from the Docker image or the job definition.
-- * 'environment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
-- * 'resourceRequirements' - The type and amount of a resource to assign to a container. This value overrides the value set in the job definition. Currently, the only supported resource is @GPU@ .
-- * 'instanceType' - The instance type to use for a multi-node parallel job. This parameter is not valid for single-node container jobs.
-- * 'memory' - The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
-- * 'vcpus' - The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
mkContainerOverrides ::
  ContainerOverrides
mkContainerOverrides =
  ContainerOverrides'
    { command = Lude.Nothing,
      environment = Lude.Nothing,
      resourceRequirements = Lude.Nothing,
      instanceType = Lude.Nothing,
      memory = Lude.Nothing,
      vcpus = Lude.Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the job definition.
--
-- /Note:/ Consider using 'command' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCommand :: Lens.Lens' ContainerOverrides (Lude.Maybe [Lude.Text])
coCommand = Lens.lens (command :: ContainerOverrides -> Lude.Maybe [Lude.Text]) (\s a -> s {command = a} :: ContainerOverrides)
{-# DEPRECATED coCommand "Use generic-lens or generic-optics with 'command' instead." #-}

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
--
-- /Note:/ Consider using 'environment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coEnvironment :: Lens.Lens' ContainerOverrides (Lude.Maybe [KeyValuePair])
coEnvironment = Lens.lens (environment :: ContainerOverrides -> Lude.Maybe [KeyValuePair]) (\s a -> s {environment = a} :: ContainerOverrides)
{-# DEPRECATED coEnvironment "Use generic-lens or generic-optics with 'environment' instead." #-}

-- | The type and amount of a resource to assign to a container. This value overrides the value set in the job definition. Currently, the only supported resource is @GPU@ .
--
-- /Note:/ Consider using 'resourceRequirements' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coResourceRequirements :: Lens.Lens' ContainerOverrides (Lude.Maybe [ResourceRequirement])
coResourceRequirements = Lens.lens (resourceRequirements :: ContainerOverrides -> Lude.Maybe [ResourceRequirement]) (\s a -> s {resourceRequirements = a} :: ContainerOverrides)
{-# DEPRECATED coResourceRequirements "Use generic-lens or generic-optics with 'resourceRequirements' instead." #-}

-- | The instance type to use for a multi-node parallel job. This parameter is not valid for single-node container jobs.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coInstanceType :: Lens.Lens' ContainerOverrides (Lude.Maybe Lude.Text)
coInstanceType = Lens.lens (instanceType :: ContainerOverrides -> Lude.Maybe Lude.Text) (\s a -> s {instanceType = a} :: ContainerOverrides)
{-# DEPRECATED coInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
--
-- /Note:/ Consider using 'memory' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coMemory :: Lens.Lens' ContainerOverrides (Lude.Maybe Lude.Int)
coMemory = Lens.lens (memory :: ContainerOverrides -> Lude.Maybe Lude.Int) (\s a -> s {memory = a} :: ContainerOverrides)
{-# DEPRECATED coMemory "Use generic-lens or generic-optics with 'memory' instead." #-}

-- | The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
--
-- /Note:/ Consider using 'vcpus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coVcpus :: Lens.Lens' ContainerOverrides (Lude.Maybe Lude.Int)
coVcpus = Lens.lens (vcpus :: ContainerOverrides -> Lude.Maybe Lude.Int) (\s a -> s {vcpus = a} :: ContainerOverrides)
{-# DEPRECATED coVcpus "Use generic-lens or generic-optics with 'vcpus' instead." #-}

instance Lude.ToJSON ContainerOverrides where
  toJSON ContainerOverrides' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("command" Lude..=) Lude.<$> command,
            ("environment" Lude..=) Lude.<$> environment,
            ("resourceRequirements" Lude..=) Lude.<$> resourceRequirements,
            ("instanceType" Lude..=) Lude.<$> instanceType,
            ("memory" Lude..=) Lude.<$> memory,
            ("vcpus" Lude..=) Lude.<$> vcpus
          ]
      )
