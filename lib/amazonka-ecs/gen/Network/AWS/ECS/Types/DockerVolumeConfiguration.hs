{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.DockerVolumeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.DockerVolumeConfiguration
  ( DockerVolumeConfiguration (..),

    -- * Smart constructor
    mkDockerVolumeConfiguration,

    -- * Lenses
    dvcAutoprovision,
    dvcDriver,
    dvcDriverOpts,
    dvcLabels,
    dvcScope,
  )
where

import qualified Network.AWS.ECS.Types.Driver as Types
import qualified Network.AWS.ECS.Types.Scope as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify a @host@ instead.
--
-- /See:/ 'mkDockerVolumeConfiguration' smart constructor.
data DockerVolumeConfiguration = DockerVolumeConfiguration'
  { -- | If this value is @true@ , the Docker volume is created if it does not already exist.
    autoprovision :: Core.Maybe Core.Bool,
    -- | The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
    driver :: Core.Maybe Types.Driver,
    -- | A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
    driverOpts :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
    labels :: Core.Maybe (Core.HashMap Types.String Types.String),
    -- | The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
    scope :: Core.Maybe Types.Scope
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DockerVolumeConfiguration' value with any optional fields omitted.
mkDockerVolumeConfiguration ::
  DockerVolumeConfiguration
mkDockerVolumeConfiguration =
  DockerVolumeConfiguration'
    { autoprovision = Core.Nothing,
      driver = Core.Nothing,
      driverOpts = Core.Nothing,
      labels = Core.Nothing,
      scope = Core.Nothing
    }

-- | If this value is @true@ , the Docker volume is created if it does not already exist.
--
-- /Note:/ Consider using 'autoprovision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcAutoprovision :: Lens.Lens' DockerVolumeConfiguration (Core.Maybe Core.Bool)
dvcAutoprovision = Lens.field @"autoprovision"
{-# DEPRECATED dvcAutoprovision "Use generic-lens or generic-optics with 'autoprovision' instead." #-}

-- | The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- /Note:/ Consider using 'driver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcDriver :: Lens.Lens' DockerVolumeConfiguration (Core.Maybe Types.Driver)
dvcDriver = Lens.field @"driver"
{-# DEPRECATED dvcDriver "Use generic-lens or generic-optics with 'driver' instead." #-}

-- | A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- /Note:/ Consider using 'driverOpts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcDriverOpts :: Lens.Lens' DockerVolumeConfiguration (Core.Maybe (Core.HashMap Types.String Types.String))
dvcDriverOpts = Lens.field @"driverOpts"
{-# DEPRECATED dvcDriverOpts "Use generic-lens or generic-optics with 'driverOpts' instead." #-}

-- | Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcLabels :: Lens.Lens' DockerVolumeConfiguration (Core.Maybe (Core.HashMap Types.String Types.String))
dvcLabels = Lens.field @"labels"
{-# DEPRECATED dvcLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcScope :: Lens.Lens' DockerVolumeConfiguration (Core.Maybe Types.Scope)
dvcScope = Lens.field @"scope"
{-# DEPRECATED dvcScope "Use generic-lens or generic-optics with 'scope' instead." #-}

instance Core.FromJSON DockerVolumeConfiguration where
  toJSON DockerVolumeConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ ("autoprovision" Core..=) Core.<$> autoprovision,
            ("driver" Core..=) Core.<$> driver,
            ("driverOpts" Core..=) Core.<$> driverOpts,
            ("labels" Core..=) Core.<$> labels,
            ("scope" Core..=) Core.<$> scope
          ]
      )

instance Core.FromJSON DockerVolumeConfiguration where
  parseJSON =
    Core.withObject "DockerVolumeConfiguration" Core.$
      \x ->
        DockerVolumeConfiguration'
          Core.<$> (x Core..:? "autoprovision")
          Core.<*> (x Core..:? "driver")
          Core.<*> (x Core..:? "driverOpts")
          Core.<*> (x Core..:? "labels")
          Core.<*> (x Core..:? "scope")
