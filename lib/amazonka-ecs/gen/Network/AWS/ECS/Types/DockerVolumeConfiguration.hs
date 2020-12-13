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
    dvcDriverOpts,
    dvcDriver,
    dvcScope,
    dvcLabels,
    dvcAutoprovision,
  )
where

import Network.AWS.ECS.Types.Scope
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This parameter is specified when you are using Docker volumes. Docker volumes are only supported when you are using the EC2 launch type. Windows containers only support the use of the @local@ driver. To use bind mounts, specify a @host@ instead.
--
-- /See:/ 'mkDockerVolumeConfiguration' smart constructor.
data DockerVolumeConfiguration = DockerVolumeConfiguration'
  { -- | A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
    driverOpts :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
    driver :: Lude.Maybe Lude.Text,
    -- | The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
    scope :: Lude.Maybe Scope,
    -- | Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
    labels :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | If this value is @true@ , the Docker volume is created if it does not already exist.
    autoprovision :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DockerVolumeConfiguration' with the minimum fields required to make a request.
--
-- * 'driverOpts' - A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
-- * 'driver' - The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
-- * 'scope' - The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
-- * 'labels' - Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
-- * 'autoprovision' - If this value is @true@ , the Docker volume is created if it does not already exist.
mkDockerVolumeConfiguration ::
  DockerVolumeConfiguration
mkDockerVolumeConfiguration =
  DockerVolumeConfiguration'
    { driverOpts = Lude.Nothing,
      driver = Lude.Nothing,
      scope = Lude.Nothing,
      labels = Lude.Nothing,
      autoprovision = Lude.Nothing
    }

-- | A map of Docker driver-specific options passed through. This parameter maps to @DriverOpts@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxopt@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- /Note:/ Consider using 'driverOpts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcDriverOpts :: Lens.Lens' DockerVolumeConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dvcDriverOpts = Lens.lens (driverOpts :: DockerVolumeConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {driverOpts = a} :: DockerVolumeConfiguration)
{-# DEPRECATED dvcDriverOpts "Use generic-lens or generic-optics with 'driverOpts' instead." #-}

-- | The Docker volume driver to use. The driver value must match the driver name provided by Docker because it is used for task placement. If the driver was installed using the Docker plugin CLI, use @docker plugin ls@ to retrieve the driver name from your container instance. If the driver was installed using another method, use Docker plugin discovery to retrieve the driver name. For more information, see <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery> . This parameter maps to @Driver@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxdriver@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- /Note:/ Consider using 'driver' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcDriver :: Lens.Lens' DockerVolumeConfiguration (Lude.Maybe Lude.Text)
dvcDriver = Lens.lens (driver :: DockerVolumeConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {driver = a} :: DockerVolumeConfiguration)
{-# DEPRECATED dvcDriver "Use generic-lens or generic-optics with 'driver' instead." #-}

-- | The scope for the Docker volume that determines its lifecycle. Docker volumes that are scoped to a @task@ are automatically provisioned when the task starts and destroyed when the task stops. Docker volumes that are scoped as @shared@ persist after the task stops.
--
-- /Note:/ Consider using 'scope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcScope :: Lens.Lens' DockerVolumeConfiguration (Lude.Maybe Scope)
dvcScope = Lens.lens (scope :: DockerVolumeConfiguration -> Lude.Maybe Scope) (\s a -> s {scope = a} :: DockerVolumeConfiguration)
{-# DEPRECATED dvcScope "Use generic-lens or generic-optics with 'scope' instead." #-}

-- | Custom metadata to add to your Docker volume. This parameter maps to @Labels@ in the <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @xxlabel@ option to <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create> .
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcLabels :: Lens.Lens' DockerVolumeConfiguration (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dvcLabels = Lens.lens (labels :: DockerVolumeConfiguration -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {labels = a} :: DockerVolumeConfiguration)
{-# DEPRECATED dvcLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | If this value is @true@ , the Docker volume is created if it does not already exist.
--
-- /Note:/ Consider using 'autoprovision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvcAutoprovision :: Lens.Lens' DockerVolumeConfiguration (Lude.Maybe Lude.Bool)
dvcAutoprovision = Lens.lens (autoprovision :: DockerVolumeConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {autoprovision = a} :: DockerVolumeConfiguration)
{-# DEPRECATED dvcAutoprovision "Use generic-lens or generic-optics with 'autoprovision' instead." #-}

instance Lude.FromJSON DockerVolumeConfiguration where
  parseJSON =
    Lude.withObject
      "DockerVolumeConfiguration"
      ( \x ->
          DockerVolumeConfiguration'
            Lude.<$> (x Lude..:? "driverOpts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "driver")
            Lude.<*> (x Lude..:? "scope")
            Lude.<*> (x Lude..:? "labels" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "autoprovision")
      )

instance Lude.ToJSON DockerVolumeConfiguration where
  toJSON DockerVolumeConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("driverOpts" Lude..=) Lude.<$> driverOpts,
            ("driver" Lude..=) Lude.<$> driver,
            ("scope" Lude..=) Lude.<$> scope,
            ("labels" Lude..=) Lude.<$> labels,
            ("autoprovision" Lude..=) Lude.<$> autoprovision
          ]
      )
