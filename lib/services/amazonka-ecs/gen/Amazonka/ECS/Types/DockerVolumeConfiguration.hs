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
-- Module      : Amazonka.ECS.Types.DockerVolumeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DockerVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.Scope
import qualified Amazonka.Prelude as Prelude

-- | This parameter is specified when you\'re using Docker volumes. Docker
-- volumes are only supported when you\'re using the EC2 launch type.
-- Windows containers only support the use of the @local@ driver. To use
-- bind mounts, specify a @host@ instead.
--
-- /See:/ 'newDockerVolumeConfiguration' smart constructor.
data DockerVolumeConfiguration = DockerVolumeConfiguration'
  { -- | If this value is @true@, the Docker volume is created if it doesn\'t
    -- already exist.
    --
    -- This field is only used if the @scope@ is @shared@.
    autoprovision :: Prelude.Maybe Prelude.Bool,
    -- | The Docker volume driver to use. The driver value must match the driver
    -- name provided by Docker because it is used for task placement. If the
    -- driver was installed using the Docker plugin CLI, use @docker plugin ls@
    -- to retrieve the driver name from your container instance. If the driver
    -- was installed using another method, use Docker plugin discovery to
    -- retrieve the driver name. For more information, see
    -- <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery>.
    -- This parameter maps to @Driver@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @xxdriver@ option to
    -- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
    driver :: Prelude.Maybe Prelude.Text,
    -- | A map of Docker driver-specific options passed through. This parameter
    -- maps to @DriverOpts@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @xxopt@ option to
    -- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
    driverOpts :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Custom metadata to add to your Docker volume. This parameter maps to
    -- @Labels@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @xxlabel@ option to
    -- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The scope for the Docker volume that determines its lifecycle. Docker
    -- volumes that are scoped to a @task@ are automatically provisioned when
    -- the task starts and destroyed when the task stops. Docker volumes that
    -- are scoped as @shared@ persist after the task stops.
    scope :: Prelude.Maybe Scope
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DockerVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoprovision', 'dockerVolumeConfiguration_autoprovision' - If this value is @true@, the Docker volume is created if it doesn\'t
-- already exist.
--
-- This field is only used if the @scope@ is @shared@.
--
-- 'driver', 'dockerVolumeConfiguration_driver' - The Docker volume driver to use. The driver value must match the driver
-- name provided by Docker because it is used for task placement. If the
-- driver was installed using the Docker plugin CLI, use @docker plugin ls@
-- to retrieve the driver name from your container instance. If the driver
-- was installed using another method, use Docker plugin discovery to
-- retrieve the driver name. For more information, see
-- <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery>.
-- This parameter maps to @Driver@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxdriver@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
--
-- 'driverOpts', 'dockerVolumeConfiguration_driverOpts' - A map of Docker driver-specific options passed through. This parameter
-- maps to @DriverOpts@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxopt@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
--
-- 'labels', 'dockerVolumeConfiguration_labels' - Custom metadata to add to your Docker volume. This parameter maps to
-- @Labels@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxlabel@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
--
-- 'scope', 'dockerVolumeConfiguration_scope' - The scope for the Docker volume that determines its lifecycle. Docker
-- volumes that are scoped to a @task@ are automatically provisioned when
-- the task starts and destroyed when the task stops. Docker volumes that
-- are scoped as @shared@ persist after the task stops.
newDockerVolumeConfiguration ::
  DockerVolumeConfiguration
newDockerVolumeConfiguration =
  DockerVolumeConfiguration'
    { autoprovision =
        Prelude.Nothing,
      driver = Prelude.Nothing,
      driverOpts = Prelude.Nothing,
      labels = Prelude.Nothing,
      scope = Prelude.Nothing
    }

-- | If this value is @true@, the Docker volume is created if it doesn\'t
-- already exist.
--
-- This field is only used if the @scope@ is @shared@.
dockerVolumeConfiguration_autoprovision :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe Prelude.Bool)
dockerVolumeConfiguration_autoprovision = Lens.lens (\DockerVolumeConfiguration' {autoprovision} -> autoprovision) (\s@DockerVolumeConfiguration' {} a -> s {autoprovision = a} :: DockerVolumeConfiguration)

-- | The Docker volume driver to use. The driver value must match the driver
-- name provided by Docker because it is used for task placement. If the
-- driver was installed using the Docker plugin CLI, use @docker plugin ls@
-- to retrieve the driver name from your container instance. If the driver
-- was installed using another method, use Docker plugin discovery to
-- retrieve the driver name. For more information, see
-- <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery>.
-- This parameter maps to @Driver@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxdriver@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
dockerVolumeConfiguration_driver :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe Prelude.Text)
dockerVolumeConfiguration_driver = Lens.lens (\DockerVolumeConfiguration' {driver} -> driver) (\s@DockerVolumeConfiguration' {} a -> s {driver = a} :: DockerVolumeConfiguration)

-- | A map of Docker driver-specific options passed through. This parameter
-- maps to @DriverOpts@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxopt@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
dockerVolumeConfiguration_driverOpts :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dockerVolumeConfiguration_driverOpts = Lens.lens (\DockerVolumeConfiguration' {driverOpts} -> driverOpts) (\s@DockerVolumeConfiguration' {} a -> s {driverOpts = a} :: DockerVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Custom metadata to add to your Docker volume. This parameter maps to
-- @Labels@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxlabel@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
dockerVolumeConfiguration_labels :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dockerVolumeConfiguration_labels = Lens.lens (\DockerVolumeConfiguration' {labels} -> labels) (\s@DockerVolumeConfiguration' {} a -> s {labels = a} :: DockerVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The scope for the Docker volume that determines its lifecycle. Docker
-- volumes that are scoped to a @task@ are automatically provisioned when
-- the task starts and destroyed when the task stops. Docker volumes that
-- are scoped as @shared@ persist after the task stops.
dockerVolumeConfiguration_scope :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe Scope)
dockerVolumeConfiguration_scope = Lens.lens (\DockerVolumeConfiguration' {scope} -> scope) (\s@DockerVolumeConfiguration' {} a -> s {scope = a} :: DockerVolumeConfiguration)

instance Data.FromJSON DockerVolumeConfiguration where
  parseJSON =
    Data.withObject
      "DockerVolumeConfiguration"
      ( \x ->
          DockerVolumeConfiguration'
            Prelude.<$> (x Data..:? "autoprovision")
            Prelude.<*> (x Data..:? "driver")
            Prelude.<*> (x Data..:? "driverOpts" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "scope")
      )

instance Prelude.Hashable DockerVolumeConfiguration where
  hashWithSalt _salt DockerVolumeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoprovision
      `Prelude.hashWithSalt` driver
      `Prelude.hashWithSalt` driverOpts
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` scope

instance Prelude.NFData DockerVolumeConfiguration where
  rnf DockerVolumeConfiguration' {..} =
    Prelude.rnf autoprovision `Prelude.seq`
      Prelude.rnf driver `Prelude.seq`
        Prelude.rnf driverOpts `Prelude.seq`
          Prelude.rnf labels `Prelude.seq`
            Prelude.rnf scope

instance Data.ToJSON DockerVolumeConfiguration where
  toJSON DockerVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("autoprovision" Data..=) Prelude.<$> autoprovision,
            ("driver" Data..=) Prelude.<$> driver,
            ("driverOpts" Data..=) Prelude.<$> driverOpts,
            ("labels" Data..=) Prelude.<$> labels,
            ("scope" Data..=) Prelude.<$> scope
          ]
      )
