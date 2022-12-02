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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a Docker volume.
--
-- /See:/ 'newAwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' smart constructor.
data AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails = AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails'
  { -- | A map of Docker driver-specific options that are passed through.
    driverOpts :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Whether to create the Docker volume automatically if it does not already
    -- exist.
    autoprovision :: Prelude.Maybe Prelude.Bool,
    -- | Custom metadata to add to the Docker volume.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The scope for the Docker volume that determines its lifecycle. Docker
    -- volumes that are scoped to a task are provisioned automatically when the
    -- task starts and destroyed when the task stops. Docker volumes that are
    -- shared persist after the task stops. Valid values are @shared@ or
    -- @task@.
    scope :: Prelude.Maybe Prelude.Text,
    -- | The Docker volume driver to use.
    driver :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'driverOpts', 'awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driverOpts' - A map of Docker driver-specific options that are passed through.
--
-- 'autoprovision', 'awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_autoprovision' - Whether to create the Docker volume automatically if it does not already
-- exist.
--
-- 'labels', 'awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_labels' - Custom metadata to add to the Docker volume.
--
-- 'scope', 'awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_scope' - The scope for the Docker volume that determines its lifecycle. Docker
-- volumes that are scoped to a task are provisioned automatically when the
-- task starts and destroyed when the task stops. Docker volumes that are
-- shared persist after the task stops. Valid values are @shared@ or
-- @task@.
--
-- 'driver', 'awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driver' - The Docker volume driver to use.
newAwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails ::
  AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
newAwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails =
  AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails'
    { driverOpts =
        Prelude.Nothing,
      autoprovision =
        Prelude.Nothing,
      labels =
        Prelude.Nothing,
      scope =
        Prelude.Nothing,
      driver =
        Prelude.Nothing
    }

-- | A map of Docker driver-specific options that are passed through.
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driverOpts :: Lens.Lens' AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driverOpts = Lens.lens (\AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {driverOpts} -> driverOpts) (\s@AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {} a -> s {driverOpts = a} :: AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether to create the Docker volume automatically if it does not already
-- exist.
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_autoprovision :: Lens.Lens' AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_autoprovision = Lens.lens (\AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {autoprovision} -> autoprovision) (\s@AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {} a -> s {autoprovision = a} :: AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails)

-- | Custom metadata to add to the Docker volume.
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_labels :: Lens.Lens' AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_labels = Lens.lens (\AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {labels} -> labels) (\s@AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {} a -> s {labels = a} :: AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails) Prelude.. Lens.mapping Lens.coerced

-- | The scope for the Docker volume that determines its lifecycle. Docker
-- volumes that are scoped to a task are provisioned automatically when the
-- task starts and destroyed when the task stops. Docker volumes that are
-- shared persist after the task stops. Valid values are @shared@ or
-- @task@.
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_scope :: Lens.Lens' AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_scope = Lens.lens (\AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {scope} -> scope) (\s@AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {} a -> s {scope = a} :: AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails)

-- | The Docker volume driver to use.
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driver :: Lens.Lens' AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails_driver = Lens.lens (\AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {driver} -> driver) (\s@AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {} a -> s {driver = a} :: AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails"
      ( \x ->
          AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails'
            Prelude.<$> (x Data..:? "DriverOpts" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Autoprovision")
              Prelude.<*> (x Data..:? "Labels" Data..!= Prelude.mempty)
              Prelude.<*> (x Data..:? "Scope")
              Prelude.<*> (x Data..:? "Driver")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {..} =
      _salt `Prelude.hashWithSalt` driverOpts
        `Prelude.hashWithSalt` autoprovision
        `Prelude.hashWithSalt` labels
        `Prelude.hashWithSalt` scope
        `Prelude.hashWithSalt` driver

instance
  Prelude.NFData
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
  where
  rnf
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {..} =
      Prelude.rnf driverOpts
        `Prelude.seq` Prelude.rnf autoprovision
        `Prelude.seq` Prelude.rnf labels
        `Prelude.seq` Prelude.rnf scope
        `Prelude.seq` Prelude.rnf driver

instance
  Data.ToJSON
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
  where
  toJSON
    AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DriverOpts" Data..=) Prelude.<$> driverOpts,
              ("Autoprovision" Data..=) Prelude.<$> autoprovision,
              ("Labels" Data..=) Prelude.<$> labels,
              ("Scope" Data..=) Prelude.<$> scope,
              ("Driver" Data..=) Prelude.<$> driver
            ]
        )
