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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionVolumesHostDetails

-- | A data volume to mount from another container.
--
-- /See:/ 'newAwsEcsTaskDefinitionVolumesDetails' smart constructor.
data AwsEcsTaskDefinitionVolumesDetails = AwsEcsTaskDefinitionVolumesDetails'
  { -- | Information about the Amazon Elastic File System file system that is
    -- used for task storage.
    efsVolumeConfiguration :: Prelude.Maybe AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails,
    -- | The name of the data volume.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about a bind mount host volume.
    host :: Prelude.Maybe AwsEcsTaskDefinitionVolumesHostDetails,
    -- | Information about a Docker volume.
    dockerVolumeConfiguration :: Prelude.Maybe AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionVolumesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'efsVolumeConfiguration', 'awsEcsTaskDefinitionVolumesDetails_efsVolumeConfiguration' - Information about the Amazon Elastic File System file system that is
-- used for task storage.
--
-- 'name', 'awsEcsTaskDefinitionVolumesDetails_name' - The name of the data volume.
--
-- 'host', 'awsEcsTaskDefinitionVolumesDetails_host' - Information about a bind mount host volume.
--
-- 'dockerVolumeConfiguration', 'awsEcsTaskDefinitionVolumesDetails_dockerVolumeConfiguration' - Information about a Docker volume.
newAwsEcsTaskDefinitionVolumesDetails ::
  AwsEcsTaskDefinitionVolumesDetails
newAwsEcsTaskDefinitionVolumesDetails =
  AwsEcsTaskDefinitionVolumesDetails'
    { efsVolumeConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      host = Prelude.Nothing,
      dockerVolumeConfiguration =
        Prelude.Nothing
    }

-- | Information about the Amazon Elastic File System file system that is
-- used for task storage.
awsEcsTaskDefinitionVolumesDetails_efsVolumeConfiguration :: Lens.Lens' AwsEcsTaskDefinitionVolumesDetails (Prelude.Maybe AwsEcsTaskDefinitionVolumesEfsVolumeConfigurationDetails)
awsEcsTaskDefinitionVolumesDetails_efsVolumeConfiguration = Lens.lens (\AwsEcsTaskDefinitionVolumesDetails' {efsVolumeConfiguration} -> efsVolumeConfiguration) (\s@AwsEcsTaskDefinitionVolumesDetails' {} a -> s {efsVolumeConfiguration = a} :: AwsEcsTaskDefinitionVolumesDetails)

-- | The name of the data volume.
awsEcsTaskDefinitionVolumesDetails_name :: Lens.Lens' AwsEcsTaskDefinitionVolumesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionVolumesDetails_name = Lens.lens (\AwsEcsTaskDefinitionVolumesDetails' {name} -> name) (\s@AwsEcsTaskDefinitionVolumesDetails' {} a -> s {name = a} :: AwsEcsTaskDefinitionVolumesDetails)

-- | Information about a bind mount host volume.
awsEcsTaskDefinitionVolumesDetails_host :: Lens.Lens' AwsEcsTaskDefinitionVolumesDetails (Prelude.Maybe AwsEcsTaskDefinitionVolumesHostDetails)
awsEcsTaskDefinitionVolumesDetails_host = Lens.lens (\AwsEcsTaskDefinitionVolumesDetails' {host} -> host) (\s@AwsEcsTaskDefinitionVolumesDetails' {} a -> s {host = a} :: AwsEcsTaskDefinitionVolumesDetails)

-- | Information about a Docker volume.
awsEcsTaskDefinitionVolumesDetails_dockerVolumeConfiguration :: Lens.Lens' AwsEcsTaskDefinitionVolumesDetails (Prelude.Maybe AwsEcsTaskDefinitionVolumesDockerVolumeConfigurationDetails)
awsEcsTaskDefinitionVolumesDetails_dockerVolumeConfiguration = Lens.lens (\AwsEcsTaskDefinitionVolumesDetails' {dockerVolumeConfiguration} -> dockerVolumeConfiguration) (\s@AwsEcsTaskDefinitionVolumesDetails' {} a -> s {dockerVolumeConfiguration = a} :: AwsEcsTaskDefinitionVolumesDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionVolumesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionVolumesDetails"
      ( \x ->
          AwsEcsTaskDefinitionVolumesDetails'
            Prelude.<$> (x Data..:? "EfsVolumeConfiguration")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Host")
            Prelude.<*> (x Data..:? "DockerVolumeConfiguration")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionVolumesDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionVolumesDetails' {..} =
      _salt `Prelude.hashWithSalt` efsVolumeConfiguration
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` host
        `Prelude.hashWithSalt` dockerVolumeConfiguration

instance
  Prelude.NFData
    AwsEcsTaskDefinitionVolumesDetails
  where
  rnf AwsEcsTaskDefinitionVolumesDetails' {..} =
    Prelude.rnf efsVolumeConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf host
      `Prelude.seq` Prelude.rnf dockerVolumeConfiguration

instance
  Data.ToJSON
    AwsEcsTaskDefinitionVolumesDetails
  where
  toJSON AwsEcsTaskDefinitionVolumesDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EfsVolumeConfiguration" Data..=)
              Prelude.<$> efsVolumeConfiguration,
            ("Name" Data..=) Prelude.<$> name,
            ("Host" Data..=) Prelude.<$> host,
            ("DockerVolumeConfiguration" Data..=)
              Prelude.<$> dockerVolumeConfiguration
          ]
      )
