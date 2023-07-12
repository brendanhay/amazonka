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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A mount point for the data volumes in the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails = AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails'
  { -- | The path on the container to mount the host volume at.
    containerPath :: Prelude.Maybe Prelude.Text,
    -- | Whether the container has read-only access to the volume.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of the volume to mount. Must match the name of a volume listed
    -- in @VolumeDetails@ for the task definition.
    sourceVolume :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerPath', 'awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath' - The path on the container to mount the host volume at.
--
-- 'readOnly', 'awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly' - Whether the container has read-only access to the volume.
--
-- 'sourceVolume', 'awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume' - The name of the volume to mount. Must match the name of a volume listed
-- in @VolumeDetails@ for the task definition.
newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails'
    { containerPath =
        Prelude.Nothing,
      readOnly =
        Prelude.Nothing,
      sourceVolume =
        Prelude.Nothing
    }

-- | The path on the container to mount the host volume at.
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {containerPath} -> containerPath) (\s@AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {} a -> s {containerPath = a} :: AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails)

-- | Whether the container has read-only access to the volume.
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {readOnly} -> readOnly) (\s@AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {} a -> s {readOnly = a} :: AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails)

-- | The name of the volume to mount. Must match the name of a volume listed
-- in @VolumeDetails@ for the task definition.
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {sourceVolume} -> sourceVolume) (\s@AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {} a -> s {sourceVolume = a} :: AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails)

instance
  Data.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails'
            Prelude.<$> (x Data..:? "ContainerPath")
            Prelude.<*> (x Data..:? "ReadOnly")
            Prelude.<*> (x Data..:? "SourceVolume")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` containerPath
        `Prelude.hashWithSalt` readOnly
        `Prelude.hashWithSalt` sourceVolume

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {..} =
      Prelude.rnf containerPath
        `Prelude.seq` Prelude.rnf readOnly
        `Prelude.seq` Prelude.rnf sourceVolume

instance
  Data.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("ContainerPath" Data..=) Prelude.<$> containerPath,
              ("ReadOnly" Data..=) Prelude.<$> readOnly,
              ("SourceVolume" Data..=) Prelude.<$> sourceVolume
            ]
        )
