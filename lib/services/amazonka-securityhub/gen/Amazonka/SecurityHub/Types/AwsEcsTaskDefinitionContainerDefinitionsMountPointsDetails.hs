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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A mount point for the data volumes in the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails = AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails'
  { -- | The path on the container to mount the host volume at.
    containerPath :: Prelude.Maybe Prelude.Text,
    -- | The name of the volume to mount. Must match the name of a volume listed
    -- in @VolumeDetails@ for the task definition.
    sourceVolume :: Prelude.Maybe Prelude.Text,
    -- | Whether the container has read-only access to the volume.
    readOnly :: Prelude.Maybe Prelude.Bool
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
-- 'sourceVolume', 'awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume' - The name of the volume to mount. Must match the name of a volume listed
-- in @VolumeDetails@ for the task definition.
--
-- 'readOnly', 'awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly' - Whether the container has read-only access to the volume.
newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
newAwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails'
    { containerPath =
        Prelude.Nothing,
      sourceVolume =
        Prelude.Nothing,
      readOnly =
        Prelude.Nothing
    }

-- | The path on the container to mount the host volume at.
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_containerPath = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {containerPath} -> containerPath) (\s@AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {} a -> s {containerPath = a} :: AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails)

-- | The name of the volume to mount. Must match the name of a volume listed
-- in @VolumeDetails@ for the task definition.
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_sourceVolume = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {sourceVolume} -> sourceVolume) (\s@AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {} a -> s {sourceVolume = a} :: AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails)

-- | Whether the container has read-only access to the volume.
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsMountPointsDetails_readOnly = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {readOnly} -> readOnly) (\s@AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {} a -> s {readOnly = a} :: AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails'
            Prelude.<$> (x Core..:? "ContainerPath")
              Prelude.<*> (x Core..:? "SourceVolume")
              Prelude.<*> (x Core..:? "ReadOnly")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  hashWithSalt
    _salt
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {..} =
      _salt `Prelude.hashWithSalt` containerPath
        `Prelude.hashWithSalt` sourceVolume
        `Prelude.hashWithSalt` readOnly

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {..} =
      Prelude.rnf containerPath
        `Prelude.seq` Prelude.rnf sourceVolume
        `Prelude.seq` Prelude.rnf readOnly

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ContainerPath" Core..=) Prelude.<$> containerPath,
              ("SourceVolume" Core..=) Prelude.<$> sourceVolume,
              ("ReadOnly" Core..=) Prelude.<$> readOnly
            ]
        )
