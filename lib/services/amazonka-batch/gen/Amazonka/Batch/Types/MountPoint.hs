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
-- Module      : Amazonka.Batch.Types.MountPoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.MountPoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details for a Docker volume mount point that\'s used in a job\'s
-- container properties. This parameter maps to @Volumes@ in the
-- <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.19/#create-a-container Create a container>
-- section of the /Docker Remote API/ and the @--volume@ option to docker
-- run.
--
-- /See:/ 'newMountPoint' smart constructor.
data MountPoint = MountPoint'
  { -- | The path on the container where the host volume is mounted.
    containerPath :: Prelude.Maybe Prelude.Text,
    -- | If this value is @true@, the container has read-only access to the
    -- volume. Otherwise, the container can write to the volume. The default
    -- value is @false@.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of the volume to mount.
    sourceVolume :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MountPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerPath', 'mountPoint_containerPath' - The path on the container where the host volume is mounted.
--
-- 'readOnly', 'mountPoint_readOnly' - If this value is @true@, the container has read-only access to the
-- volume. Otherwise, the container can write to the volume. The default
-- value is @false@.
--
-- 'sourceVolume', 'mountPoint_sourceVolume' - The name of the volume to mount.
newMountPoint ::
  MountPoint
newMountPoint =
  MountPoint'
    { containerPath = Prelude.Nothing,
      readOnly = Prelude.Nothing,
      sourceVolume = Prelude.Nothing
    }

-- | The path on the container where the host volume is mounted.
mountPoint_containerPath :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Text)
mountPoint_containerPath = Lens.lens (\MountPoint' {containerPath} -> containerPath) (\s@MountPoint' {} a -> s {containerPath = a} :: MountPoint)

-- | If this value is @true@, the container has read-only access to the
-- volume. Otherwise, the container can write to the volume. The default
-- value is @false@.
mountPoint_readOnly :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Bool)
mountPoint_readOnly = Lens.lens (\MountPoint' {readOnly} -> readOnly) (\s@MountPoint' {} a -> s {readOnly = a} :: MountPoint)

-- | The name of the volume to mount.
mountPoint_sourceVolume :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Text)
mountPoint_sourceVolume = Lens.lens (\MountPoint' {sourceVolume} -> sourceVolume) (\s@MountPoint' {} a -> s {sourceVolume = a} :: MountPoint)

instance Data.FromJSON MountPoint where
  parseJSON =
    Data.withObject
      "MountPoint"
      ( \x ->
          MountPoint'
            Prelude.<$> (x Data..:? "containerPath")
            Prelude.<*> (x Data..:? "readOnly")
            Prelude.<*> (x Data..:? "sourceVolume")
      )

instance Prelude.Hashable MountPoint where
  hashWithSalt _salt MountPoint' {..} =
    _salt `Prelude.hashWithSalt` containerPath
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` sourceVolume

instance Prelude.NFData MountPoint where
  rnf MountPoint' {..} =
    Prelude.rnf containerPath
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf sourceVolume

instance Data.ToJSON MountPoint where
  toJSON MountPoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("containerPath" Data..=) Prelude.<$> containerPath,
            ("readOnly" Data..=) Prelude.<$> readOnly,
            ("sourceVolume" Data..=) Prelude.<$> sourceVolume
          ]
      )
