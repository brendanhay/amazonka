{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Batch.Types.MountPoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.MountPoint where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details on a Docker volume mount point that\'s used in a job\'s
-- container properties. This parameter maps to @Volumes@ in the
-- <https://docs.docker.com/engine/reference/api/docker_remote_api_v1.19/#create-a-container Create a container>
-- section of the Docker Remote API and the @--volume@ option to docker
-- run.
--
-- /See:/ 'newMountPoint' smart constructor.
data MountPoint = MountPoint'
  { -- | If this value is @true@, the container has read-only access to the
    -- volume. Otherwise, the container can write to the volume. The default
    -- value is @false@.
    readOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of the volume to mount.
    sourceVolume :: Prelude.Maybe Prelude.Text,
    -- | The path on the container where the host volume is mounted.
    containerPath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MountPoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readOnly', 'mountPoint_readOnly' - If this value is @true@, the container has read-only access to the
-- volume. Otherwise, the container can write to the volume. The default
-- value is @false@.
--
-- 'sourceVolume', 'mountPoint_sourceVolume' - The name of the volume to mount.
--
-- 'containerPath', 'mountPoint_containerPath' - The path on the container where the host volume is mounted.
newMountPoint ::
  MountPoint
newMountPoint =
  MountPoint'
    { readOnly = Prelude.Nothing,
      sourceVolume = Prelude.Nothing,
      containerPath = Prelude.Nothing
    }

-- | If this value is @true@, the container has read-only access to the
-- volume. Otherwise, the container can write to the volume. The default
-- value is @false@.
mountPoint_readOnly :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Bool)
mountPoint_readOnly = Lens.lens (\MountPoint' {readOnly} -> readOnly) (\s@MountPoint' {} a -> s {readOnly = a} :: MountPoint)

-- | The name of the volume to mount.
mountPoint_sourceVolume :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Text)
mountPoint_sourceVolume = Lens.lens (\MountPoint' {sourceVolume} -> sourceVolume) (\s@MountPoint' {} a -> s {sourceVolume = a} :: MountPoint)

-- | The path on the container where the host volume is mounted.
mountPoint_containerPath :: Lens.Lens' MountPoint (Prelude.Maybe Prelude.Text)
mountPoint_containerPath = Lens.lens (\MountPoint' {containerPath} -> containerPath) (\s@MountPoint' {} a -> s {containerPath = a} :: MountPoint)

instance Prelude.FromJSON MountPoint where
  parseJSON =
    Prelude.withObject
      "MountPoint"
      ( \x ->
          MountPoint'
            Prelude.<$> (x Prelude..:? "readOnly")
            Prelude.<*> (x Prelude..:? "sourceVolume")
            Prelude.<*> (x Prelude..:? "containerPath")
      )

instance Prelude.Hashable MountPoint

instance Prelude.NFData MountPoint

instance Prelude.ToJSON MountPoint where
  toJSON MountPoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("readOnly" Prelude..=) Prelude.<$> readOnly,
            ("sourceVolume" Prelude..=) Prelude.<$> sourceVolume,
            ("containerPath" Prelude..=)
              Prelude.<$> containerPath
          ]
      )
