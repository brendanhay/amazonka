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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

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
    readOnly :: Core.Maybe Core.Bool,
    -- | The name of the volume to mount.
    sourceVolume :: Core.Maybe Core.Text,
    -- | The path on the container where the host volume is mounted.
    containerPath :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { readOnly = Core.Nothing,
      sourceVolume = Core.Nothing,
      containerPath = Core.Nothing
    }

-- | If this value is @true@, the container has read-only access to the
-- volume. Otherwise, the container can write to the volume. The default
-- value is @false@.
mountPoint_readOnly :: Lens.Lens' MountPoint (Core.Maybe Core.Bool)
mountPoint_readOnly = Lens.lens (\MountPoint' {readOnly} -> readOnly) (\s@MountPoint' {} a -> s {readOnly = a} :: MountPoint)

-- | The name of the volume to mount.
mountPoint_sourceVolume :: Lens.Lens' MountPoint (Core.Maybe Core.Text)
mountPoint_sourceVolume = Lens.lens (\MountPoint' {sourceVolume} -> sourceVolume) (\s@MountPoint' {} a -> s {sourceVolume = a} :: MountPoint)

-- | The path on the container where the host volume is mounted.
mountPoint_containerPath :: Lens.Lens' MountPoint (Core.Maybe Core.Text)
mountPoint_containerPath = Lens.lens (\MountPoint' {containerPath} -> containerPath) (\s@MountPoint' {} a -> s {containerPath = a} :: MountPoint)

instance Core.FromJSON MountPoint where
  parseJSON =
    Core.withObject
      "MountPoint"
      ( \x ->
          MountPoint'
            Core.<$> (x Core..:? "readOnly")
            Core.<*> (x Core..:? "sourceVolume")
            Core.<*> (x Core..:? "containerPath")
      )

instance Core.Hashable MountPoint

instance Core.NFData MountPoint

instance Core.ToJSON MountPoint where
  toJSON MountPoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("readOnly" Core..=) Core.<$> readOnly,
            ("sourceVolume" Core..=) Core.<$> sourceVolume,
            ("containerPath" Core..=) Core.<$> containerPath
          ]
      )
