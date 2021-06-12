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
-- Module      : Network.AWS.ECS.Types.Tmpfs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Tmpfs where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The container path, mount options, and size of the tmpfs mount.
--
-- /See:/ 'newTmpfs' smart constructor.
data Tmpfs = Tmpfs'
  { -- | The list of tmpfs volume mount options.
    --
    -- Valid values:
    -- @\"defaults\" | \"ro\" | \"rw\" | \"suid\" | \"nosuid\" | \"dev\" | \"nodev\" | \"exec\" | \"noexec\" | \"sync\" | \"async\" | \"dirsync\" | \"remount\" | \"mand\" | \"nomand\" | \"atime\" | \"noatime\" | \"diratime\" | \"nodiratime\" | \"bind\" | \"rbind\" | \"unbindable\" | \"runbindable\" | \"private\" | \"rprivate\" | \"shared\" | \"rshared\" | \"slave\" | \"rslave\" | \"relatime\" | \"norelatime\" | \"strictatime\" | \"nostrictatime\" | \"mode\" | \"uid\" | \"gid\" | \"nr_inodes\" | \"nr_blocks\" | \"mpol\"@
    mountOptions :: Core.Maybe [Core.Text],
    -- | The absolute file path where the tmpfs volume is to be mounted.
    containerPath :: Core.Text,
    -- | The maximum size (in MiB) of the tmpfs volume.
    size :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tmpfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountOptions', 'tmpfs_mountOptions' - The list of tmpfs volume mount options.
--
-- Valid values:
-- @\"defaults\" | \"ro\" | \"rw\" | \"suid\" | \"nosuid\" | \"dev\" | \"nodev\" | \"exec\" | \"noexec\" | \"sync\" | \"async\" | \"dirsync\" | \"remount\" | \"mand\" | \"nomand\" | \"atime\" | \"noatime\" | \"diratime\" | \"nodiratime\" | \"bind\" | \"rbind\" | \"unbindable\" | \"runbindable\" | \"private\" | \"rprivate\" | \"shared\" | \"rshared\" | \"slave\" | \"rslave\" | \"relatime\" | \"norelatime\" | \"strictatime\" | \"nostrictatime\" | \"mode\" | \"uid\" | \"gid\" | \"nr_inodes\" | \"nr_blocks\" | \"mpol\"@
--
-- 'containerPath', 'tmpfs_containerPath' - The absolute file path where the tmpfs volume is to be mounted.
--
-- 'size', 'tmpfs_size' - The maximum size (in MiB) of the tmpfs volume.
newTmpfs ::
  -- | 'containerPath'
  Core.Text ->
  -- | 'size'
  Core.Int ->
  Tmpfs
newTmpfs pContainerPath_ pSize_ =
  Tmpfs'
    { mountOptions = Core.Nothing,
      containerPath = pContainerPath_,
      size = pSize_
    }

-- | The list of tmpfs volume mount options.
--
-- Valid values:
-- @\"defaults\" | \"ro\" | \"rw\" | \"suid\" | \"nosuid\" | \"dev\" | \"nodev\" | \"exec\" | \"noexec\" | \"sync\" | \"async\" | \"dirsync\" | \"remount\" | \"mand\" | \"nomand\" | \"atime\" | \"noatime\" | \"diratime\" | \"nodiratime\" | \"bind\" | \"rbind\" | \"unbindable\" | \"runbindable\" | \"private\" | \"rprivate\" | \"shared\" | \"rshared\" | \"slave\" | \"rslave\" | \"relatime\" | \"norelatime\" | \"strictatime\" | \"nostrictatime\" | \"mode\" | \"uid\" | \"gid\" | \"nr_inodes\" | \"nr_blocks\" | \"mpol\"@
tmpfs_mountOptions :: Lens.Lens' Tmpfs (Core.Maybe [Core.Text])
tmpfs_mountOptions = Lens.lens (\Tmpfs' {mountOptions} -> mountOptions) (\s@Tmpfs' {} a -> s {mountOptions = a} :: Tmpfs) Core.. Lens.mapping Lens._Coerce

-- | The absolute file path where the tmpfs volume is to be mounted.
tmpfs_containerPath :: Lens.Lens' Tmpfs Core.Text
tmpfs_containerPath = Lens.lens (\Tmpfs' {containerPath} -> containerPath) (\s@Tmpfs' {} a -> s {containerPath = a} :: Tmpfs)

-- | The maximum size (in MiB) of the tmpfs volume.
tmpfs_size :: Lens.Lens' Tmpfs Core.Int
tmpfs_size = Lens.lens (\Tmpfs' {size} -> size) (\s@Tmpfs' {} a -> s {size = a} :: Tmpfs)

instance Core.FromJSON Tmpfs where
  parseJSON =
    Core.withObject
      "Tmpfs"
      ( \x ->
          Tmpfs'
            Core.<$> (x Core..:? "mountOptions" Core..!= Core.mempty)
            Core.<*> (x Core..: "containerPath")
            Core.<*> (x Core..: "size")
      )

instance Core.Hashable Tmpfs

instance Core.NFData Tmpfs

instance Core.ToJSON Tmpfs where
  toJSON Tmpfs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("mountOptions" Core..=) Core.<$> mountOptions,
            Core.Just ("containerPath" Core..= containerPath),
            Core.Just ("size" Core..= size)
          ]
      )
