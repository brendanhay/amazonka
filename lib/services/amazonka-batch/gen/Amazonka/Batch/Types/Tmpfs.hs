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
-- Module      : Amazonka.Batch.Types.Tmpfs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.Tmpfs where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The container path, mount options, and size of the @tmpfs@ mount.
--
-- This object isn\'t applicable to jobs that are running on Fargate
-- resources.
--
-- /See:/ 'newTmpfs' smart constructor.
data Tmpfs = Tmpfs'
  { -- | The list of @tmpfs@ volume mount options.
    --
    -- Valid values: \"@defaults@\" | \"@ro@\" | \"@rw@\" | \"@suid@\" |
    -- \"@nosuid@\" | \"@dev@\" | \"@nodev@\" | \"@exec@\" | \"@noexec@\" |
    -- \"@sync@\" | \"@async@\" | \"@dirsync@\" | \"@remount@\" | \"@mand@\" |
    -- \"@nomand@\" | \"@atime@\" | \"@noatime@\" | \"@diratime@\" |
    -- \"@nodiratime@\" | \"@bind@\" |
    -- \"@rbind\" | \"unbindable\" | \"runbindable\" | \"private\" | \"rprivate\" | \"shared\" | \"rshared\" | \"slave\" | \"rslave\" | \"relatime@\"
    -- | \"@norelatime@\" | \"@strictatime@\" | \"@nostrictatime@\" |
    -- \"@mode@\" | \"@uid@\" | \"@gid@\" | \"@nr_inodes@\" | \"@nr_blocks@\" |
    -- \"@mpol@\"
    mountOptions :: Prelude.Maybe [Prelude.Text],
    -- | The absolute file path in the container where the @tmpfs@ volume is
    -- mounted.
    containerPath :: Prelude.Text,
    -- | The size (in MiB) of the @tmpfs@ volume.
    size :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Tmpfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountOptions', 'tmpfs_mountOptions' - The list of @tmpfs@ volume mount options.
--
-- Valid values: \"@defaults@\" | \"@ro@\" | \"@rw@\" | \"@suid@\" |
-- \"@nosuid@\" | \"@dev@\" | \"@nodev@\" | \"@exec@\" | \"@noexec@\" |
-- \"@sync@\" | \"@async@\" | \"@dirsync@\" | \"@remount@\" | \"@mand@\" |
-- \"@nomand@\" | \"@atime@\" | \"@noatime@\" | \"@diratime@\" |
-- \"@nodiratime@\" | \"@bind@\" |
-- \"@rbind\" | \"unbindable\" | \"runbindable\" | \"private\" | \"rprivate\" | \"shared\" | \"rshared\" | \"slave\" | \"rslave\" | \"relatime@\"
-- | \"@norelatime@\" | \"@strictatime@\" | \"@nostrictatime@\" |
-- \"@mode@\" | \"@uid@\" | \"@gid@\" | \"@nr_inodes@\" | \"@nr_blocks@\" |
-- \"@mpol@\"
--
-- 'containerPath', 'tmpfs_containerPath' - The absolute file path in the container where the @tmpfs@ volume is
-- mounted.
--
-- 'size', 'tmpfs_size' - The size (in MiB) of the @tmpfs@ volume.
newTmpfs ::
  -- | 'containerPath'
  Prelude.Text ->
  -- | 'size'
  Prelude.Int ->
  Tmpfs
newTmpfs pContainerPath_ pSize_ =
  Tmpfs'
    { mountOptions = Prelude.Nothing,
      containerPath = pContainerPath_,
      size = pSize_
    }

-- | The list of @tmpfs@ volume mount options.
--
-- Valid values: \"@defaults@\" | \"@ro@\" | \"@rw@\" | \"@suid@\" |
-- \"@nosuid@\" | \"@dev@\" | \"@nodev@\" | \"@exec@\" | \"@noexec@\" |
-- \"@sync@\" | \"@async@\" | \"@dirsync@\" | \"@remount@\" | \"@mand@\" |
-- \"@nomand@\" | \"@atime@\" | \"@noatime@\" | \"@diratime@\" |
-- \"@nodiratime@\" | \"@bind@\" |
-- \"@rbind\" | \"unbindable\" | \"runbindable\" | \"private\" | \"rprivate\" | \"shared\" | \"rshared\" | \"slave\" | \"rslave\" | \"relatime@\"
-- | \"@norelatime@\" | \"@strictatime@\" | \"@nostrictatime@\" |
-- \"@mode@\" | \"@uid@\" | \"@gid@\" | \"@nr_inodes@\" | \"@nr_blocks@\" |
-- \"@mpol@\"
tmpfs_mountOptions :: Lens.Lens' Tmpfs (Prelude.Maybe [Prelude.Text])
tmpfs_mountOptions = Lens.lens (\Tmpfs' {mountOptions} -> mountOptions) (\s@Tmpfs' {} a -> s {mountOptions = a} :: Tmpfs) Prelude.. Lens.mapping Lens.coerced

-- | The absolute file path in the container where the @tmpfs@ volume is
-- mounted.
tmpfs_containerPath :: Lens.Lens' Tmpfs Prelude.Text
tmpfs_containerPath = Lens.lens (\Tmpfs' {containerPath} -> containerPath) (\s@Tmpfs' {} a -> s {containerPath = a} :: Tmpfs)

-- | The size (in MiB) of the @tmpfs@ volume.
tmpfs_size :: Lens.Lens' Tmpfs Prelude.Int
tmpfs_size = Lens.lens (\Tmpfs' {size} -> size) (\s@Tmpfs' {} a -> s {size = a} :: Tmpfs)

instance Data.FromJSON Tmpfs where
  parseJSON =
    Data.withObject
      "Tmpfs"
      ( \x ->
          Tmpfs'
            Prelude.<$> (x Data..:? "mountOptions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "containerPath")
            Prelude.<*> (x Data..: "size")
      )

instance Prelude.Hashable Tmpfs where
  hashWithSalt _salt Tmpfs' {..} =
    _salt
      `Prelude.hashWithSalt` mountOptions
      `Prelude.hashWithSalt` containerPath
      `Prelude.hashWithSalt` size

instance Prelude.NFData Tmpfs where
  rnf Tmpfs' {..} =
    Prelude.rnf mountOptions `Prelude.seq`
      Prelude.rnf containerPath `Prelude.seq`
        Prelude.rnf size

instance Data.ToJSON Tmpfs where
  toJSON Tmpfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("mountOptions" Data..=) Prelude.<$> mountOptions,
            Prelude.Just ("containerPath" Data..= containerPath),
            Prelude.Just ("size" Data..= size)
          ]
      )
