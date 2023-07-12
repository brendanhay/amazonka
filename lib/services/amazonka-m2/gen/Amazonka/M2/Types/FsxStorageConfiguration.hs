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
-- Module      : Amazonka.M2.Types.FsxStorageConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.FsxStorageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the storage configuration for an Amazon FSx file system.
--
-- /See:/ 'newFsxStorageConfiguration' smart constructor.
data FsxStorageConfiguration = FsxStorageConfiguration'
  { -- | The file system identifier.
    fileSystemId :: Prelude.Text,
    -- | The mount point for the file system.
    mountPoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FsxStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'fsxStorageConfiguration_fileSystemId' - The file system identifier.
--
-- 'mountPoint', 'fsxStorageConfiguration_mountPoint' - The mount point for the file system.
newFsxStorageConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'mountPoint'
  Prelude.Text ->
  FsxStorageConfiguration
newFsxStorageConfiguration
  pFileSystemId_
  pMountPoint_ =
    FsxStorageConfiguration'
      { fileSystemId =
          pFileSystemId_,
        mountPoint = pMountPoint_
      }

-- | The file system identifier.
fsxStorageConfiguration_fileSystemId :: Lens.Lens' FsxStorageConfiguration Prelude.Text
fsxStorageConfiguration_fileSystemId = Lens.lens (\FsxStorageConfiguration' {fileSystemId} -> fileSystemId) (\s@FsxStorageConfiguration' {} a -> s {fileSystemId = a} :: FsxStorageConfiguration)

-- | The mount point for the file system.
fsxStorageConfiguration_mountPoint :: Lens.Lens' FsxStorageConfiguration Prelude.Text
fsxStorageConfiguration_mountPoint = Lens.lens (\FsxStorageConfiguration' {mountPoint} -> mountPoint) (\s@FsxStorageConfiguration' {} a -> s {mountPoint = a} :: FsxStorageConfiguration)

instance Data.FromJSON FsxStorageConfiguration where
  parseJSON =
    Data.withObject
      "FsxStorageConfiguration"
      ( \x ->
          FsxStorageConfiguration'
            Prelude.<$> (x Data..: "file-system-id")
            Prelude.<*> (x Data..: "mount-point")
      )

instance Prelude.Hashable FsxStorageConfiguration where
  hashWithSalt _salt FsxStorageConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` mountPoint

instance Prelude.NFData FsxStorageConfiguration where
  rnf FsxStorageConfiguration' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf mountPoint

instance Data.ToJSON FsxStorageConfiguration where
  toJSON FsxStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("file-system-id" Data..= fileSystemId),
            Prelude.Just ("mount-point" Data..= mountPoint)
          ]
      )
