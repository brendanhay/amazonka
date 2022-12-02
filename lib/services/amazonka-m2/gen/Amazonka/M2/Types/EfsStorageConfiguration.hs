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
-- Module      : Amazonka.M2.Types.EfsStorageConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.M2.Types.EfsStorageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the storage configuration for an Amazon EFS file system.
--
-- /See:/ 'newEfsStorageConfiguration' smart constructor.
data EfsStorageConfiguration = EfsStorageConfiguration'
  { -- | The file system identifier.
    fileSystemId :: Prelude.Text,
    -- | The mount point for the file system.
    mountPoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EfsStorageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'efsStorageConfiguration_fileSystemId' - The file system identifier.
--
-- 'mountPoint', 'efsStorageConfiguration_mountPoint' - The mount point for the file system.
newEfsStorageConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'mountPoint'
  Prelude.Text ->
  EfsStorageConfiguration
newEfsStorageConfiguration
  pFileSystemId_
  pMountPoint_ =
    EfsStorageConfiguration'
      { fileSystemId =
          pFileSystemId_,
        mountPoint = pMountPoint_
      }

-- | The file system identifier.
efsStorageConfiguration_fileSystemId :: Lens.Lens' EfsStorageConfiguration Prelude.Text
efsStorageConfiguration_fileSystemId = Lens.lens (\EfsStorageConfiguration' {fileSystemId} -> fileSystemId) (\s@EfsStorageConfiguration' {} a -> s {fileSystemId = a} :: EfsStorageConfiguration)

-- | The mount point for the file system.
efsStorageConfiguration_mountPoint :: Lens.Lens' EfsStorageConfiguration Prelude.Text
efsStorageConfiguration_mountPoint = Lens.lens (\EfsStorageConfiguration' {mountPoint} -> mountPoint) (\s@EfsStorageConfiguration' {} a -> s {mountPoint = a} :: EfsStorageConfiguration)

instance Data.FromJSON EfsStorageConfiguration where
  parseJSON =
    Data.withObject
      "EfsStorageConfiguration"
      ( \x ->
          EfsStorageConfiguration'
            Prelude.<$> (x Data..: "file-system-id")
            Prelude.<*> (x Data..: "mount-point")
      )

instance Prelude.Hashable EfsStorageConfiguration where
  hashWithSalt _salt EfsStorageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` mountPoint

instance Prelude.NFData EfsStorageConfiguration where
  rnf EfsStorageConfiguration' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf mountPoint

instance Data.ToJSON EfsStorageConfiguration where
  toJSON EfsStorageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("file-system-id" Data..= fileSystemId),
            Prelude.Just ("mount-point" Data..= mountPoint)
          ]
      )
