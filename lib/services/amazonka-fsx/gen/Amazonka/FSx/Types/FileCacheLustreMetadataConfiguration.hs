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
-- Module      : Amazonka.FSx.Types.FileCacheLustreMetadataConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileCacheLustreMetadataConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a Lustre MDT (Metadata Target) storage volume. The
-- metadata on Amazon File Cache is managed by a Lustre Metadata Server
-- (MDS) while the actual metadata is persisted on an MDT.
--
-- /See:/ 'newFileCacheLustreMetadataConfiguration' smart constructor.
data FileCacheLustreMetadataConfiguration = FileCacheLustreMetadataConfiguration'
  { -- | The storage capacity of the Lustre MDT (Metadata Target) storage volume
    -- in gibibytes (GiB). The only supported value is @2400@ GiB.
    storageCapacity :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileCacheLustreMetadataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageCapacity', 'fileCacheLustreMetadataConfiguration_storageCapacity' - The storage capacity of the Lustre MDT (Metadata Target) storage volume
-- in gibibytes (GiB). The only supported value is @2400@ GiB.
newFileCacheLustreMetadataConfiguration ::
  -- | 'storageCapacity'
  Prelude.Natural ->
  FileCacheLustreMetadataConfiguration
newFileCacheLustreMetadataConfiguration
  pStorageCapacity_ =
    FileCacheLustreMetadataConfiguration'
      { storageCapacity =
          pStorageCapacity_
      }

-- | The storage capacity of the Lustre MDT (Metadata Target) storage volume
-- in gibibytes (GiB). The only supported value is @2400@ GiB.
fileCacheLustreMetadataConfiguration_storageCapacity :: Lens.Lens' FileCacheLustreMetadataConfiguration Prelude.Natural
fileCacheLustreMetadataConfiguration_storageCapacity = Lens.lens (\FileCacheLustreMetadataConfiguration' {storageCapacity} -> storageCapacity) (\s@FileCacheLustreMetadataConfiguration' {} a -> s {storageCapacity = a} :: FileCacheLustreMetadataConfiguration)

instance
  Data.FromJSON
    FileCacheLustreMetadataConfiguration
  where
  parseJSON =
    Data.withObject
      "FileCacheLustreMetadataConfiguration"
      ( \x ->
          FileCacheLustreMetadataConfiguration'
            Prelude.<$> (x Data..: "StorageCapacity")
      )

instance
  Prelude.Hashable
    FileCacheLustreMetadataConfiguration
  where
  hashWithSalt
    _salt
    FileCacheLustreMetadataConfiguration' {..} =
      _salt `Prelude.hashWithSalt` storageCapacity

instance
  Prelude.NFData
    FileCacheLustreMetadataConfiguration
  where
  rnf FileCacheLustreMetadataConfiguration' {..} =
    Prelude.rnf storageCapacity

instance
  Data.ToJSON
    FileCacheLustreMetadataConfiguration
  where
  toJSON FileCacheLustreMetadataConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StorageCapacity" Data..= storageCapacity)
          ]
      )
