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
-- Module      : Amazonka.FSx.Types.FileSystemEndpoints
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.FileSystemEndpoints where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.FileSystemEndpoint
import qualified Amazonka.Prelude as Prelude

-- | An Amazon FSx for NetApp ONTAP file system has the following endpoints
-- that are used to access data or to manage the file system using the
-- NetApp ONTAP CLI, REST API, or NetApp SnapMirror.
--
-- /See:/ 'newFileSystemEndpoints' smart constructor.
data FileSystemEndpoints = FileSystemEndpoints'
  { -- | An endpoint for managing your file system by setting up NetApp
    -- SnapMirror with other ONTAP systems.
    intercluster :: Prelude.Maybe FileSystemEndpoint,
    -- | An endpoint for managing your file system using the NetApp ONTAP CLI and
    -- NetApp ONTAP API.
    management :: Prelude.Maybe FileSystemEndpoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'intercluster', 'fileSystemEndpoints_intercluster' - An endpoint for managing your file system by setting up NetApp
-- SnapMirror with other ONTAP systems.
--
-- 'management', 'fileSystemEndpoints_management' - An endpoint for managing your file system using the NetApp ONTAP CLI and
-- NetApp ONTAP API.
newFileSystemEndpoints ::
  FileSystemEndpoints
newFileSystemEndpoints =
  FileSystemEndpoints'
    { intercluster =
        Prelude.Nothing,
      management = Prelude.Nothing
    }

-- | An endpoint for managing your file system by setting up NetApp
-- SnapMirror with other ONTAP systems.
fileSystemEndpoints_intercluster :: Lens.Lens' FileSystemEndpoints (Prelude.Maybe FileSystemEndpoint)
fileSystemEndpoints_intercluster = Lens.lens (\FileSystemEndpoints' {intercluster} -> intercluster) (\s@FileSystemEndpoints' {} a -> s {intercluster = a} :: FileSystemEndpoints)

-- | An endpoint for managing your file system using the NetApp ONTAP CLI and
-- NetApp ONTAP API.
fileSystemEndpoints_management :: Lens.Lens' FileSystemEndpoints (Prelude.Maybe FileSystemEndpoint)
fileSystemEndpoints_management = Lens.lens (\FileSystemEndpoints' {management} -> management) (\s@FileSystemEndpoints' {} a -> s {management = a} :: FileSystemEndpoints)

instance Data.FromJSON FileSystemEndpoints where
  parseJSON =
    Data.withObject
      "FileSystemEndpoints"
      ( \x ->
          FileSystemEndpoints'
            Prelude.<$> (x Data..:? "Intercluster")
            Prelude.<*> (x Data..:? "Management")
      )

instance Prelude.Hashable FileSystemEndpoints where
  hashWithSalt _salt FileSystemEndpoints' {..} =
    _salt
      `Prelude.hashWithSalt` intercluster
      `Prelude.hashWithSalt` management

instance Prelude.NFData FileSystemEndpoints where
  rnf FileSystemEndpoints' {..} =
    Prelude.rnf intercluster `Prelude.seq`
      Prelude.rnf management
