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
-- Module      : Amazonka.Nimble.Types.SharedFileSystemConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.SharedFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a shared file storage system that is associated
-- with a studio resource.
--
-- /See:/ 'newSharedFileSystemConfiguration' smart constructor.
data SharedFileSystemConfiguration = SharedFileSystemConfiguration'
  { -- | The endpoint of the shared file system that is accessed by the studio
    -- component resource.
    endpoint :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The unique identifier for a file system.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The mount location for a shared file system on a Linux virtual
    -- workstation.
    linuxMountPoint :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The name of the file share.
    shareName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The mount location for a shared file system on a Windows virtual
    -- workstation.
    windowsMountDrive :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SharedFileSystemConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'sharedFileSystemConfiguration_endpoint' - The endpoint of the shared file system that is accessed by the studio
-- component resource.
--
-- 'fileSystemId', 'sharedFileSystemConfiguration_fileSystemId' - The unique identifier for a file system.
--
-- 'linuxMountPoint', 'sharedFileSystemConfiguration_linuxMountPoint' - The mount location for a shared file system on a Linux virtual
-- workstation.
--
-- 'shareName', 'sharedFileSystemConfiguration_shareName' - The name of the file share.
--
-- 'windowsMountDrive', 'sharedFileSystemConfiguration_windowsMountDrive' - The mount location for a shared file system on a Windows virtual
-- workstation.
newSharedFileSystemConfiguration ::
  SharedFileSystemConfiguration
newSharedFileSystemConfiguration =
  SharedFileSystemConfiguration'
    { endpoint =
        Prelude.Nothing,
      fileSystemId = Prelude.Nothing,
      linuxMountPoint = Prelude.Nothing,
      shareName = Prelude.Nothing,
      windowsMountDrive = Prelude.Nothing
    }

-- | The endpoint of the shared file system that is accessed by the studio
-- component resource.
sharedFileSystemConfiguration_endpoint :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_endpoint = Lens.lens (\SharedFileSystemConfiguration' {endpoint} -> endpoint) (\s@SharedFileSystemConfiguration' {} a -> s {endpoint = a} :: SharedFileSystemConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The unique identifier for a file system.
sharedFileSystemConfiguration_fileSystemId :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_fileSystemId = Lens.lens (\SharedFileSystemConfiguration' {fileSystemId} -> fileSystemId) (\s@SharedFileSystemConfiguration' {} a -> s {fileSystemId = a} :: SharedFileSystemConfiguration)

-- | The mount location for a shared file system on a Linux virtual
-- workstation.
sharedFileSystemConfiguration_linuxMountPoint :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_linuxMountPoint = Lens.lens (\SharedFileSystemConfiguration' {linuxMountPoint} -> linuxMountPoint) (\s@SharedFileSystemConfiguration' {} a -> s {linuxMountPoint = a} :: SharedFileSystemConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The name of the file share.
sharedFileSystemConfiguration_shareName :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_shareName = Lens.lens (\SharedFileSystemConfiguration' {shareName} -> shareName) (\s@SharedFileSystemConfiguration' {} a -> s {shareName = a} :: SharedFileSystemConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The mount location for a shared file system on a Windows virtual
-- workstation.
sharedFileSystemConfiguration_windowsMountDrive :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_windowsMountDrive = Lens.lens (\SharedFileSystemConfiguration' {windowsMountDrive} -> windowsMountDrive) (\s@SharedFileSystemConfiguration' {} a -> s {windowsMountDrive = a} :: SharedFileSystemConfiguration)

instance Data.FromJSON SharedFileSystemConfiguration where
  parseJSON =
    Data.withObject
      "SharedFileSystemConfiguration"
      ( \x ->
          SharedFileSystemConfiguration'
            Prelude.<$> (x Data..:? "endpoint")
            Prelude.<*> (x Data..:? "fileSystemId")
            Prelude.<*> (x Data..:? "linuxMountPoint")
            Prelude.<*> (x Data..:? "shareName")
            Prelude.<*> (x Data..:? "windowsMountDrive")
      )

instance
  Prelude.Hashable
    SharedFileSystemConfiguration
  where
  hashWithSalt _salt SharedFileSystemConfiguration' {..} =
    _salt `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` linuxMountPoint
      `Prelude.hashWithSalt` shareName
      `Prelude.hashWithSalt` windowsMountDrive

instance Prelude.NFData SharedFileSystemConfiguration where
  rnf SharedFileSystemConfiguration' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf linuxMountPoint
      `Prelude.seq` Prelude.rnf shareName
      `Prelude.seq` Prelude.rnf windowsMountDrive

instance Data.ToJSON SharedFileSystemConfiguration where
  toJSON SharedFileSystemConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("endpoint" Data..=) Prelude.<$> endpoint,
            ("fileSystemId" Data..=) Prelude.<$> fileSystemId,
            ("linuxMountPoint" Data..=)
              Prelude.<$> linuxMountPoint,
            ("shareName" Data..=) Prelude.<$> shareName,
            ("windowsMountDrive" Data..=)
              Prelude.<$> windowsMountDrive
          ]
      )
