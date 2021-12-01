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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Nimble.Types.SharedFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for a shared file storage system that is associated
-- with a studio resource.
--
-- /See:/ 'newSharedFileSystemConfiguration' smart constructor.
data SharedFileSystemConfiguration = SharedFileSystemConfiguration'
  { -- | The unique identifier for a file system.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The mount location for a shared file system on a Windows virtual
    -- workstation.
    windowsMountDrive :: Prelude.Maybe Prelude.Text,
    -- | The mount location for a shared file system on a Linux virtual
    -- workstation.
    linuxMountPoint :: Prelude.Maybe Prelude.Text,
    -- | The name of the file share.
    shareName :: Prelude.Maybe Prelude.Text,
    -- | The endpoint of the shared file system that is accessed by the studio
    -- component resource.
    endpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SharedFileSystemConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'sharedFileSystemConfiguration_fileSystemId' - The unique identifier for a file system.
--
-- 'windowsMountDrive', 'sharedFileSystemConfiguration_windowsMountDrive' - The mount location for a shared file system on a Windows virtual
-- workstation.
--
-- 'linuxMountPoint', 'sharedFileSystemConfiguration_linuxMountPoint' - The mount location for a shared file system on a Linux virtual
-- workstation.
--
-- 'shareName', 'sharedFileSystemConfiguration_shareName' - The name of the file share.
--
-- 'endpoint', 'sharedFileSystemConfiguration_endpoint' - The endpoint of the shared file system that is accessed by the studio
-- component resource.
newSharedFileSystemConfiguration ::
  SharedFileSystemConfiguration
newSharedFileSystemConfiguration =
  SharedFileSystemConfiguration'
    { fileSystemId =
        Prelude.Nothing,
      windowsMountDrive = Prelude.Nothing,
      linuxMountPoint = Prelude.Nothing,
      shareName = Prelude.Nothing,
      endpoint = Prelude.Nothing
    }

-- | The unique identifier for a file system.
sharedFileSystemConfiguration_fileSystemId :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_fileSystemId = Lens.lens (\SharedFileSystemConfiguration' {fileSystemId} -> fileSystemId) (\s@SharedFileSystemConfiguration' {} a -> s {fileSystemId = a} :: SharedFileSystemConfiguration)

-- | The mount location for a shared file system on a Windows virtual
-- workstation.
sharedFileSystemConfiguration_windowsMountDrive :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_windowsMountDrive = Lens.lens (\SharedFileSystemConfiguration' {windowsMountDrive} -> windowsMountDrive) (\s@SharedFileSystemConfiguration' {} a -> s {windowsMountDrive = a} :: SharedFileSystemConfiguration)

-- | The mount location for a shared file system on a Linux virtual
-- workstation.
sharedFileSystemConfiguration_linuxMountPoint :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_linuxMountPoint = Lens.lens (\SharedFileSystemConfiguration' {linuxMountPoint} -> linuxMountPoint) (\s@SharedFileSystemConfiguration' {} a -> s {linuxMountPoint = a} :: SharedFileSystemConfiguration)

-- | The name of the file share.
sharedFileSystemConfiguration_shareName :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_shareName = Lens.lens (\SharedFileSystemConfiguration' {shareName} -> shareName) (\s@SharedFileSystemConfiguration' {} a -> s {shareName = a} :: SharedFileSystemConfiguration)

-- | The endpoint of the shared file system that is accessed by the studio
-- component resource.
sharedFileSystemConfiguration_endpoint :: Lens.Lens' SharedFileSystemConfiguration (Prelude.Maybe Prelude.Text)
sharedFileSystemConfiguration_endpoint = Lens.lens (\SharedFileSystemConfiguration' {endpoint} -> endpoint) (\s@SharedFileSystemConfiguration' {} a -> s {endpoint = a} :: SharedFileSystemConfiguration)

instance Core.FromJSON SharedFileSystemConfiguration where
  parseJSON =
    Core.withObject
      "SharedFileSystemConfiguration"
      ( \x ->
          SharedFileSystemConfiguration'
            Prelude.<$> (x Core..:? "fileSystemId")
            Prelude.<*> (x Core..:? "windowsMountDrive")
            Prelude.<*> (x Core..:? "linuxMountPoint")
            Prelude.<*> (x Core..:? "shareName")
            Prelude.<*> (x Core..:? "endpoint")
      )

instance
  Prelude.Hashable
    SharedFileSystemConfiguration
  where
  hashWithSalt salt' SharedFileSystemConfiguration' {..} =
    salt' `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` shareName
      `Prelude.hashWithSalt` linuxMountPoint
      `Prelude.hashWithSalt` windowsMountDrive
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData SharedFileSystemConfiguration where
  rnf SharedFileSystemConfiguration' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf shareName
      `Prelude.seq` Prelude.rnf linuxMountPoint
      `Prelude.seq` Prelude.rnf windowsMountDrive

instance Core.ToJSON SharedFileSystemConfiguration where
  toJSON SharedFileSystemConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("fileSystemId" Core..=) Prelude.<$> fileSystemId,
            ("windowsMountDrive" Core..=)
              Prelude.<$> windowsMountDrive,
            ("linuxMountPoint" Core..=)
              Prelude.<$> linuxMountPoint,
            ("shareName" Core..=) Prelude.<$> shareName,
            ("endpoint" Core..=) Prelude.<$> endpoint
          ]
      )
