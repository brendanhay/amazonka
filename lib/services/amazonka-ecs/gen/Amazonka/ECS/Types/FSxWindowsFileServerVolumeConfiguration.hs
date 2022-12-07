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
-- Module      : Amazonka.ECS.Types.FSxWindowsFileServerVolumeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.FSxWindowsFileServerVolumeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.FSxWindowsFileServerAuthorizationConfig
import qualified Amazonka.Prelude as Prelude

-- | This parameter is specified when you\'re using
-- <https://docs.aws.amazon.com/fsx/latest/WindowsGuide/what-is.html Amazon FSx for Windows File Server>
-- file system for task storage.
--
-- For more information and the input format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server volumes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newFSxWindowsFileServerVolumeConfiguration' smart constructor.
data FSxWindowsFileServerVolumeConfiguration = FSxWindowsFileServerVolumeConfiguration'
  { -- | The Amazon FSx for Windows File Server file system ID to use.
    fileSystemId :: Prelude.Text,
    -- | The directory within the Amazon FSx for Windows File Server file system
    -- to mount as the root directory inside the host.
    rootDirectory :: Prelude.Text,
    -- | The authorization configuration details for the Amazon FSx for Windows
    -- File Server file system.
    authorizationConfig :: FSxWindowsFileServerAuthorizationConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FSxWindowsFileServerVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'fSxWindowsFileServerVolumeConfiguration_fileSystemId' - The Amazon FSx for Windows File Server file system ID to use.
--
-- 'rootDirectory', 'fSxWindowsFileServerVolumeConfiguration_rootDirectory' - The directory within the Amazon FSx for Windows File Server file system
-- to mount as the root directory inside the host.
--
-- 'authorizationConfig', 'fSxWindowsFileServerVolumeConfiguration_authorizationConfig' - The authorization configuration details for the Amazon FSx for Windows
-- File Server file system.
newFSxWindowsFileServerVolumeConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'rootDirectory'
  Prelude.Text ->
  -- | 'authorizationConfig'
  FSxWindowsFileServerAuthorizationConfig ->
  FSxWindowsFileServerVolumeConfiguration
newFSxWindowsFileServerVolumeConfiguration
  pFileSystemId_
  pRootDirectory_
  pAuthorizationConfig_ =
    FSxWindowsFileServerVolumeConfiguration'
      { fileSystemId =
          pFileSystemId_,
        rootDirectory = pRootDirectory_,
        authorizationConfig =
          pAuthorizationConfig_
      }

-- | The Amazon FSx for Windows File Server file system ID to use.
fSxWindowsFileServerVolumeConfiguration_fileSystemId :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration Prelude.Text
fSxWindowsFileServerVolumeConfiguration_fileSystemId = Lens.lens (\FSxWindowsFileServerVolumeConfiguration' {fileSystemId} -> fileSystemId) (\s@FSxWindowsFileServerVolumeConfiguration' {} a -> s {fileSystemId = a} :: FSxWindowsFileServerVolumeConfiguration)

-- | The directory within the Amazon FSx for Windows File Server file system
-- to mount as the root directory inside the host.
fSxWindowsFileServerVolumeConfiguration_rootDirectory :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration Prelude.Text
fSxWindowsFileServerVolumeConfiguration_rootDirectory = Lens.lens (\FSxWindowsFileServerVolumeConfiguration' {rootDirectory} -> rootDirectory) (\s@FSxWindowsFileServerVolumeConfiguration' {} a -> s {rootDirectory = a} :: FSxWindowsFileServerVolumeConfiguration)

-- | The authorization configuration details for the Amazon FSx for Windows
-- File Server file system.
fSxWindowsFileServerVolumeConfiguration_authorizationConfig :: Lens.Lens' FSxWindowsFileServerVolumeConfiguration FSxWindowsFileServerAuthorizationConfig
fSxWindowsFileServerVolumeConfiguration_authorizationConfig = Lens.lens (\FSxWindowsFileServerVolumeConfiguration' {authorizationConfig} -> authorizationConfig) (\s@FSxWindowsFileServerVolumeConfiguration' {} a -> s {authorizationConfig = a} :: FSxWindowsFileServerVolumeConfiguration)

instance
  Data.FromJSON
    FSxWindowsFileServerVolumeConfiguration
  where
  parseJSON =
    Data.withObject
      "FSxWindowsFileServerVolumeConfiguration"
      ( \x ->
          FSxWindowsFileServerVolumeConfiguration'
            Prelude.<$> (x Data..: "fileSystemId")
            Prelude.<*> (x Data..: "rootDirectory")
            Prelude.<*> (x Data..: "authorizationConfig")
      )

instance
  Prelude.Hashable
    FSxWindowsFileServerVolumeConfiguration
  where
  hashWithSalt
    _salt
    FSxWindowsFileServerVolumeConfiguration' {..} =
      _salt `Prelude.hashWithSalt` fileSystemId
        `Prelude.hashWithSalt` rootDirectory
        `Prelude.hashWithSalt` authorizationConfig

instance
  Prelude.NFData
    FSxWindowsFileServerVolumeConfiguration
  where
  rnf FSxWindowsFileServerVolumeConfiguration' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf rootDirectory
      `Prelude.seq` Prelude.rnf authorizationConfig

instance
  Data.ToJSON
    FSxWindowsFileServerVolumeConfiguration
  where
  toJSON FSxWindowsFileServerVolumeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("fileSystemId" Data..= fileSystemId),
            Prelude.Just ("rootDirectory" Data..= rootDirectory),
            Prelude.Just
              ("authorizationConfig" Data..= authorizationConfig)
          ]
      )
