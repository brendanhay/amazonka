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
-- Module      : Network.AWS.Lambda.Types.FileSystemConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.FileSystemConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the connection between a Lambda function and an Amazon EFS
-- file system.
--
-- /See:/ 'newFileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { -- | The Amazon Resource Name (ARN) of the Amazon EFS access point that
    -- provides access to the file system.
    arn :: Core.Text,
    -- | The path where the function can access the file system, starting with
    -- @\/mnt\/@.
    localMountPath :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FileSystemConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'fileSystemConfig_arn' - The Amazon Resource Name (ARN) of the Amazon EFS access point that
-- provides access to the file system.
--
-- 'localMountPath', 'fileSystemConfig_localMountPath' - The path where the function can access the file system, starting with
-- @\/mnt\/@.
newFileSystemConfig ::
  -- | 'arn'
  Core.Text ->
  -- | 'localMountPath'
  Core.Text ->
  FileSystemConfig
newFileSystemConfig pArn_ pLocalMountPath_ =
  FileSystemConfig'
    { arn = pArn_,
      localMountPath = pLocalMountPath_
    }

-- | The Amazon Resource Name (ARN) of the Amazon EFS access point that
-- provides access to the file system.
fileSystemConfig_arn :: Lens.Lens' FileSystemConfig Core.Text
fileSystemConfig_arn = Lens.lens (\FileSystemConfig' {arn} -> arn) (\s@FileSystemConfig' {} a -> s {arn = a} :: FileSystemConfig)

-- | The path where the function can access the file system, starting with
-- @\/mnt\/@.
fileSystemConfig_localMountPath :: Lens.Lens' FileSystemConfig Core.Text
fileSystemConfig_localMountPath = Lens.lens (\FileSystemConfig' {localMountPath} -> localMountPath) (\s@FileSystemConfig' {} a -> s {localMountPath = a} :: FileSystemConfig)

instance Core.FromJSON FileSystemConfig where
  parseJSON =
    Core.withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig'
            Core.<$> (x Core..: "Arn")
            Core.<*> (x Core..: "LocalMountPath")
      )

instance Core.Hashable FileSystemConfig

instance Core.NFData FileSystemConfig

instance Core.ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Arn" Core..= arn),
            Core.Just ("LocalMountPath" Core..= localMountPath)
          ]
      )
