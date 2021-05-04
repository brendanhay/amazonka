{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the connection between a Lambda function and an Amazon EFS
-- file system.
--
-- /See:/ 'newFileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { -- | The Amazon Resource Name (ARN) of the Amazon EFS access point that
    -- provides access to the file system.
    arn :: Prelude.Text,
    -- | The path where the function can access the file system, starting with
    -- @\/mnt\/@.
    localMountPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'localMountPath'
  Prelude.Text ->
  FileSystemConfig
newFileSystemConfig pArn_ pLocalMountPath_ =
  FileSystemConfig'
    { arn = pArn_,
      localMountPath = pLocalMountPath_
    }

-- | The Amazon Resource Name (ARN) of the Amazon EFS access point that
-- provides access to the file system.
fileSystemConfig_arn :: Lens.Lens' FileSystemConfig Prelude.Text
fileSystemConfig_arn = Lens.lens (\FileSystemConfig' {arn} -> arn) (\s@FileSystemConfig' {} a -> s {arn = a} :: FileSystemConfig)

-- | The path where the function can access the file system, starting with
-- @\/mnt\/@.
fileSystemConfig_localMountPath :: Lens.Lens' FileSystemConfig Prelude.Text
fileSystemConfig_localMountPath = Lens.lens (\FileSystemConfig' {localMountPath} -> localMountPath) (\s@FileSystemConfig' {} a -> s {localMountPath = a} :: FileSystemConfig)

instance Prelude.FromJSON FileSystemConfig where
  parseJSON =
    Prelude.withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig'
            Prelude.<$> (x Prelude..: "Arn")
            Prelude.<*> (x Prelude..: "LocalMountPath")
      )

instance Prelude.Hashable FileSystemConfig

instance Prelude.NFData FileSystemConfig

instance Prelude.ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Prelude..= arn),
            Prelude.Just
              ("LocalMountPath" Prelude..= localMountPath)
          ]
      )
