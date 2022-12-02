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
-- Module      : Amazonka.Lambda.Types.FileSystemConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FileSystemConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the connection between a Lambda function and an
-- <https://docs.aws.amazon.com/lambda/latest/dg/configuration-filesystem.html Amazon EFS file system>.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON FileSystemConfig where
  parseJSON =
    Data.withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig'
            Prelude.<$> (x Data..: "Arn")
            Prelude.<*> (x Data..: "LocalMountPath")
      )

instance Prelude.Hashable FileSystemConfig where
  hashWithSalt _salt FileSystemConfig' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` localMountPath

instance Prelude.NFData FileSystemConfig where
  rnf FileSystemConfig' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf localMountPath

instance Data.ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Data..= arn),
            Prelude.Just
              ("LocalMountPath" Data..= localMountPath)
          ]
      )
