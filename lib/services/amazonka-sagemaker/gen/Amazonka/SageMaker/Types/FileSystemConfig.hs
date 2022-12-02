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
-- Module      : Amazonka.SageMaker.Types.FileSystemConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FileSystemConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Elastic File System (EFS) storage configuration for a
-- SageMaker image.
--
-- /See:/ 'newFileSystemConfig' smart constructor.
data FileSystemConfig = FileSystemConfig'
  { -- | The default POSIX group ID (GID). If not specified, defaults to @100@.
    defaultGid :: Prelude.Maybe Prelude.Natural,
    -- | The default POSIX user ID (UID). If not specified, defaults to @1000@.
    defaultUid :: Prelude.Maybe Prelude.Natural,
    -- | The path within the image to mount the user\'s EFS home directory. The
    -- directory should be empty. If not specified, defaults to
    -- /\/home\/sagemaker-user/.
    mountPath :: Prelude.Maybe Prelude.Text
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
-- 'defaultGid', 'fileSystemConfig_defaultGid' - The default POSIX group ID (GID). If not specified, defaults to @100@.
--
-- 'defaultUid', 'fileSystemConfig_defaultUid' - The default POSIX user ID (UID). If not specified, defaults to @1000@.
--
-- 'mountPath', 'fileSystemConfig_mountPath' - The path within the image to mount the user\'s EFS home directory. The
-- directory should be empty. If not specified, defaults to
-- /\/home\/sagemaker-user/.
newFileSystemConfig ::
  FileSystemConfig
newFileSystemConfig =
  FileSystemConfig'
    { defaultGid = Prelude.Nothing,
      defaultUid = Prelude.Nothing,
      mountPath = Prelude.Nothing
    }

-- | The default POSIX group ID (GID). If not specified, defaults to @100@.
fileSystemConfig_defaultGid :: Lens.Lens' FileSystemConfig (Prelude.Maybe Prelude.Natural)
fileSystemConfig_defaultGid = Lens.lens (\FileSystemConfig' {defaultGid} -> defaultGid) (\s@FileSystemConfig' {} a -> s {defaultGid = a} :: FileSystemConfig)

-- | The default POSIX user ID (UID). If not specified, defaults to @1000@.
fileSystemConfig_defaultUid :: Lens.Lens' FileSystemConfig (Prelude.Maybe Prelude.Natural)
fileSystemConfig_defaultUid = Lens.lens (\FileSystemConfig' {defaultUid} -> defaultUid) (\s@FileSystemConfig' {} a -> s {defaultUid = a} :: FileSystemConfig)

-- | The path within the image to mount the user\'s EFS home directory. The
-- directory should be empty. If not specified, defaults to
-- /\/home\/sagemaker-user/.
fileSystemConfig_mountPath :: Lens.Lens' FileSystemConfig (Prelude.Maybe Prelude.Text)
fileSystemConfig_mountPath = Lens.lens (\FileSystemConfig' {mountPath} -> mountPath) (\s@FileSystemConfig' {} a -> s {mountPath = a} :: FileSystemConfig)

instance Data.FromJSON FileSystemConfig where
  parseJSON =
    Data.withObject
      "FileSystemConfig"
      ( \x ->
          FileSystemConfig'
            Prelude.<$> (x Data..:? "DefaultGid")
            Prelude.<*> (x Data..:? "DefaultUid")
            Prelude.<*> (x Data..:? "MountPath")
      )

instance Prelude.Hashable FileSystemConfig where
  hashWithSalt _salt FileSystemConfig' {..} =
    _salt `Prelude.hashWithSalt` defaultGid
      `Prelude.hashWithSalt` defaultUid
      `Prelude.hashWithSalt` mountPath

instance Prelude.NFData FileSystemConfig where
  rnf FileSystemConfig' {..} =
    Prelude.rnf defaultGid
      `Prelude.seq` Prelude.rnf defaultUid
      `Prelude.seq` Prelude.rnf mountPath

instance Data.ToJSON FileSystemConfig where
  toJSON FileSystemConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultGid" Data..=) Prelude.<$> defaultGid,
            ("DefaultUid" Data..=) Prelude.<$> defaultUid,
            ("MountPath" Data..=) Prelude.<$> mountPath
          ]
      )
