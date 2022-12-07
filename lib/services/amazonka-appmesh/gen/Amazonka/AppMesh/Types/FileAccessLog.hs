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
-- Module      : Amazonka.AppMesh.Types.FileAccessLog
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.FileAccessLog where

import Amazonka.AppMesh.Types.LoggingFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents an access log file.
--
-- /See:/ 'newFileAccessLog' smart constructor.
data FileAccessLog = FileAccessLog'
  { -- | The specified format for the logs. The format is either @json_format@ or
    -- @text_format@.
    format :: Prelude.Maybe LoggingFormat,
    -- | The file path to write access logs to. You can use @\/dev\/stdout@ to
    -- send access logs to standard out and configure your Envoy container to
    -- use a log driver, such as @awslogs@, to export the access logs to a log
    -- storage service such as Amazon CloudWatch Logs. You can also specify a
    -- path in the Envoy container\'s file system to write the files to disk.
    --
    -- >  <note> <p>The Envoy process must have write permissions to the path that you specify here. Otherwise, Envoy fails to bootstrap properly.</p> </note>
    path :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileAccessLog' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'fileAccessLog_format' - The specified format for the logs. The format is either @json_format@ or
-- @text_format@.
--
-- 'path', 'fileAccessLog_path' - The file path to write access logs to. You can use @\/dev\/stdout@ to
-- send access logs to standard out and configure your Envoy container to
-- use a log driver, such as @awslogs@, to export the access logs to a log
-- storage service such as Amazon CloudWatch Logs. You can also specify a
-- path in the Envoy container\'s file system to write the files to disk.
--
-- >  <note> <p>The Envoy process must have write permissions to the path that you specify here. Otherwise, Envoy fails to bootstrap properly.</p> </note>
newFileAccessLog ::
  -- | 'path'
  Prelude.Text ->
  FileAccessLog
newFileAccessLog pPath_ =
  FileAccessLog'
    { format = Prelude.Nothing,
      path = pPath_
    }

-- | The specified format for the logs. The format is either @json_format@ or
-- @text_format@.
fileAccessLog_format :: Lens.Lens' FileAccessLog (Prelude.Maybe LoggingFormat)
fileAccessLog_format = Lens.lens (\FileAccessLog' {format} -> format) (\s@FileAccessLog' {} a -> s {format = a} :: FileAccessLog)

-- | The file path to write access logs to. You can use @\/dev\/stdout@ to
-- send access logs to standard out and configure your Envoy container to
-- use a log driver, such as @awslogs@, to export the access logs to a log
-- storage service such as Amazon CloudWatch Logs. You can also specify a
-- path in the Envoy container\'s file system to write the files to disk.
--
-- >  <note> <p>The Envoy process must have write permissions to the path that you specify here. Otherwise, Envoy fails to bootstrap properly.</p> </note>
fileAccessLog_path :: Lens.Lens' FileAccessLog Prelude.Text
fileAccessLog_path = Lens.lens (\FileAccessLog' {path} -> path) (\s@FileAccessLog' {} a -> s {path = a} :: FileAccessLog)

instance Data.FromJSON FileAccessLog where
  parseJSON =
    Data.withObject
      "FileAccessLog"
      ( \x ->
          FileAccessLog'
            Prelude.<$> (x Data..:? "format") Prelude.<*> (x Data..: "path")
      )

instance Prelude.Hashable FileAccessLog where
  hashWithSalt _salt FileAccessLog' {..} =
    _salt `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` path

instance Prelude.NFData FileAccessLog where
  rnf FileAccessLog' {..} =
    Prelude.rnf format `Prelude.seq` Prelude.rnf path

instance Data.ToJSON FileAccessLog where
  toJSON FileAccessLog' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("format" Data..=) Prelude.<$> format,
            Prelude.Just ("path" Data..= path)
          ]
      )
