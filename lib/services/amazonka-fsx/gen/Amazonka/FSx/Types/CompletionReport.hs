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
-- Module      : Amazonka.FSx.Types.CompletionReport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.CompletionReport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.ReportFormat
import Amazonka.FSx.Types.ReportScope
import qualified Amazonka.Prelude as Prelude

-- | Provides a report detailing the data repository task results of the
-- files processed that match the criteria specified in the report @Scope@
-- parameter. FSx delivers the report to the file system\'s linked data
-- repository in Amazon S3, using the path specified in the report @Path@
-- parameter. You can specify whether or not a report gets generated for a
-- task using the @Enabled@ parameter.
--
-- /See:/ 'newCompletionReport' smart constructor.
data CompletionReport = CompletionReport'
  { -- | Required if @Enabled@ is set to @true@. Specifies the format of the
    -- @CompletionReport@. @REPORT_CSV_20191124@ is the only format currently
    -- supported. When @Format@ is set to @REPORT_CSV_20191124@, the
    -- @CompletionReport@ is provided in CSV format, and is delivered to
    -- @{path}\/task-{id}\/failures.csv@.
    format :: Prelude.Maybe ReportFormat,
    -- | Required if @Enabled@ is set to @true@. Specifies the location of the
    -- report on the file system\'s linked S3 data repository. An absolute path
    -- that defines where the completion report will be stored in the
    -- destination location. The @Path@ you provide must be located within the
    -- file system’s ExportPath. An example @Path@ value is
    -- \"s3:\/\/myBucket\/myExportPath\/optionalPrefix\". The report provides
    -- the following information for each file in the report: FilePath,
    -- FileStatus, and ErrorCode.
    path :: Prelude.Maybe Prelude.Text,
    -- | Required if @Enabled@ is set to @true@. Specifies the scope of the
    -- @CompletionReport@; @FAILED_FILES_ONLY@ is the only scope currently
    -- supported. When @Scope@ is set to @FAILED_FILES_ONLY@, the
    -- @CompletionReport@ only contains information about files that the data
    -- repository task failed to process.
    scope :: Prelude.Maybe ReportScope,
    -- | Set @Enabled@ to @True@ to generate a @CompletionReport@ when the task
    -- completes. If set to @true@, then you need to provide a report @Scope@,
    -- @Path@, and @Format@. Set @Enabled@ to @False@ if you do not want a
    -- @CompletionReport@ generated when the task completes.
    enabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompletionReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'format', 'completionReport_format' - Required if @Enabled@ is set to @true@. Specifies the format of the
-- @CompletionReport@. @REPORT_CSV_20191124@ is the only format currently
-- supported. When @Format@ is set to @REPORT_CSV_20191124@, the
-- @CompletionReport@ is provided in CSV format, and is delivered to
-- @{path}\/task-{id}\/failures.csv@.
--
-- 'path', 'completionReport_path' - Required if @Enabled@ is set to @true@. Specifies the location of the
-- report on the file system\'s linked S3 data repository. An absolute path
-- that defines where the completion report will be stored in the
-- destination location. The @Path@ you provide must be located within the
-- file system’s ExportPath. An example @Path@ value is
-- \"s3:\/\/myBucket\/myExportPath\/optionalPrefix\". The report provides
-- the following information for each file in the report: FilePath,
-- FileStatus, and ErrorCode.
--
-- 'scope', 'completionReport_scope' - Required if @Enabled@ is set to @true@. Specifies the scope of the
-- @CompletionReport@; @FAILED_FILES_ONLY@ is the only scope currently
-- supported. When @Scope@ is set to @FAILED_FILES_ONLY@, the
-- @CompletionReport@ only contains information about files that the data
-- repository task failed to process.
--
-- 'enabled', 'completionReport_enabled' - Set @Enabled@ to @True@ to generate a @CompletionReport@ when the task
-- completes. If set to @true@, then you need to provide a report @Scope@,
-- @Path@, and @Format@. Set @Enabled@ to @False@ if you do not want a
-- @CompletionReport@ generated when the task completes.
newCompletionReport ::
  -- | 'enabled'
  Prelude.Bool ->
  CompletionReport
newCompletionReport pEnabled_ =
  CompletionReport'
    { format = Prelude.Nothing,
      path = Prelude.Nothing,
      scope = Prelude.Nothing,
      enabled = pEnabled_
    }

-- | Required if @Enabled@ is set to @true@. Specifies the format of the
-- @CompletionReport@. @REPORT_CSV_20191124@ is the only format currently
-- supported. When @Format@ is set to @REPORT_CSV_20191124@, the
-- @CompletionReport@ is provided in CSV format, and is delivered to
-- @{path}\/task-{id}\/failures.csv@.
completionReport_format :: Lens.Lens' CompletionReport (Prelude.Maybe ReportFormat)
completionReport_format = Lens.lens (\CompletionReport' {format} -> format) (\s@CompletionReport' {} a -> s {format = a} :: CompletionReport)

-- | Required if @Enabled@ is set to @true@. Specifies the location of the
-- report on the file system\'s linked S3 data repository. An absolute path
-- that defines where the completion report will be stored in the
-- destination location. The @Path@ you provide must be located within the
-- file system’s ExportPath. An example @Path@ value is
-- \"s3:\/\/myBucket\/myExportPath\/optionalPrefix\". The report provides
-- the following information for each file in the report: FilePath,
-- FileStatus, and ErrorCode.
completionReport_path :: Lens.Lens' CompletionReport (Prelude.Maybe Prelude.Text)
completionReport_path = Lens.lens (\CompletionReport' {path} -> path) (\s@CompletionReport' {} a -> s {path = a} :: CompletionReport)

-- | Required if @Enabled@ is set to @true@. Specifies the scope of the
-- @CompletionReport@; @FAILED_FILES_ONLY@ is the only scope currently
-- supported. When @Scope@ is set to @FAILED_FILES_ONLY@, the
-- @CompletionReport@ only contains information about files that the data
-- repository task failed to process.
completionReport_scope :: Lens.Lens' CompletionReport (Prelude.Maybe ReportScope)
completionReport_scope = Lens.lens (\CompletionReport' {scope} -> scope) (\s@CompletionReport' {} a -> s {scope = a} :: CompletionReport)

-- | Set @Enabled@ to @True@ to generate a @CompletionReport@ when the task
-- completes. If set to @true@, then you need to provide a report @Scope@,
-- @Path@, and @Format@. Set @Enabled@ to @False@ if you do not want a
-- @CompletionReport@ generated when the task completes.
completionReport_enabled :: Lens.Lens' CompletionReport Prelude.Bool
completionReport_enabled = Lens.lens (\CompletionReport' {enabled} -> enabled) (\s@CompletionReport' {} a -> s {enabled = a} :: CompletionReport)

instance Data.FromJSON CompletionReport where
  parseJSON =
    Data.withObject
      "CompletionReport"
      ( \x ->
          CompletionReport'
            Prelude.<$> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "Path")
            Prelude.<*> (x Data..:? "Scope")
            Prelude.<*> (x Data..: "Enabled")
      )

instance Prelude.Hashable CompletionReport where
  hashWithSalt _salt CompletionReport' {..} =
    _salt
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` enabled

instance Prelude.NFData CompletionReport where
  rnf CompletionReport' {..} =
    Prelude.rnf format
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf enabled

instance Data.ToJSON CompletionReport where
  toJSON CompletionReport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Format" Data..=) Prelude.<$> format,
            ("Path" Data..=) Prelude.<$> path,
            ("Scope" Data..=) Prelude.<$> scope,
            Prelude.Just ("Enabled" Data..= enabled)
          ]
      )
