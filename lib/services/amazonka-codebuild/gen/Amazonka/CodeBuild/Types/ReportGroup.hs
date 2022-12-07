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
-- Module      : Amazonka.CodeBuild.Types.ReportGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportGroup where

import Amazonka.CodeBuild.Types.ReportExportConfig
import Amazonka.CodeBuild.Types.ReportGroupStatusType
import Amazonka.CodeBuild.Types.ReportType
import Amazonka.CodeBuild.Types.Tag
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A series of reports. Each report contains information about the results
-- from running a series of test cases. You specify the test cases for a
-- report group in the buildspec for a build project using one or more
-- paths to the test case files.
--
-- /See:/ 'newReportGroup' smart constructor.
data ReportGroup = ReportGroup'
  { -- | A list of tag key and value pairs associated with this report group.
    --
    -- These tags are available for use by Amazon Web Services services that
    -- support CodeBuild report group tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the @ReportGroup@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of the @ReportGroup@. This can be one of the following values:
    --
    -- [CODE_COVERAGE]
    --     The report group contains code coverage reports.
    --
    -- [TEST]
    --     The report group contains test reports.
    type' :: Prelude.Maybe ReportType,
    -- | The date and time this @ReportGroup@ was created.
    created :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the @ReportGroup@.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the report group. This property is read-only.
    --
    -- This can be one of the following values:
    --
    -- [ACTIVE]
    --     The report group is active.
    --
    -- [DELETING]
    --     The report group is in the process of being deleted.
    status :: Prelude.Maybe ReportGroupStatusType,
    -- | Information about the destination where the raw data of this
    -- @ReportGroup@ is exported.
    exportConfig :: Prelude.Maybe ReportExportConfig,
    -- | The date and time this @ReportGroup@ was last modified.
    lastModified :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'reportGroup_tags' - A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild report group tags.
--
-- 'name', 'reportGroup_name' - The name of the @ReportGroup@.
--
-- 'type'', 'reportGroup_type' - The type of the @ReportGroup@. This can be one of the following values:
--
-- [CODE_COVERAGE]
--     The report group contains code coverage reports.
--
-- [TEST]
--     The report group contains test reports.
--
-- 'created', 'reportGroup_created' - The date and time this @ReportGroup@ was created.
--
-- 'arn', 'reportGroup_arn' - The ARN of the @ReportGroup@.
--
-- 'status', 'reportGroup_status' - The status of the report group. This property is read-only.
--
-- This can be one of the following values:
--
-- [ACTIVE]
--     The report group is active.
--
-- [DELETING]
--     The report group is in the process of being deleted.
--
-- 'exportConfig', 'reportGroup_exportConfig' - Information about the destination where the raw data of this
-- @ReportGroup@ is exported.
--
-- 'lastModified', 'reportGroup_lastModified' - The date and time this @ReportGroup@ was last modified.
newReportGroup ::
  ReportGroup
newReportGroup =
  ReportGroup'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing,
      created = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      exportConfig = Prelude.Nothing,
      lastModified = Prelude.Nothing
    }

-- | A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by Amazon Web Services services that
-- support CodeBuild report group tags.
reportGroup_tags :: Lens.Lens' ReportGroup (Prelude.Maybe [Tag])
reportGroup_tags = Lens.lens (\ReportGroup' {tags} -> tags) (\s@ReportGroup' {} a -> s {tags = a} :: ReportGroup) Prelude.. Lens.mapping Lens.coerced

-- | The name of the @ReportGroup@.
reportGroup_name :: Lens.Lens' ReportGroup (Prelude.Maybe Prelude.Text)
reportGroup_name = Lens.lens (\ReportGroup' {name} -> name) (\s@ReportGroup' {} a -> s {name = a} :: ReportGroup)

-- | The type of the @ReportGroup@. This can be one of the following values:
--
-- [CODE_COVERAGE]
--     The report group contains code coverage reports.
--
-- [TEST]
--     The report group contains test reports.
reportGroup_type :: Lens.Lens' ReportGroup (Prelude.Maybe ReportType)
reportGroup_type = Lens.lens (\ReportGroup' {type'} -> type') (\s@ReportGroup' {} a -> s {type' = a} :: ReportGroup)

-- | The date and time this @ReportGroup@ was created.
reportGroup_created :: Lens.Lens' ReportGroup (Prelude.Maybe Prelude.UTCTime)
reportGroup_created = Lens.lens (\ReportGroup' {created} -> created) (\s@ReportGroup' {} a -> s {created = a} :: ReportGroup) Prelude.. Lens.mapping Data._Time

-- | The ARN of the @ReportGroup@.
reportGroup_arn :: Lens.Lens' ReportGroup (Prelude.Maybe Prelude.Text)
reportGroup_arn = Lens.lens (\ReportGroup' {arn} -> arn) (\s@ReportGroup' {} a -> s {arn = a} :: ReportGroup)

-- | The status of the report group. This property is read-only.
--
-- This can be one of the following values:
--
-- [ACTIVE]
--     The report group is active.
--
-- [DELETING]
--     The report group is in the process of being deleted.
reportGroup_status :: Lens.Lens' ReportGroup (Prelude.Maybe ReportGroupStatusType)
reportGroup_status = Lens.lens (\ReportGroup' {status} -> status) (\s@ReportGroup' {} a -> s {status = a} :: ReportGroup)

-- | Information about the destination where the raw data of this
-- @ReportGroup@ is exported.
reportGroup_exportConfig :: Lens.Lens' ReportGroup (Prelude.Maybe ReportExportConfig)
reportGroup_exportConfig = Lens.lens (\ReportGroup' {exportConfig} -> exportConfig) (\s@ReportGroup' {} a -> s {exportConfig = a} :: ReportGroup)

-- | The date and time this @ReportGroup@ was last modified.
reportGroup_lastModified :: Lens.Lens' ReportGroup (Prelude.Maybe Prelude.UTCTime)
reportGroup_lastModified = Lens.lens (\ReportGroup' {lastModified} -> lastModified) (\s@ReportGroup' {} a -> s {lastModified = a} :: ReportGroup) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ReportGroup where
  parseJSON =
    Data.withObject
      "ReportGroup"
      ( \x ->
          ReportGroup'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "created")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "exportConfig")
            Prelude.<*> (x Data..:? "lastModified")
      )

instance Prelude.Hashable ReportGroup where
  hashWithSalt _salt ReportGroup' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` exportConfig
      `Prelude.hashWithSalt` lastModified

instance Prelude.NFData ReportGroup where
  rnf ReportGroup' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf exportConfig
      `Prelude.seq` Prelude.rnf lastModified
