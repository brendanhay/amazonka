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
-- Module      : Network.AWS.CodeBuild.Types.ReportGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroup where

import Network.AWS.CodeBuild.Types.ReportExportConfig
import Network.AWS.CodeBuild.Types.ReportGroupStatusType
import Network.AWS.CodeBuild.Types.ReportType
import Network.AWS.CodeBuild.Types.Tag
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A series of reports. Each report contains information about the results
-- from running a series of test cases. You specify the test cases for a
-- report group in the buildspec for a build project using one or more
-- paths to the test case files.
--
-- /See:/ 'newReportGroup' smart constructor.
data ReportGroup = ReportGroup'
  { -- | The status of the report group. This property is read-only.
    --
    -- This can be one of the following values:
    --
    -- [ACTIVE]
    --     The report group is active.
    --
    -- [DELETING]
    --     The report group is in the process of being deleted.
    status :: Core.Maybe ReportGroupStatusType,
    -- | Information about the destination where the raw data of this
    -- @ReportGroup@ is exported.
    exportConfig :: Core.Maybe ReportExportConfig,
    -- | The ARN of the @ReportGroup@.
    arn :: Core.Maybe Core.Text,
    -- | The name of the @ReportGroup@.
    name :: Core.Maybe Core.Text,
    -- | A list of tag key and value pairs associated with this report group.
    --
    -- These tags are available for use by AWS services that support AWS
    -- CodeBuild report group tags.
    tags :: Core.Maybe [Tag],
    -- | The date and time this @ReportGroup@ was last modified.
    lastModified :: Core.Maybe Core.POSIX,
    -- | The date and time this @ReportGroup@ was created.
    created :: Core.Maybe Core.POSIX,
    -- | The type of the @ReportGroup@. This can be one of the following values:
    --
    -- [CODE_COVERAGE]
    --     The report group contains code coverage reports.
    --
    -- [TEST]
    --     The report group contains test reports.
    type' :: Core.Maybe ReportType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReportGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'arn', 'reportGroup_arn' - The ARN of the @ReportGroup@.
--
-- 'name', 'reportGroup_name' - The name of the @ReportGroup@.
--
-- 'tags', 'reportGroup_tags' - A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS
-- CodeBuild report group tags.
--
-- 'lastModified', 'reportGroup_lastModified' - The date and time this @ReportGroup@ was last modified.
--
-- 'created', 'reportGroup_created' - The date and time this @ReportGroup@ was created.
--
-- 'type'', 'reportGroup_type' - The type of the @ReportGroup@. This can be one of the following values:
--
-- [CODE_COVERAGE]
--     The report group contains code coverage reports.
--
-- [TEST]
--     The report group contains test reports.
newReportGroup ::
  ReportGroup
newReportGroup =
  ReportGroup'
    { status = Core.Nothing,
      exportConfig = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      lastModified = Core.Nothing,
      created = Core.Nothing,
      type' = Core.Nothing
    }

-- | The status of the report group. This property is read-only.
--
-- This can be one of the following values:
--
-- [ACTIVE]
--     The report group is active.
--
-- [DELETING]
--     The report group is in the process of being deleted.
reportGroup_status :: Lens.Lens' ReportGroup (Core.Maybe ReportGroupStatusType)
reportGroup_status = Lens.lens (\ReportGroup' {status} -> status) (\s@ReportGroup' {} a -> s {status = a} :: ReportGroup)

-- | Information about the destination where the raw data of this
-- @ReportGroup@ is exported.
reportGroup_exportConfig :: Lens.Lens' ReportGroup (Core.Maybe ReportExportConfig)
reportGroup_exportConfig = Lens.lens (\ReportGroup' {exportConfig} -> exportConfig) (\s@ReportGroup' {} a -> s {exportConfig = a} :: ReportGroup)

-- | The ARN of the @ReportGroup@.
reportGroup_arn :: Lens.Lens' ReportGroup (Core.Maybe Core.Text)
reportGroup_arn = Lens.lens (\ReportGroup' {arn} -> arn) (\s@ReportGroup' {} a -> s {arn = a} :: ReportGroup)

-- | The name of the @ReportGroup@.
reportGroup_name :: Lens.Lens' ReportGroup (Core.Maybe Core.Text)
reportGroup_name = Lens.lens (\ReportGroup' {name} -> name) (\s@ReportGroup' {} a -> s {name = a} :: ReportGroup)

-- | A list of tag key and value pairs associated with this report group.
--
-- These tags are available for use by AWS services that support AWS
-- CodeBuild report group tags.
reportGroup_tags :: Lens.Lens' ReportGroup (Core.Maybe [Tag])
reportGroup_tags = Lens.lens (\ReportGroup' {tags} -> tags) (\s@ReportGroup' {} a -> s {tags = a} :: ReportGroup) Core.. Lens.mapping Lens._Coerce

-- | The date and time this @ReportGroup@ was last modified.
reportGroup_lastModified :: Lens.Lens' ReportGroup (Core.Maybe Core.UTCTime)
reportGroup_lastModified = Lens.lens (\ReportGroup' {lastModified} -> lastModified) (\s@ReportGroup' {} a -> s {lastModified = a} :: ReportGroup) Core.. Lens.mapping Core._Time

-- | The date and time this @ReportGroup@ was created.
reportGroup_created :: Lens.Lens' ReportGroup (Core.Maybe Core.UTCTime)
reportGroup_created = Lens.lens (\ReportGroup' {created} -> created) (\s@ReportGroup' {} a -> s {created = a} :: ReportGroup) Core.. Lens.mapping Core._Time

-- | The type of the @ReportGroup@. This can be one of the following values:
--
-- [CODE_COVERAGE]
--     The report group contains code coverage reports.
--
-- [TEST]
--     The report group contains test reports.
reportGroup_type :: Lens.Lens' ReportGroup (Core.Maybe ReportType)
reportGroup_type = Lens.lens (\ReportGroup' {type'} -> type') (\s@ReportGroup' {} a -> s {type' = a} :: ReportGroup)

instance Core.FromJSON ReportGroup where
  parseJSON =
    Core.withObject
      "ReportGroup"
      ( \x ->
          ReportGroup'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "exportConfig")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "lastModified")
            Core.<*> (x Core..:? "created")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable ReportGroup

instance Core.NFData ReportGroup
