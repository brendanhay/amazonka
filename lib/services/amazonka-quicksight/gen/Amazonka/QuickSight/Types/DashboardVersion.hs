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
-- Module      : Amazonka.QuickSight.Types.DashboardVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardError
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.Sheet

-- | Dashboard version.
--
-- /See:/ 'newDashboardVersion' smart constructor.
data DashboardVersion = DashboardVersion'
  { -- | The HTTP status of the request.
    status :: Prelude.Maybe ResourceStatus,
    -- | The ARN of the theme associated with a version of the dashboard.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Numbers (ARNs) for the datasets that are associated
    -- with this version of the dashboard.
    dataSetArns :: Prelude.Maybe [Prelude.Text],
    -- | A list of the associated sheets with the unique identifier and name of
    -- each sheet.
    sheets :: Prelude.Maybe [Sheet],
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this dashboard version was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Source entity ARN.
    sourceEntityArn :: Prelude.Maybe Prelude.Text,
    -- | Version number for this version of the dashboard.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | Errors associated with this dashboard version.
    errors :: Prelude.Maybe (Prelude.NonEmpty DashboardError),
    -- | Description.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DashboardVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dashboardVersion_status' - The HTTP status of the request.
--
-- 'themeArn', 'dashboardVersion_themeArn' - The ARN of the theme associated with a version of the dashboard.
--
-- 'dataSetArns', 'dashboardVersion_dataSetArns' - The Amazon Resource Numbers (ARNs) for the datasets that are associated
-- with this version of the dashboard.
--
-- 'sheets', 'dashboardVersion_sheets' - A list of the associated sheets with the unique identifier and name of
-- each sheet.
--
-- 'arn', 'dashboardVersion_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'dashboardVersion_createdTime' - The time that this dashboard version was created.
--
-- 'sourceEntityArn', 'dashboardVersion_sourceEntityArn' - Source entity ARN.
--
-- 'versionNumber', 'dashboardVersion_versionNumber' - Version number for this version of the dashboard.
--
-- 'errors', 'dashboardVersion_errors' - Errors associated with this dashboard version.
--
-- 'description', 'dashboardVersion_description' - Description.
newDashboardVersion ::
  DashboardVersion
newDashboardVersion =
  DashboardVersion'
    { status = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      dataSetArns = Prelude.Nothing,
      sheets = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      sourceEntityArn = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      errors = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The HTTP status of the request.
dashboardVersion_status :: Lens.Lens' DashboardVersion (Prelude.Maybe ResourceStatus)
dashboardVersion_status = Lens.lens (\DashboardVersion' {status} -> status) (\s@DashboardVersion' {} a -> s {status = a} :: DashboardVersion)

-- | The ARN of the theme associated with a version of the dashboard.
dashboardVersion_themeArn :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_themeArn = Lens.lens (\DashboardVersion' {themeArn} -> themeArn) (\s@DashboardVersion' {} a -> s {themeArn = a} :: DashboardVersion)

-- | The Amazon Resource Numbers (ARNs) for the datasets that are associated
-- with this version of the dashboard.
dashboardVersion_dataSetArns :: Lens.Lens' DashboardVersion (Prelude.Maybe [Prelude.Text])
dashboardVersion_dataSetArns = Lens.lens (\DashboardVersion' {dataSetArns} -> dataSetArns) (\s@DashboardVersion' {} a -> s {dataSetArns = a} :: DashboardVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of the associated sheets with the unique identifier and name of
-- each sheet.
dashboardVersion_sheets :: Lens.Lens' DashboardVersion (Prelude.Maybe [Sheet])
dashboardVersion_sheets = Lens.lens (\DashboardVersion' {sheets} -> sheets) (\s@DashboardVersion' {} a -> s {sheets = a} :: DashboardVersion) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource.
dashboardVersion_arn :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_arn = Lens.lens (\DashboardVersion' {arn} -> arn) (\s@DashboardVersion' {} a -> s {arn = a} :: DashboardVersion)

-- | The time that this dashboard version was created.
dashboardVersion_createdTime :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.UTCTime)
dashboardVersion_createdTime = Lens.lens (\DashboardVersion' {createdTime} -> createdTime) (\s@DashboardVersion' {} a -> s {createdTime = a} :: DashboardVersion) Prelude.. Lens.mapping Core._Time

-- | Source entity ARN.
dashboardVersion_sourceEntityArn :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_sourceEntityArn = Lens.lens (\DashboardVersion' {sourceEntityArn} -> sourceEntityArn) (\s@DashboardVersion' {} a -> s {sourceEntityArn = a} :: DashboardVersion)

-- | Version number for this version of the dashboard.
dashboardVersion_versionNumber :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Natural)
dashboardVersion_versionNumber = Lens.lens (\DashboardVersion' {versionNumber} -> versionNumber) (\s@DashboardVersion' {} a -> s {versionNumber = a} :: DashboardVersion)

-- | Errors associated with this dashboard version.
dashboardVersion_errors :: Lens.Lens' DashboardVersion (Prelude.Maybe (Prelude.NonEmpty DashboardError))
dashboardVersion_errors = Lens.lens (\DashboardVersion' {errors} -> errors) (\s@DashboardVersion' {} a -> s {errors = a} :: DashboardVersion) Prelude.. Lens.mapping Lens.coerced

-- | Description.
dashboardVersion_description :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_description = Lens.lens (\DashboardVersion' {description} -> description) (\s@DashboardVersion' {} a -> s {description = a} :: DashboardVersion)

instance Core.FromJSON DashboardVersion where
  parseJSON =
    Core.withObject
      "DashboardVersion"
      ( \x ->
          DashboardVersion'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ThemeArn")
            Prelude.<*> (x Core..:? "DataSetArns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Sheets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "SourceEntityArn")
            Prelude.<*> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "Errors")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable DashboardVersion where
  hashWithSalt salt' DashboardVersion' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` sourceEntityArn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` sheets
      `Prelude.hashWithSalt` dataSetArns
      `Prelude.hashWithSalt` themeArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData DashboardVersion where
  rnf DashboardVersion' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf sourceEntityArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf sheets
      `Prelude.seq` Prelude.rnf dataSetArns
      `Prelude.seq` Prelude.rnf themeArn
