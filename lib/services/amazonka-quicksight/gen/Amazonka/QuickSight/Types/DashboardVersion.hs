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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DashboardVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardError
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.Sheet

-- | Dashboard version.
--
-- /See:/ 'newDashboardVersion' smart constructor.
data DashboardVersion = DashboardVersion'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this dashboard version was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Numbers (ARNs) for the datasets that are associated
    -- with this version of the dashboard.
    dataSetArns :: Prelude.Maybe [Prelude.Text],
    -- | Description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Errors associated with this dashboard version.
    errors :: Prelude.Maybe (Prelude.NonEmpty DashboardError),
    -- | A list of the associated sheets with the unique identifier and name of
    -- each sheet.
    sheets :: Prelude.Maybe [Sheet],
    -- | Source entity ARN.
    sourceEntityArn :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Maybe ResourceStatus,
    -- | The ARN of the theme associated with a version of the dashboard.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | Version number for this version of the dashboard.
    versionNumber :: Prelude.Maybe Prelude.Natural
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
-- 'arn', 'dashboardVersion_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'dashboardVersion_createdTime' - The time that this dashboard version was created.
--
-- 'dataSetArns', 'dashboardVersion_dataSetArns' - The Amazon Resource Numbers (ARNs) for the datasets that are associated
-- with this version of the dashboard.
--
-- 'description', 'dashboardVersion_description' - Description.
--
-- 'errors', 'dashboardVersion_errors' - Errors associated with this dashboard version.
--
-- 'sheets', 'dashboardVersion_sheets' - A list of the associated sheets with the unique identifier and name of
-- each sheet.
--
-- 'sourceEntityArn', 'dashboardVersion_sourceEntityArn' - Source entity ARN.
--
-- 'status', 'dashboardVersion_status' - The HTTP status of the request.
--
-- 'themeArn', 'dashboardVersion_themeArn' - The ARN of the theme associated with a version of the dashboard.
--
-- 'versionNumber', 'dashboardVersion_versionNumber' - Version number for this version of the dashboard.
newDashboardVersion ::
  DashboardVersion
newDashboardVersion =
  DashboardVersion'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dataSetArns = Prelude.Nothing,
      description = Prelude.Nothing,
      errors = Prelude.Nothing,
      sheets = Prelude.Nothing,
      sourceEntityArn = Prelude.Nothing,
      status = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
dashboardVersion_arn :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_arn = Lens.lens (\DashboardVersion' {arn} -> arn) (\s@DashboardVersion' {} a -> s {arn = a} :: DashboardVersion)

-- | The time that this dashboard version was created.
dashboardVersion_createdTime :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.UTCTime)
dashboardVersion_createdTime = Lens.lens (\DashboardVersion' {createdTime} -> createdTime) (\s@DashboardVersion' {} a -> s {createdTime = a} :: DashboardVersion) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Numbers (ARNs) for the datasets that are associated
-- with this version of the dashboard.
dashboardVersion_dataSetArns :: Lens.Lens' DashboardVersion (Prelude.Maybe [Prelude.Text])
dashboardVersion_dataSetArns = Lens.lens (\DashboardVersion' {dataSetArns} -> dataSetArns) (\s@DashboardVersion' {} a -> s {dataSetArns = a} :: DashboardVersion) Prelude.. Lens.mapping Lens.coerced

-- | Description.
dashboardVersion_description :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_description = Lens.lens (\DashboardVersion' {description} -> description) (\s@DashboardVersion' {} a -> s {description = a} :: DashboardVersion)

-- | Errors associated with this dashboard version.
dashboardVersion_errors :: Lens.Lens' DashboardVersion (Prelude.Maybe (Prelude.NonEmpty DashboardError))
dashboardVersion_errors = Lens.lens (\DashboardVersion' {errors} -> errors) (\s@DashboardVersion' {} a -> s {errors = a} :: DashboardVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of the associated sheets with the unique identifier and name of
-- each sheet.
dashboardVersion_sheets :: Lens.Lens' DashboardVersion (Prelude.Maybe [Sheet])
dashboardVersion_sheets = Lens.lens (\DashboardVersion' {sheets} -> sheets) (\s@DashboardVersion' {} a -> s {sheets = a} :: DashboardVersion) Prelude.. Lens.mapping Lens.coerced

-- | Source entity ARN.
dashboardVersion_sourceEntityArn :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_sourceEntityArn = Lens.lens (\DashboardVersion' {sourceEntityArn} -> sourceEntityArn) (\s@DashboardVersion' {} a -> s {sourceEntityArn = a} :: DashboardVersion)

-- | The HTTP status of the request.
dashboardVersion_status :: Lens.Lens' DashboardVersion (Prelude.Maybe ResourceStatus)
dashboardVersion_status = Lens.lens (\DashboardVersion' {status} -> status) (\s@DashboardVersion' {} a -> s {status = a} :: DashboardVersion)

-- | The ARN of the theme associated with a version of the dashboard.
dashboardVersion_themeArn :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Text)
dashboardVersion_themeArn = Lens.lens (\DashboardVersion' {themeArn} -> themeArn) (\s@DashboardVersion' {} a -> s {themeArn = a} :: DashboardVersion)

-- | Version number for this version of the dashboard.
dashboardVersion_versionNumber :: Lens.Lens' DashboardVersion (Prelude.Maybe Prelude.Natural)
dashboardVersion_versionNumber = Lens.lens (\DashboardVersion' {versionNumber} -> versionNumber) (\s@DashboardVersion' {} a -> s {versionNumber = a} :: DashboardVersion)

instance Data.FromJSON DashboardVersion where
  parseJSON =
    Data.withObject
      "DashboardVersion"
      ( \x ->
          DashboardVersion'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DataSetArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Errors")
            Prelude.<*> (x Data..:? "Sheets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceEntityArn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "ThemeArn")
            Prelude.<*> (x Data..:? "VersionNumber")
      )

instance Prelude.Hashable DashboardVersion where
  hashWithSalt _salt DashboardVersion' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dataSetArns
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` sheets
      `Prelude.hashWithSalt` sourceEntityArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` themeArn
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData DashboardVersion where
  rnf DashboardVersion' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dataSetArns
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf sheets
      `Prelude.seq` Prelude.rnf sourceEntityArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf versionNumber
