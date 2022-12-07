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
-- Module      : Amazonka.QuickSight.Types.Analysis
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Analysis where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AnalysisError
import Amazonka.QuickSight.Types.ResourceStatus
import Amazonka.QuickSight.Types.Sheet

-- | Metadata structure for an analysis in Amazon QuickSight
--
-- /See:/ 'newAnalysis' smart constructor.
data Analysis = Analysis'
  { -- | The ID of the analysis.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The descriptive name of the analysis.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the theme of the analysis.
    themeArn :: Prelude.Maybe Prelude.Text,
    -- | The time that the analysis was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the analysis.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Status associated with the analysis.
    status :: Prelude.Maybe ResourceStatus,
    -- | The time that the analysis was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Errors associated with the analysis.
    errors :: Prelude.Maybe (Prelude.NonEmpty AnalysisError),
    -- | The ARNs of the datasets of the analysis.
    dataSetArns :: Prelude.Maybe [Prelude.Text],
    -- | A list of the associated sheets with the unique identifier and name of
    -- each sheet.
    sheets :: Prelude.Maybe [Sheet]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Analysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisId', 'analysis_analysisId' - The ID of the analysis.
--
-- 'name', 'analysis_name' - The descriptive name of the analysis.
--
-- 'themeArn', 'analysis_themeArn' - The ARN of the theme of the analysis.
--
-- 'createdTime', 'analysis_createdTime' - The time that the analysis was created.
--
-- 'arn', 'analysis_arn' - The Amazon Resource Name (ARN) of the analysis.
--
-- 'status', 'analysis_status' - Status associated with the analysis.
--
-- 'lastUpdatedTime', 'analysis_lastUpdatedTime' - The time that the analysis was last updated.
--
-- 'errors', 'analysis_errors' - Errors associated with the analysis.
--
-- 'dataSetArns', 'analysis_dataSetArns' - The ARNs of the datasets of the analysis.
--
-- 'sheets', 'analysis_sheets' - A list of the associated sheets with the unique identifier and name of
-- each sheet.
newAnalysis ::
  Analysis
newAnalysis =
  Analysis'
    { analysisId = Prelude.Nothing,
      name = Prelude.Nothing,
      themeArn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      errors = Prelude.Nothing,
      dataSetArns = Prelude.Nothing,
      sheets = Prelude.Nothing
    }

-- | The ID of the analysis.
analysis_analysisId :: Lens.Lens' Analysis (Prelude.Maybe Prelude.Text)
analysis_analysisId = Lens.lens (\Analysis' {analysisId} -> analysisId) (\s@Analysis' {} a -> s {analysisId = a} :: Analysis)

-- | The descriptive name of the analysis.
analysis_name :: Lens.Lens' Analysis (Prelude.Maybe Prelude.Text)
analysis_name = Lens.lens (\Analysis' {name} -> name) (\s@Analysis' {} a -> s {name = a} :: Analysis)

-- | The ARN of the theme of the analysis.
analysis_themeArn :: Lens.Lens' Analysis (Prelude.Maybe Prelude.Text)
analysis_themeArn = Lens.lens (\Analysis' {themeArn} -> themeArn) (\s@Analysis' {} a -> s {themeArn = a} :: Analysis)

-- | The time that the analysis was created.
analysis_createdTime :: Lens.Lens' Analysis (Prelude.Maybe Prelude.UTCTime)
analysis_createdTime = Lens.lens (\Analysis' {createdTime} -> createdTime) (\s@Analysis' {} a -> s {createdTime = a} :: Analysis) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the analysis.
analysis_arn :: Lens.Lens' Analysis (Prelude.Maybe Prelude.Text)
analysis_arn = Lens.lens (\Analysis' {arn} -> arn) (\s@Analysis' {} a -> s {arn = a} :: Analysis)

-- | Status associated with the analysis.
analysis_status :: Lens.Lens' Analysis (Prelude.Maybe ResourceStatus)
analysis_status = Lens.lens (\Analysis' {status} -> status) (\s@Analysis' {} a -> s {status = a} :: Analysis)

-- | The time that the analysis was last updated.
analysis_lastUpdatedTime :: Lens.Lens' Analysis (Prelude.Maybe Prelude.UTCTime)
analysis_lastUpdatedTime = Lens.lens (\Analysis' {lastUpdatedTime} -> lastUpdatedTime) (\s@Analysis' {} a -> s {lastUpdatedTime = a} :: Analysis) Prelude.. Lens.mapping Data._Time

-- | Errors associated with the analysis.
analysis_errors :: Lens.Lens' Analysis (Prelude.Maybe (Prelude.NonEmpty AnalysisError))
analysis_errors = Lens.lens (\Analysis' {errors} -> errors) (\s@Analysis' {} a -> s {errors = a} :: Analysis) Prelude.. Lens.mapping Lens.coerced

-- | The ARNs of the datasets of the analysis.
analysis_dataSetArns :: Lens.Lens' Analysis (Prelude.Maybe [Prelude.Text])
analysis_dataSetArns = Lens.lens (\Analysis' {dataSetArns} -> dataSetArns) (\s@Analysis' {} a -> s {dataSetArns = a} :: Analysis) Prelude.. Lens.mapping Lens.coerced

-- | A list of the associated sheets with the unique identifier and name of
-- each sheet.
analysis_sheets :: Lens.Lens' Analysis (Prelude.Maybe [Sheet])
analysis_sheets = Lens.lens (\Analysis' {sheets} -> sheets) (\s@Analysis' {} a -> s {sheets = a} :: Analysis) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Analysis where
  parseJSON =
    Data.withObject
      "Analysis"
      ( \x ->
          Analysis'
            Prelude.<$> (x Data..:? "AnalysisId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ThemeArn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Errors")
            Prelude.<*> (x Data..:? "DataSetArns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Sheets" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Analysis where
  hashWithSalt _salt Analysis' {..} =
    _salt `Prelude.hashWithSalt` analysisId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` themeArn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` errors
      `Prelude.hashWithSalt` dataSetArns
      `Prelude.hashWithSalt` sheets

instance Prelude.NFData Analysis where
  rnf Analysis' {..} =
    Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf themeArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf dataSetArns
      `Prelude.seq` Prelude.rnf sheets
