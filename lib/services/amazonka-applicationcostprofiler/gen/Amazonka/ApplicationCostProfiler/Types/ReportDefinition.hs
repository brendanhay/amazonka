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
-- Module      : Amazonka.ApplicationCostProfiler.Types.ReportDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationCostProfiler.Types.ReportDefinition where

import Amazonka.ApplicationCostProfiler.Types.Format
import Amazonka.ApplicationCostProfiler.Types.ReportFrequency
import Amazonka.ApplicationCostProfiler.Types.S3Location
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration of a report in AWS Application Cost Profiler.
--
-- /See:/ 'newReportDefinition' smart constructor.
data ReportDefinition = ReportDefinition'
  { -- | Timestamp (milliseconds) when this report definition was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The location in Amazon Simple Storage Service (Amazon S3) the reports
    -- should be saved to.
    destinationS3Location :: Prelude.Maybe S3Location,
    -- | The cadence at which the report is generated.
    reportFrequency :: Prelude.Maybe ReportFrequency,
    -- | Description of the report
    reportDescription :: Prelude.Maybe Prelude.Text,
    -- | The format used for the generated reports.
    format :: Prelude.Maybe Format,
    -- | The ID of the report.
    reportId :: Prelude.Maybe Prelude.Text,
    -- | Timestamp (milliseconds) when this report definition was created.
    createdAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReportDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedAt', 'reportDefinition_lastUpdatedAt' - Timestamp (milliseconds) when this report definition was last updated.
--
-- 'destinationS3Location', 'reportDefinition_destinationS3Location' - The location in Amazon Simple Storage Service (Amazon S3) the reports
-- should be saved to.
--
-- 'reportFrequency', 'reportDefinition_reportFrequency' - The cadence at which the report is generated.
--
-- 'reportDescription', 'reportDefinition_reportDescription' - Description of the report
--
-- 'format', 'reportDefinition_format' - The format used for the generated reports.
--
-- 'reportId', 'reportDefinition_reportId' - The ID of the report.
--
-- 'createdAt', 'reportDefinition_createdAt' - Timestamp (milliseconds) when this report definition was created.
newReportDefinition ::
  ReportDefinition
newReportDefinition =
  ReportDefinition'
    { lastUpdatedAt = Prelude.Nothing,
      destinationS3Location = Prelude.Nothing,
      reportFrequency = Prelude.Nothing,
      reportDescription = Prelude.Nothing,
      format = Prelude.Nothing,
      reportId = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | Timestamp (milliseconds) when this report definition was last updated.
reportDefinition_lastUpdatedAt :: Lens.Lens' ReportDefinition (Prelude.Maybe Prelude.UTCTime)
reportDefinition_lastUpdatedAt = Lens.lens (\ReportDefinition' {lastUpdatedAt} -> lastUpdatedAt) (\s@ReportDefinition' {} a -> s {lastUpdatedAt = a} :: ReportDefinition) Prelude.. Lens.mapping Core._Time

-- | The location in Amazon Simple Storage Service (Amazon S3) the reports
-- should be saved to.
reportDefinition_destinationS3Location :: Lens.Lens' ReportDefinition (Prelude.Maybe S3Location)
reportDefinition_destinationS3Location = Lens.lens (\ReportDefinition' {destinationS3Location} -> destinationS3Location) (\s@ReportDefinition' {} a -> s {destinationS3Location = a} :: ReportDefinition)

-- | The cadence at which the report is generated.
reportDefinition_reportFrequency :: Lens.Lens' ReportDefinition (Prelude.Maybe ReportFrequency)
reportDefinition_reportFrequency = Lens.lens (\ReportDefinition' {reportFrequency} -> reportFrequency) (\s@ReportDefinition' {} a -> s {reportFrequency = a} :: ReportDefinition)

-- | Description of the report
reportDefinition_reportDescription :: Lens.Lens' ReportDefinition (Prelude.Maybe Prelude.Text)
reportDefinition_reportDescription = Lens.lens (\ReportDefinition' {reportDescription} -> reportDescription) (\s@ReportDefinition' {} a -> s {reportDescription = a} :: ReportDefinition)

-- | The format used for the generated reports.
reportDefinition_format :: Lens.Lens' ReportDefinition (Prelude.Maybe Format)
reportDefinition_format = Lens.lens (\ReportDefinition' {format} -> format) (\s@ReportDefinition' {} a -> s {format = a} :: ReportDefinition)

-- | The ID of the report.
reportDefinition_reportId :: Lens.Lens' ReportDefinition (Prelude.Maybe Prelude.Text)
reportDefinition_reportId = Lens.lens (\ReportDefinition' {reportId} -> reportId) (\s@ReportDefinition' {} a -> s {reportId = a} :: ReportDefinition)

-- | Timestamp (milliseconds) when this report definition was created.
reportDefinition_createdAt :: Lens.Lens' ReportDefinition (Prelude.Maybe Prelude.UTCTime)
reportDefinition_createdAt = Lens.lens (\ReportDefinition' {createdAt} -> createdAt) (\s@ReportDefinition' {} a -> s {createdAt = a} :: ReportDefinition) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ReportDefinition where
  parseJSON =
    Core.withObject
      "ReportDefinition"
      ( \x ->
          ReportDefinition'
            Prelude.<$> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "destinationS3Location")
            Prelude.<*> (x Core..:? "reportFrequency")
            Prelude.<*> (x Core..:? "reportDescription")
            Prelude.<*> (x Core..:? "format")
            Prelude.<*> (x Core..:? "reportId")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable ReportDefinition where
  hashWithSalt _salt ReportDefinition' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` destinationS3Location
      `Prelude.hashWithSalt` reportFrequency
      `Prelude.hashWithSalt` reportDescription
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` reportId
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData ReportDefinition where
  rnf ReportDefinition' {..} =
    Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf destinationS3Location
      `Prelude.seq` Prelude.rnf reportFrequency
      `Prelude.seq` Prelude.rnf reportDescription
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf reportId
      `Prelude.seq` Prelude.rnf createdAt
