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
-- Module      : Amazonka.Evidently.Types.ExperimentReport
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.ExperimentReport where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.ExperimentReportName
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains results of an experiment.
--
-- /See:/ 'newExperimentReport' smart constructor.
data ExperimentReport = ExperimentReport'
  { -- | The type of analysis used for this report.
    reportName :: Prelude.Maybe ExperimentReportName,
    -- | The name of the variation that this report pertains to.
    treatmentName :: Prelude.Maybe Prelude.Text,
    -- | The name of the metric that is analyzed in this experiment report.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The content of the report.
    content :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentReport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportName', 'experimentReport_reportName' - The type of analysis used for this report.
--
-- 'treatmentName', 'experimentReport_treatmentName' - The name of the variation that this report pertains to.
--
-- 'metricName', 'experimentReport_metricName' - The name of the metric that is analyzed in this experiment report.
--
-- 'content', 'experimentReport_content' - The content of the report.
newExperimentReport ::
  ExperimentReport
newExperimentReport =
  ExperimentReport'
    { reportName = Prelude.Nothing,
      treatmentName = Prelude.Nothing,
      metricName = Prelude.Nothing,
      content = Prelude.Nothing
    }

-- | The type of analysis used for this report.
experimentReport_reportName :: Lens.Lens' ExperimentReport (Prelude.Maybe ExperimentReportName)
experimentReport_reportName = Lens.lens (\ExperimentReport' {reportName} -> reportName) (\s@ExperimentReport' {} a -> s {reportName = a} :: ExperimentReport)

-- | The name of the variation that this report pertains to.
experimentReport_treatmentName :: Lens.Lens' ExperimentReport (Prelude.Maybe Prelude.Text)
experimentReport_treatmentName = Lens.lens (\ExperimentReport' {treatmentName} -> treatmentName) (\s@ExperimentReport' {} a -> s {treatmentName = a} :: ExperimentReport)

-- | The name of the metric that is analyzed in this experiment report.
experimentReport_metricName :: Lens.Lens' ExperimentReport (Prelude.Maybe Prelude.Text)
experimentReport_metricName = Lens.lens (\ExperimentReport' {metricName} -> metricName) (\s@ExperimentReport' {} a -> s {metricName = a} :: ExperimentReport)

-- | The content of the report.
experimentReport_content :: Lens.Lens' ExperimentReport (Prelude.Maybe Prelude.Text)
experimentReport_content = Lens.lens (\ExperimentReport' {content} -> content) (\s@ExperimentReport' {} a -> s {content = a} :: ExperimentReport)

instance Data.FromJSON ExperimentReport where
  parseJSON =
    Data.withObject
      "ExperimentReport"
      ( \x ->
          ExperimentReport'
            Prelude.<$> (x Data..:? "reportName")
            Prelude.<*> (x Data..:? "treatmentName")
            Prelude.<*> (x Data..:? "metricName")
            Prelude.<*> (x Data..:? "content")
      )

instance Prelude.Hashable ExperimentReport where
  hashWithSalt _salt ExperimentReport' {..} =
    _salt `Prelude.hashWithSalt` reportName
      `Prelude.hashWithSalt` treatmentName
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` content

instance Prelude.NFData ExperimentReport where
  rnf ExperimentReport' {..} =
    Prelude.rnf reportName
      `Prelude.seq` Prelude.rnf treatmentName
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf content
