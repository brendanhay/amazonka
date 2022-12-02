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
-- Module      : Amazonka.QuickSight.Types.AnalysisSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ResourceStatus

-- | The summary metadata that describes an analysis.
--
-- /See:/ 'newAnalysisSummary' smart constructor.
data AnalysisSummary = AnalysisSummary'
  { -- | The ID of the analysis. This ID displays in the URL.
    analysisId :: Prelude.Maybe Prelude.Text,
    -- | The name of the analysis. This name is displayed in the Amazon
    -- QuickSight console.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time that the analysis was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) for the analysis.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The last known status for the analysis.
    status :: Prelude.Maybe ResourceStatus,
    -- | The time that the analysis was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisId', 'analysisSummary_analysisId' - The ID of the analysis. This ID displays in the URL.
--
-- 'name', 'analysisSummary_name' - The name of the analysis. This name is displayed in the Amazon
-- QuickSight console.
--
-- 'createdTime', 'analysisSummary_createdTime' - The time that the analysis was created.
--
-- 'arn', 'analysisSummary_arn' - The Amazon Resource Name (ARN) for the analysis.
--
-- 'status', 'analysisSummary_status' - The last known status for the analysis.
--
-- 'lastUpdatedTime', 'analysisSummary_lastUpdatedTime' - The time that the analysis was last updated.
newAnalysisSummary ::
  AnalysisSummary
newAnalysisSummary =
  AnalysisSummary'
    { analysisId = Prelude.Nothing,
      name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | The ID of the analysis. This ID displays in the URL.
analysisSummary_analysisId :: Lens.Lens' AnalysisSummary (Prelude.Maybe Prelude.Text)
analysisSummary_analysisId = Lens.lens (\AnalysisSummary' {analysisId} -> analysisId) (\s@AnalysisSummary' {} a -> s {analysisId = a} :: AnalysisSummary)

-- | The name of the analysis. This name is displayed in the Amazon
-- QuickSight console.
analysisSummary_name :: Lens.Lens' AnalysisSummary (Prelude.Maybe Prelude.Text)
analysisSummary_name = Lens.lens (\AnalysisSummary' {name} -> name) (\s@AnalysisSummary' {} a -> s {name = a} :: AnalysisSummary)

-- | The time that the analysis was created.
analysisSummary_createdTime :: Lens.Lens' AnalysisSummary (Prelude.Maybe Prelude.UTCTime)
analysisSummary_createdTime = Lens.lens (\AnalysisSummary' {createdTime} -> createdTime) (\s@AnalysisSummary' {} a -> s {createdTime = a} :: AnalysisSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) for the analysis.
analysisSummary_arn :: Lens.Lens' AnalysisSummary (Prelude.Maybe Prelude.Text)
analysisSummary_arn = Lens.lens (\AnalysisSummary' {arn} -> arn) (\s@AnalysisSummary' {} a -> s {arn = a} :: AnalysisSummary)

-- | The last known status for the analysis.
analysisSummary_status :: Lens.Lens' AnalysisSummary (Prelude.Maybe ResourceStatus)
analysisSummary_status = Lens.lens (\AnalysisSummary' {status} -> status) (\s@AnalysisSummary' {} a -> s {status = a} :: AnalysisSummary)

-- | The time that the analysis was last updated.
analysisSummary_lastUpdatedTime :: Lens.Lens' AnalysisSummary (Prelude.Maybe Prelude.UTCTime)
analysisSummary_lastUpdatedTime = Lens.lens (\AnalysisSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@AnalysisSummary' {} a -> s {lastUpdatedTime = a} :: AnalysisSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON AnalysisSummary where
  parseJSON =
    Data.withObject
      "AnalysisSummary"
      ( \x ->
          AnalysisSummary'
            Prelude.<$> (x Data..:? "AnalysisId")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
      )

instance Prelude.Hashable AnalysisSummary where
  hashWithSalt _salt AnalysisSummary' {..} =
    _salt `Prelude.hashWithSalt` analysisId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedTime

instance Prelude.NFData AnalysisSummary where
  rnf AnalysisSummary' {..} =
    Prelude.rnf analysisId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedTime
