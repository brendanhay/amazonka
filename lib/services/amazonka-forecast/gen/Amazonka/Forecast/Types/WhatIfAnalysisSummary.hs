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
-- Module      : Amazonka.Forecast.Types.WhatIfAnalysisSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Forecast.Types.WhatIfAnalysisSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the what-if analysis properties used in the
-- ListWhatIfAnalyses operation. To get the complete set of properties,
-- call the DescribeWhatIfAnalysis operation, and provide the
-- @WhatIfAnalysisArn@ that is listed in the summary.
--
-- /See:/ 'newWhatIfAnalysisSummary' smart constructor.
data WhatIfAnalysisSummary = WhatIfAnalysisSummary'
  { -- | The last time the resource was modified. The timestamp depends on the
    -- status of the job:
    --
    -- -   @CREATE_PENDING@ - The @CreationTime@.
    --
    -- -   @CREATE_IN_PROGRESS@ - The current timestamp.
    --
    -- -   @CREATE_STOPPING@ - The current timestamp.
    --
    -- -   @CREATE_STOPPED@ - When the job stopped.
    --
    -- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
    lastModificationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the what-if analysis.
    whatIfAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | If an error occurred, an informational message about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the what-if analysis.
    whatIfAnalysisName :: Prelude.Maybe Prelude.Text,
    -- | The status of the what-if analysis. States include:
    --
    -- -   @ACTIVE@
    --
    -- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
    --
    -- -   @CREATE_STOPPING@, @CREATE_STOPPED@
    --
    -- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
    --
    -- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
    -- access the analysis.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the baseline forecast that is being
    -- used in this what-if analysis.
    forecastArn :: Prelude.Maybe Prelude.Text,
    -- | When the what-if analysis was created.
    creationTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WhatIfAnalysisSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModificationTime', 'whatIfAnalysisSummary_lastModificationTime' - The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
--
-- 'whatIfAnalysisArn', 'whatIfAnalysisSummary_whatIfAnalysisArn' - The Amazon Resource Name (ARN) of the what-if analysis.
--
-- 'message', 'whatIfAnalysisSummary_message' - If an error occurred, an informational message about the error.
--
-- 'whatIfAnalysisName', 'whatIfAnalysisSummary_whatIfAnalysisName' - The name of the what-if analysis.
--
-- 'status', 'whatIfAnalysisSummary_status' - The status of the what-if analysis. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
-- access the analysis.
--
-- 'forecastArn', 'whatIfAnalysisSummary_forecastArn' - The Amazon Resource Name (ARN) of the baseline forecast that is being
-- used in this what-if analysis.
--
-- 'creationTime', 'whatIfAnalysisSummary_creationTime' - When the what-if analysis was created.
newWhatIfAnalysisSummary ::
  WhatIfAnalysisSummary
newWhatIfAnalysisSummary =
  WhatIfAnalysisSummary'
    { lastModificationTime =
        Prelude.Nothing,
      whatIfAnalysisArn = Prelude.Nothing,
      message = Prelude.Nothing,
      whatIfAnalysisName = Prelude.Nothing,
      status = Prelude.Nothing,
      forecastArn = Prelude.Nothing,
      creationTime = Prelude.Nothing
    }

-- | The last time the resource was modified. The timestamp depends on the
-- status of the job:
--
-- -   @CREATE_PENDING@ - The @CreationTime@.
--
-- -   @CREATE_IN_PROGRESS@ - The current timestamp.
--
-- -   @CREATE_STOPPING@ - The current timestamp.
--
-- -   @CREATE_STOPPED@ - When the job stopped.
--
-- -   @ACTIVE@ or @CREATE_FAILED@ - When the job finished or failed.
whatIfAnalysisSummary_lastModificationTime :: Lens.Lens' WhatIfAnalysisSummary (Prelude.Maybe Prelude.UTCTime)
whatIfAnalysisSummary_lastModificationTime = Lens.lens (\WhatIfAnalysisSummary' {lastModificationTime} -> lastModificationTime) (\s@WhatIfAnalysisSummary' {} a -> s {lastModificationTime = a} :: WhatIfAnalysisSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the what-if analysis.
whatIfAnalysisSummary_whatIfAnalysisArn :: Lens.Lens' WhatIfAnalysisSummary (Prelude.Maybe Prelude.Text)
whatIfAnalysisSummary_whatIfAnalysisArn = Lens.lens (\WhatIfAnalysisSummary' {whatIfAnalysisArn} -> whatIfAnalysisArn) (\s@WhatIfAnalysisSummary' {} a -> s {whatIfAnalysisArn = a} :: WhatIfAnalysisSummary)

-- | If an error occurred, an informational message about the error.
whatIfAnalysisSummary_message :: Lens.Lens' WhatIfAnalysisSummary (Prelude.Maybe Prelude.Text)
whatIfAnalysisSummary_message = Lens.lens (\WhatIfAnalysisSummary' {message} -> message) (\s@WhatIfAnalysisSummary' {} a -> s {message = a} :: WhatIfAnalysisSummary)

-- | The name of the what-if analysis.
whatIfAnalysisSummary_whatIfAnalysisName :: Lens.Lens' WhatIfAnalysisSummary (Prelude.Maybe Prelude.Text)
whatIfAnalysisSummary_whatIfAnalysisName = Lens.lens (\WhatIfAnalysisSummary' {whatIfAnalysisName} -> whatIfAnalysisName) (\s@WhatIfAnalysisSummary' {} a -> s {whatIfAnalysisName = a} :: WhatIfAnalysisSummary)

-- | The status of the what-if analysis. States include:
--
-- -   @ACTIVE@
--
-- -   @CREATE_PENDING@, @CREATE_IN_PROGRESS@, @CREATE_FAILED@
--
-- -   @CREATE_STOPPING@, @CREATE_STOPPED@
--
-- -   @DELETE_PENDING@, @DELETE_IN_PROGRESS@, @DELETE_FAILED@
--
-- The @Status@ of the what-if analysis must be @ACTIVE@ before you can
-- access the analysis.
whatIfAnalysisSummary_status :: Lens.Lens' WhatIfAnalysisSummary (Prelude.Maybe Prelude.Text)
whatIfAnalysisSummary_status = Lens.lens (\WhatIfAnalysisSummary' {status} -> status) (\s@WhatIfAnalysisSummary' {} a -> s {status = a} :: WhatIfAnalysisSummary)

-- | The Amazon Resource Name (ARN) of the baseline forecast that is being
-- used in this what-if analysis.
whatIfAnalysisSummary_forecastArn :: Lens.Lens' WhatIfAnalysisSummary (Prelude.Maybe Prelude.Text)
whatIfAnalysisSummary_forecastArn = Lens.lens (\WhatIfAnalysisSummary' {forecastArn} -> forecastArn) (\s@WhatIfAnalysisSummary' {} a -> s {forecastArn = a} :: WhatIfAnalysisSummary)

-- | When the what-if analysis was created.
whatIfAnalysisSummary_creationTime :: Lens.Lens' WhatIfAnalysisSummary (Prelude.Maybe Prelude.UTCTime)
whatIfAnalysisSummary_creationTime = Lens.lens (\WhatIfAnalysisSummary' {creationTime} -> creationTime) (\s@WhatIfAnalysisSummary' {} a -> s {creationTime = a} :: WhatIfAnalysisSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON WhatIfAnalysisSummary where
  parseJSON =
    Data.withObject
      "WhatIfAnalysisSummary"
      ( \x ->
          WhatIfAnalysisSummary'
            Prelude.<$> (x Data..:? "LastModificationTime")
            Prelude.<*> (x Data..:? "WhatIfAnalysisArn")
            Prelude.<*> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "WhatIfAnalysisName")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "ForecastArn")
            Prelude.<*> (x Data..:? "CreationTime")
      )

instance Prelude.Hashable WhatIfAnalysisSummary where
  hashWithSalt _salt WhatIfAnalysisSummary' {..} =
    _salt `Prelude.hashWithSalt` lastModificationTime
      `Prelude.hashWithSalt` whatIfAnalysisArn
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` whatIfAnalysisName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` forecastArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData WhatIfAnalysisSummary where
  rnf WhatIfAnalysisSummary' {..} =
    Prelude.rnf lastModificationTime
      `Prelude.seq` Prelude.rnf whatIfAnalysisArn
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf whatIfAnalysisName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf forecastArn
      `Prelude.seq` Prelude.rnf creationTime
