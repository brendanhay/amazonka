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
-- Module      : Amazonka.MigrationHubStrategy.Types.ApplicationComponentStatusSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ApplicationComponentStatusSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.SrcCodeOrDbAnalysisStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary of the analysis status of the application component.
--
-- /See:/ 'newApplicationComponentStatusSummary' smart constructor.
data ApplicationComponentStatusSummary = ApplicationComponentStatusSummary'
  { -- | The number of application components successfully analyzed, partially
    -- successful or failed analysis.
    count :: Prelude.Maybe Prelude.Int,
    -- | The status of database analysis.
    srcCodeOrDbAnalysisStatus :: Prelude.Maybe SrcCodeOrDbAnalysisStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationComponentStatusSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'applicationComponentStatusSummary_count' - The number of application components successfully analyzed, partially
-- successful or failed analysis.
--
-- 'srcCodeOrDbAnalysisStatus', 'applicationComponentStatusSummary_srcCodeOrDbAnalysisStatus' - The status of database analysis.
newApplicationComponentStatusSummary ::
  ApplicationComponentStatusSummary
newApplicationComponentStatusSummary =
  ApplicationComponentStatusSummary'
    { count =
        Prelude.Nothing,
      srcCodeOrDbAnalysisStatus =
        Prelude.Nothing
    }

-- | The number of application components successfully analyzed, partially
-- successful or failed analysis.
applicationComponentStatusSummary_count :: Lens.Lens' ApplicationComponentStatusSummary (Prelude.Maybe Prelude.Int)
applicationComponentStatusSummary_count = Lens.lens (\ApplicationComponentStatusSummary' {count} -> count) (\s@ApplicationComponentStatusSummary' {} a -> s {count = a} :: ApplicationComponentStatusSummary)

-- | The status of database analysis.
applicationComponentStatusSummary_srcCodeOrDbAnalysisStatus :: Lens.Lens' ApplicationComponentStatusSummary (Prelude.Maybe SrcCodeOrDbAnalysisStatus)
applicationComponentStatusSummary_srcCodeOrDbAnalysisStatus = Lens.lens (\ApplicationComponentStatusSummary' {srcCodeOrDbAnalysisStatus} -> srcCodeOrDbAnalysisStatus) (\s@ApplicationComponentStatusSummary' {} a -> s {srcCodeOrDbAnalysisStatus = a} :: ApplicationComponentStatusSummary)

instance
  Data.FromJSON
    ApplicationComponentStatusSummary
  where
  parseJSON =
    Data.withObject
      "ApplicationComponentStatusSummary"
      ( \x ->
          ApplicationComponentStatusSummary'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "srcCodeOrDbAnalysisStatus")
      )

instance
  Prelude.Hashable
    ApplicationComponentStatusSummary
  where
  hashWithSalt
    _salt
    ApplicationComponentStatusSummary' {..} =
      _salt `Prelude.hashWithSalt` count
        `Prelude.hashWithSalt` srcCodeOrDbAnalysisStatus

instance
  Prelude.NFData
    ApplicationComponentStatusSummary
  where
  rnf ApplicationComponentStatusSummary' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf srcCodeOrDbAnalysisStatus
