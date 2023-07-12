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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerStatusSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerStatusSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.RunTimeAssessmentStatus
import qualified Amazonka.Prelude as Prelude

-- | The status summary of the server analysis.
--
-- /See:/ 'newServerStatusSummary' smart constructor.
data ServerStatusSummary = ServerStatusSummary'
  { -- | The number of servers successfully analyzed, partially successful or
    -- failed analysis.
    count :: Prelude.Maybe Prelude.Int,
    -- | The status of the run time.
    runTimeAssessmentStatus :: Prelude.Maybe RunTimeAssessmentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerStatusSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'serverStatusSummary_count' - The number of servers successfully analyzed, partially successful or
-- failed analysis.
--
-- 'runTimeAssessmentStatus', 'serverStatusSummary_runTimeAssessmentStatus' - The status of the run time.
newServerStatusSummary ::
  ServerStatusSummary
newServerStatusSummary =
  ServerStatusSummary'
    { count = Prelude.Nothing,
      runTimeAssessmentStatus = Prelude.Nothing
    }

-- | The number of servers successfully analyzed, partially successful or
-- failed analysis.
serverStatusSummary_count :: Lens.Lens' ServerStatusSummary (Prelude.Maybe Prelude.Int)
serverStatusSummary_count = Lens.lens (\ServerStatusSummary' {count} -> count) (\s@ServerStatusSummary' {} a -> s {count = a} :: ServerStatusSummary)

-- | The status of the run time.
serverStatusSummary_runTimeAssessmentStatus :: Lens.Lens' ServerStatusSummary (Prelude.Maybe RunTimeAssessmentStatus)
serverStatusSummary_runTimeAssessmentStatus = Lens.lens (\ServerStatusSummary' {runTimeAssessmentStatus} -> runTimeAssessmentStatus) (\s@ServerStatusSummary' {} a -> s {runTimeAssessmentStatus = a} :: ServerStatusSummary)

instance Data.FromJSON ServerStatusSummary where
  parseJSON =
    Data.withObject
      "ServerStatusSummary"
      ( \x ->
          ServerStatusSummary'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "runTimeAssessmentStatus")
      )

instance Prelude.Hashable ServerStatusSummary where
  hashWithSalt _salt ServerStatusSummary' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` runTimeAssessmentStatus

instance Prelude.NFData ServerStatusSummary where
  rnf ServerStatusSummary' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf runTimeAssessmentStatus
