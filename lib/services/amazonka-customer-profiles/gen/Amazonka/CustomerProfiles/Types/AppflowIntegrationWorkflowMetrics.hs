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
-- Module      : Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowMetrics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Workflow specific execution metrics for @APPFLOW_INTEGRATION@ workflow.
--
-- /See:/ 'newAppflowIntegrationWorkflowMetrics' smart constructor.
data AppflowIntegrationWorkflowMetrics = AppflowIntegrationWorkflowMetrics'
  { -- | Number of records processed in @APPFLOW_INTEGRATION@ workflow.
    recordsProcessed :: Prelude.Integer,
    -- | Total steps completed in @APPFLOW_INTEGRATION@ workflow.
    stepsCompleted :: Prelude.Integer,
    -- | Total steps in @APPFLOW_INTEGRATION@ workflow.
    totalSteps :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppflowIntegrationWorkflowMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordsProcessed', 'appflowIntegrationWorkflowMetrics_recordsProcessed' - Number of records processed in @APPFLOW_INTEGRATION@ workflow.
--
-- 'stepsCompleted', 'appflowIntegrationWorkflowMetrics_stepsCompleted' - Total steps completed in @APPFLOW_INTEGRATION@ workflow.
--
-- 'totalSteps', 'appflowIntegrationWorkflowMetrics_totalSteps' - Total steps in @APPFLOW_INTEGRATION@ workflow.
newAppflowIntegrationWorkflowMetrics ::
  -- | 'recordsProcessed'
  Prelude.Integer ->
  -- | 'stepsCompleted'
  Prelude.Integer ->
  -- | 'totalSteps'
  Prelude.Integer ->
  AppflowIntegrationWorkflowMetrics
newAppflowIntegrationWorkflowMetrics
  pRecordsProcessed_
  pStepsCompleted_
  pTotalSteps_ =
    AppflowIntegrationWorkflowMetrics'
      { recordsProcessed =
          pRecordsProcessed_,
        stepsCompleted = pStepsCompleted_,
        totalSteps = pTotalSteps_
      }

-- | Number of records processed in @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowMetrics_recordsProcessed :: Lens.Lens' AppflowIntegrationWorkflowMetrics Prelude.Integer
appflowIntegrationWorkflowMetrics_recordsProcessed = Lens.lens (\AppflowIntegrationWorkflowMetrics' {recordsProcessed} -> recordsProcessed) (\s@AppflowIntegrationWorkflowMetrics' {} a -> s {recordsProcessed = a} :: AppflowIntegrationWorkflowMetrics)

-- | Total steps completed in @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowMetrics_stepsCompleted :: Lens.Lens' AppflowIntegrationWorkflowMetrics Prelude.Integer
appflowIntegrationWorkflowMetrics_stepsCompleted = Lens.lens (\AppflowIntegrationWorkflowMetrics' {stepsCompleted} -> stepsCompleted) (\s@AppflowIntegrationWorkflowMetrics' {} a -> s {stepsCompleted = a} :: AppflowIntegrationWorkflowMetrics)

-- | Total steps in @APPFLOW_INTEGRATION@ workflow.
appflowIntegrationWorkflowMetrics_totalSteps :: Lens.Lens' AppflowIntegrationWorkflowMetrics Prelude.Integer
appflowIntegrationWorkflowMetrics_totalSteps = Lens.lens (\AppflowIntegrationWorkflowMetrics' {totalSteps} -> totalSteps) (\s@AppflowIntegrationWorkflowMetrics' {} a -> s {totalSteps = a} :: AppflowIntegrationWorkflowMetrics)

instance
  Data.FromJSON
    AppflowIntegrationWorkflowMetrics
  where
  parseJSON =
    Data.withObject
      "AppflowIntegrationWorkflowMetrics"
      ( \x ->
          AppflowIntegrationWorkflowMetrics'
            Prelude.<$> (x Data..: "RecordsProcessed")
            Prelude.<*> (x Data..: "StepsCompleted")
            Prelude.<*> (x Data..: "TotalSteps")
      )

instance
  Prelude.Hashable
    AppflowIntegrationWorkflowMetrics
  where
  hashWithSalt
    _salt
    AppflowIntegrationWorkflowMetrics' {..} =
      _salt `Prelude.hashWithSalt` recordsProcessed
        `Prelude.hashWithSalt` stepsCompleted
        `Prelude.hashWithSalt` totalSteps

instance
  Prelude.NFData
    AppflowIntegrationWorkflowMetrics
  where
  rnf AppflowIntegrationWorkflowMetrics' {..} =
    Prelude.rnf recordsProcessed
      `Prelude.seq` Prelude.rnf stepsCompleted
      `Prelude.seq` Prelude.rnf totalSteps
