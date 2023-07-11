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
-- Module      : Amazonka.CustomerProfiles.Types.WorkflowMetrics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.WorkflowMetrics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types.AppflowIntegrationWorkflowMetrics
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Generic object containing workflow execution metrics.
--
-- /See:/ 'newWorkflowMetrics' smart constructor.
data WorkflowMetrics = WorkflowMetrics'
  { -- | Workflow execution metrics for @APPFLOW_INTEGRATION@ workflow.
    appflowIntegration :: Prelude.Maybe AppflowIntegrationWorkflowMetrics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowMetrics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appflowIntegration', 'workflowMetrics_appflowIntegration' - Workflow execution metrics for @APPFLOW_INTEGRATION@ workflow.
newWorkflowMetrics ::
  WorkflowMetrics
newWorkflowMetrics =
  WorkflowMetrics'
    { appflowIntegration =
        Prelude.Nothing
    }

-- | Workflow execution metrics for @APPFLOW_INTEGRATION@ workflow.
workflowMetrics_appflowIntegration :: Lens.Lens' WorkflowMetrics (Prelude.Maybe AppflowIntegrationWorkflowMetrics)
workflowMetrics_appflowIntegration = Lens.lens (\WorkflowMetrics' {appflowIntegration} -> appflowIntegration) (\s@WorkflowMetrics' {} a -> s {appflowIntegration = a} :: WorkflowMetrics)

instance Data.FromJSON WorkflowMetrics where
  parseJSON =
    Data.withObject
      "WorkflowMetrics"
      ( \x ->
          WorkflowMetrics'
            Prelude.<$> (x Data..:? "AppflowIntegration")
      )

instance Prelude.Hashable WorkflowMetrics where
  hashWithSalt _salt WorkflowMetrics' {..} =
    _salt `Prelude.hashWithSalt` appflowIntegration

instance Prelude.NFData WorkflowMetrics where
  rnf WorkflowMetrics' {..} =
    Prelude.rnf appflowIntegration
