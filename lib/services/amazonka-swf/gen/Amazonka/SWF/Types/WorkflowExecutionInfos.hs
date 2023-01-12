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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionInfos
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionInfos where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.WorkflowExecutionInfo

-- | Contains a paginated list of information about workflow executions.
--
-- /See:/ 'newWorkflowExecutionInfos' smart constructor.
data WorkflowExecutionInfos = WorkflowExecutionInfos'
  { -- | If a @NextPageToken@ was returned by a previous call, there are more
    -- results available. To retrieve the next page of results, make the call
    -- again using the returned token in @nextPageToken@. Keep all other
    -- arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be
    -- returned in a single call.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The list of workflow information structures.
    executionInfos :: [WorkflowExecutionInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionInfos' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'workflowExecutionInfos_nextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
--
-- 'executionInfos', 'workflowExecutionInfos_executionInfos' - The list of workflow information structures.
newWorkflowExecutionInfos ::
  WorkflowExecutionInfos
newWorkflowExecutionInfos =
  WorkflowExecutionInfos'
    { nextPageToken =
        Prelude.Nothing,
      executionInfos = Prelude.mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
workflowExecutionInfos_nextPageToken :: Lens.Lens' WorkflowExecutionInfos (Prelude.Maybe Prelude.Text)
workflowExecutionInfos_nextPageToken = Lens.lens (\WorkflowExecutionInfos' {nextPageToken} -> nextPageToken) (\s@WorkflowExecutionInfos' {} a -> s {nextPageToken = a} :: WorkflowExecutionInfos)

-- | The list of workflow information structures.
workflowExecutionInfos_executionInfos :: Lens.Lens' WorkflowExecutionInfos [WorkflowExecutionInfo]
workflowExecutionInfos_executionInfos = Lens.lens (\WorkflowExecutionInfos' {executionInfos} -> executionInfos) (\s@WorkflowExecutionInfos' {} a -> s {executionInfos = a} :: WorkflowExecutionInfos) Prelude.. Lens.coerced

instance Data.FromJSON WorkflowExecutionInfos where
  parseJSON =
    Data.withObject
      "WorkflowExecutionInfos"
      ( \x ->
          WorkflowExecutionInfos'
            Prelude.<$> (x Data..:? "nextPageToken")
            Prelude.<*> ( x Data..:? "executionInfos"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable WorkflowExecutionInfos where
  hashWithSalt _salt WorkflowExecutionInfos' {..} =
    _salt `Prelude.hashWithSalt` nextPageToken
      `Prelude.hashWithSalt` executionInfos

instance Prelude.NFData WorkflowExecutionInfos where
  rnf WorkflowExecutionInfos' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf executionInfos
