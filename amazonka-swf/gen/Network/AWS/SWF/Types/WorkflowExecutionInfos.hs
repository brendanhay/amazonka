{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionInfos
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionInfos where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SWF.Types.WorkflowExecutionInfo

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
workflowExecutionInfos_executionInfos = Lens.lens (\WorkflowExecutionInfos' {executionInfos} -> executionInfos) (\s@WorkflowExecutionInfos' {} a -> s {executionInfos = a} :: WorkflowExecutionInfos) Prelude.. Prelude._Coerce

instance Prelude.FromJSON WorkflowExecutionInfos where
  parseJSON =
    Prelude.withObject
      "WorkflowExecutionInfos"
      ( \x ->
          WorkflowExecutionInfos'
            Prelude.<$> (x Prelude..:? "nextPageToken")
            Prelude.<*> ( x Prelude..:? "executionInfos"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable WorkflowExecutionInfos

instance Prelude.NFData WorkflowExecutionInfos
