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
-- Module      : Amazonka.Transfer.Types.WorkflowDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.WorkflowDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.WorkflowDetail

-- | Container for the @WorkflowDetail@ data type. It is used by actions that
-- trigger a workflow to begin execution.
--
-- /See:/ 'newWorkflowDetails' smart constructor.
data WorkflowDetails = WorkflowDetails'
  { -- | A trigger that starts a workflow: the workflow begins to execute after a
    -- file is uploaded.
    --
    -- To remove an associated workflow from a server, you can provide an empty
    -- @OnUpload@ object, as in the following example.
    --
    -- @aws transfer update-server --server-id s-01234567890abcdef --workflow-details \'{\"OnUpload\":[]}\'@
    onUpload :: [WorkflowDetail]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onUpload', 'workflowDetails_onUpload' - A trigger that starts a workflow: the workflow begins to execute after a
-- file is uploaded.
--
-- To remove an associated workflow from a server, you can provide an empty
-- @OnUpload@ object, as in the following example.
--
-- @aws transfer update-server --server-id s-01234567890abcdef --workflow-details \'{\"OnUpload\":[]}\'@
newWorkflowDetails ::
  WorkflowDetails
newWorkflowDetails =
  WorkflowDetails' {onUpload = Prelude.mempty}

-- | A trigger that starts a workflow: the workflow begins to execute after a
-- file is uploaded.
--
-- To remove an associated workflow from a server, you can provide an empty
-- @OnUpload@ object, as in the following example.
--
-- @aws transfer update-server --server-id s-01234567890abcdef --workflow-details \'{\"OnUpload\":[]}\'@
workflowDetails_onUpload :: Lens.Lens' WorkflowDetails [WorkflowDetail]
workflowDetails_onUpload = Lens.lens (\WorkflowDetails' {onUpload} -> onUpload) (\s@WorkflowDetails' {} a -> s {onUpload = a} :: WorkflowDetails) Prelude.. Lens.coerced

instance Core.FromJSON WorkflowDetails where
  parseJSON =
    Core.withObject
      "WorkflowDetails"
      ( \x ->
          WorkflowDetails'
            Prelude.<$> (x Core..:? "OnUpload" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable WorkflowDetails where
  hashWithSalt _salt WorkflowDetails' {..} =
    _salt `Prelude.hashWithSalt` onUpload

instance Prelude.NFData WorkflowDetails where
  rnf WorkflowDetails' {..} = Prelude.rnf onUpload

instance Core.ToJSON WorkflowDetails where
  toJSON WorkflowDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("OnUpload" Core..= onUpload)]
      )
