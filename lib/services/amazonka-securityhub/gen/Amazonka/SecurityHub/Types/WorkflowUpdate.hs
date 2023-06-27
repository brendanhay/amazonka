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
-- Module      : Amazonka.SecurityHub.Types.WorkflowUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.WorkflowUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.WorkflowStatus

-- | Used to update information about the investigation into the finding.
--
-- /See:/ 'newWorkflowUpdate' smart constructor.
data WorkflowUpdate = WorkflowUpdate'
  { -- | The status of the investigation into the finding. The workflow status is
    -- specific to an individual finding. It does not affect the generation of
    -- new findings. For example, setting the workflow status to @SUPPRESSED@
    -- or @RESOLVED@ does not prevent a new finding for the same issue.
    --
    -- The allowed values are the following.
    --
    -- -   @NEW@ - The initial state of a finding, before it is reviewed.
    --
    --     Security Hub also resets @WorkFlowStatus@ from @NOTIFIED@ or
    --     @RESOLVED@ to @NEW@ in the following cases:
    --
    --     -   The record state changes from @ARCHIVED@ to @ACTIVE@.
    --
    --     -   The compliance status changes from @PASSED@ to either @WARNING@,
    --         @FAILED@, or @NOT_AVAILABLE@.
    --
    -- -   @NOTIFIED@ - Indicates that you notified the resource owner about
    --     the security issue. Used when the initial reviewer is not the
    --     resource owner, and needs intervention from the resource owner.
    --
    -- -   @RESOLVED@ - The finding was reviewed and remediated and is now
    --     considered resolved.
    --
    -- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
    --     believe that any action is needed. The finding is no longer updated.
    status :: Prelude.Maybe WorkflowStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'workflowUpdate_status' - The status of the investigation into the finding. The workflow status is
-- specific to an individual finding. It does not affect the generation of
-- new findings. For example, setting the workflow status to @SUPPRESSED@
-- or @RESOLVED@ does not prevent a new finding for the same issue.
--
-- The allowed values are the following.
--
-- -   @NEW@ - The initial state of a finding, before it is reviewed.
--
--     Security Hub also resets @WorkFlowStatus@ from @NOTIFIED@ or
--     @RESOLVED@ to @NEW@ in the following cases:
--
--     -   The record state changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   The compliance status changes from @PASSED@ to either @WARNING@,
--         @FAILED@, or @NOT_AVAILABLE@.
--
-- -   @NOTIFIED@ - Indicates that you notified the resource owner about
--     the security issue. Used when the initial reviewer is not the
--     resource owner, and needs intervention from the resource owner.
--
-- -   @RESOLVED@ - The finding was reviewed and remediated and is now
--     considered resolved.
--
-- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
--     believe that any action is needed. The finding is no longer updated.
newWorkflowUpdate ::
  WorkflowUpdate
newWorkflowUpdate =
  WorkflowUpdate' {status = Prelude.Nothing}

-- | The status of the investigation into the finding. The workflow status is
-- specific to an individual finding. It does not affect the generation of
-- new findings. For example, setting the workflow status to @SUPPRESSED@
-- or @RESOLVED@ does not prevent a new finding for the same issue.
--
-- The allowed values are the following.
--
-- -   @NEW@ - The initial state of a finding, before it is reviewed.
--
--     Security Hub also resets @WorkFlowStatus@ from @NOTIFIED@ or
--     @RESOLVED@ to @NEW@ in the following cases:
--
--     -   The record state changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   The compliance status changes from @PASSED@ to either @WARNING@,
--         @FAILED@, or @NOT_AVAILABLE@.
--
-- -   @NOTIFIED@ - Indicates that you notified the resource owner about
--     the security issue. Used when the initial reviewer is not the
--     resource owner, and needs intervention from the resource owner.
--
-- -   @RESOLVED@ - The finding was reviewed and remediated and is now
--     considered resolved.
--
-- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
--     believe that any action is needed. The finding is no longer updated.
workflowUpdate_status :: Lens.Lens' WorkflowUpdate (Prelude.Maybe WorkflowStatus)
workflowUpdate_status = Lens.lens (\WorkflowUpdate' {status} -> status) (\s@WorkflowUpdate' {} a -> s {status = a} :: WorkflowUpdate)

instance Data.FromJSON WorkflowUpdate where
  parseJSON =
    Data.withObject
      "WorkflowUpdate"
      ( \x ->
          WorkflowUpdate' Prelude.<$> (x Data..:? "Status")
      )

instance Prelude.Hashable WorkflowUpdate where
  hashWithSalt _salt WorkflowUpdate' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData WorkflowUpdate where
  rnf WorkflowUpdate' {..} = Prelude.rnf status

instance Data.ToJSON WorkflowUpdate where
  toJSON WorkflowUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Status" Data..=) Prelude.<$> status]
      )
