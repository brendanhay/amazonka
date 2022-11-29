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
-- Module      : Amazonka.SecurityHub.Types.Workflow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Workflow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.WorkflowStatus

-- | Provides information about the status of the investigation into a
-- finding.
--
-- /See:/ 'newWorkflow' smart constructor.
data Workflow = Workflow'
  { -- | The status of the investigation into the finding. The workflow status is
    -- specific to an individual finding. It does not affect the generation of
    -- new findings. For example, setting the workflow status to @SUPPRESSED@
    -- or @RESOLVED@ does not prevent a new finding for the same issue.
    --
    -- The allowed values are the following.
    --
    -- -   @NEW@ - The initial state of a finding, before it is reviewed.
    --
    --     Security Hub also resets the workflow status from @NOTIFIED@ or
    --     @RESOLVED@ to @NEW@ in the following cases:
    --
    --     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
    --
    --     -   @ComplianceStatus@ changes from @PASSED@ to either @WARNING@,
    --         @FAILED@, or @NOT_AVAILABLE@.
    --
    -- -   @NOTIFIED@ - Indicates that you notified the resource owner about
    --     the security issue. Used when the initial reviewer is not the
    --     resource owner, and needs intervention from the resource owner.
    --
    -- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
    --     believe that any action is needed. The finding is no longer updated.
    --
    -- -   @RESOLVED@ - The finding was reviewed and remediated and is now
    --     considered resolved.
    status :: Prelude.Maybe WorkflowStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Workflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'workflow_status' - The status of the investigation into the finding. The workflow status is
-- specific to an individual finding. It does not affect the generation of
-- new findings. For example, setting the workflow status to @SUPPRESSED@
-- or @RESOLVED@ does not prevent a new finding for the same issue.
--
-- The allowed values are the following.
--
-- -   @NEW@ - The initial state of a finding, before it is reviewed.
--
--     Security Hub also resets the workflow status from @NOTIFIED@ or
--     @RESOLVED@ to @NEW@ in the following cases:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @ComplianceStatus@ changes from @PASSED@ to either @WARNING@,
--         @FAILED@, or @NOT_AVAILABLE@.
--
-- -   @NOTIFIED@ - Indicates that you notified the resource owner about
--     the security issue. Used when the initial reviewer is not the
--     resource owner, and needs intervention from the resource owner.
--
-- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
--     believe that any action is needed. The finding is no longer updated.
--
-- -   @RESOLVED@ - The finding was reviewed and remediated and is now
--     considered resolved.
newWorkflow ::
  Workflow
newWorkflow = Workflow' {status = Prelude.Nothing}

-- | The status of the investigation into the finding. The workflow status is
-- specific to an individual finding. It does not affect the generation of
-- new findings. For example, setting the workflow status to @SUPPRESSED@
-- or @RESOLVED@ does not prevent a new finding for the same issue.
--
-- The allowed values are the following.
--
-- -   @NEW@ - The initial state of a finding, before it is reviewed.
--
--     Security Hub also resets the workflow status from @NOTIFIED@ or
--     @RESOLVED@ to @NEW@ in the following cases:
--
--     -   @RecordState@ changes from @ARCHIVED@ to @ACTIVE@.
--
--     -   @ComplianceStatus@ changes from @PASSED@ to either @WARNING@,
--         @FAILED@, or @NOT_AVAILABLE@.
--
-- -   @NOTIFIED@ - Indicates that you notified the resource owner about
--     the security issue. Used when the initial reviewer is not the
--     resource owner, and needs intervention from the resource owner.
--
-- -   @SUPPRESSED@ - Indicates that you reviewed the finding and do not
--     believe that any action is needed. The finding is no longer updated.
--
-- -   @RESOLVED@ - The finding was reviewed and remediated and is now
--     considered resolved.
workflow_status :: Lens.Lens' Workflow (Prelude.Maybe WorkflowStatus)
workflow_status = Lens.lens (\Workflow' {status} -> status) (\s@Workflow' {} a -> s {status = a} :: Workflow)

instance Core.FromJSON Workflow where
  parseJSON =
    Core.withObject
      "Workflow"
      (\x -> Workflow' Prelude.<$> (x Core..:? "Status"))

instance Prelude.Hashable Workflow where
  hashWithSalt _salt Workflow' {..} =
    _salt `Prelude.hashWithSalt` status

instance Prelude.NFData Workflow where
  rnf Workflow' {..} = Prelude.rnf status

instance Core.ToJSON Workflow where
  toJSON Workflow' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Status" Core..=) Prelude.<$> status]
      )
