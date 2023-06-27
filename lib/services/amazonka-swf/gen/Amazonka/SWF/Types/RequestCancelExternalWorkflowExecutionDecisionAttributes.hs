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
-- Module      : Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionDecisionAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @RequestCancelExternalWorkflowExecution@
-- decision.
--
-- __Access Control__
--
-- You can use IAM policies to control this decision\'s access to Amazon
-- SWF resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
--
-- /See:/ 'newRequestCancelExternalWorkflowExecutionDecisionAttributes' smart constructor.
data RequestCancelExternalWorkflowExecutionDecisionAttributes = RequestCancelExternalWorkflowExecutionDecisionAttributes'
  { -- | The data attached to the event that can be used by the decider in
    -- subsequent workflow tasks.
    control :: Prelude.Maybe Prelude.Text,
    -- | The @runId@ of the external workflow execution to cancel.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The @workflowId@ of the external workflow execution to cancel.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RequestCancelExternalWorkflowExecutionDecisionAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'control', 'requestCancelExternalWorkflowExecutionDecisionAttributes_control' - The data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
--
-- 'runId', 'requestCancelExternalWorkflowExecutionDecisionAttributes_runId' - The @runId@ of the external workflow execution to cancel.
--
-- 'workflowId', 'requestCancelExternalWorkflowExecutionDecisionAttributes_workflowId' - The @workflowId@ of the external workflow execution to cancel.
newRequestCancelExternalWorkflowExecutionDecisionAttributes ::
  -- | 'workflowId'
  Prelude.Text ->
  RequestCancelExternalWorkflowExecutionDecisionAttributes
newRequestCancelExternalWorkflowExecutionDecisionAttributes
  pWorkflowId_ =
    RequestCancelExternalWorkflowExecutionDecisionAttributes'
      { control =
          Prelude.Nothing,
        runId =
          Prelude.Nothing,
        workflowId =
          pWorkflowId_
      }

-- | The data attached to the event that can be used by the decider in
-- subsequent workflow tasks.
requestCancelExternalWorkflowExecutionDecisionAttributes_control :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
requestCancelExternalWorkflowExecutionDecisionAttributes_control = Lens.lens (\RequestCancelExternalWorkflowExecutionDecisionAttributes' {control} -> control) (\s@RequestCancelExternalWorkflowExecutionDecisionAttributes' {} a -> s {control = a} :: RequestCancelExternalWorkflowExecutionDecisionAttributes)

-- | The @runId@ of the external workflow execution to cancel.
requestCancelExternalWorkflowExecutionDecisionAttributes_runId :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes (Prelude.Maybe Prelude.Text)
requestCancelExternalWorkflowExecutionDecisionAttributes_runId = Lens.lens (\RequestCancelExternalWorkflowExecutionDecisionAttributes' {runId} -> runId) (\s@RequestCancelExternalWorkflowExecutionDecisionAttributes' {} a -> s {runId = a} :: RequestCancelExternalWorkflowExecutionDecisionAttributes)

-- | The @workflowId@ of the external workflow execution to cancel.
requestCancelExternalWorkflowExecutionDecisionAttributes_workflowId :: Lens.Lens' RequestCancelExternalWorkflowExecutionDecisionAttributes Prelude.Text
requestCancelExternalWorkflowExecutionDecisionAttributes_workflowId = Lens.lens (\RequestCancelExternalWorkflowExecutionDecisionAttributes' {workflowId} -> workflowId) (\s@RequestCancelExternalWorkflowExecutionDecisionAttributes' {} a -> s {workflowId = a} :: RequestCancelExternalWorkflowExecutionDecisionAttributes)

instance
  Prelude.Hashable
    RequestCancelExternalWorkflowExecutionDecisionAttributes
  where
  hashWithSalt
    _salt
    RequestCancelExternalWorkflowExecutionDecisionAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` control
        `Prelude.hashWithSalt` runId
        `Prelude.hashWithSalt` workflowId

instance
  Prelude.NFData
    RequestCancelExternalWorkflowExecutionDecisionAttributes
  where
  rnf
    RequestCancelExternalWorkflowExecutionDecisionAttributes' {..} =
      Prelude.rnf control
        `Prelude.seq` Prelude.rnf runId
        `Prelude.seq` Prelude.rnf workflowId

instance
  Data.ToJSON
    RequestCancelExternalWorkflowExecutionDecisionAttributes
  where
  toJSON
    RequestCancelExternalWorkflowExecutionDecisionAttributes' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("control" Data..=) Prelude.<$> control,
              ("runId" Data..=) Prelude.<$> runId,
              Prelude.Just ("workflowId" Data..= workflowId)
            ]
        )
