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
-- Module      : Amazonka.CodePipeline.Types.ActionExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecution where

import Amazonka.CodePipeline.Types.ActionExecutionStatus
import Amazonka.CodePipeline.Types.ErrorDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the run of an action.
--
-- /See:/ 'newActionExecution' smart constructor.
data ActionExecution = ActionExecution'
  { -- | The details of an error returned by a URL external to AWS.
    errorDetails :: Prelude.Maybe ErrorDetails,
    -- | ID of the workflow action execution in the current stage. Use the
    -- GetPipelineState action to retrieve the current action execution details
    -- of the current stage.
    --
    -- For older executions, this field might be empty. The action execution ID
    -- is available for executions run on or after March 2020.
    actionExecutionId :: Prelude.Maybe Prelude.Text,
    -- | A summary of the run of the action.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The status of the action, or for a completed action, the last status of
    -- the action.
    status :: Prelude.Maybe ActionExecutionStatus,
    -- | The last status change of the action.
    lastStatusChange :: Prelude.Maybe Core.POSIX,
    -- | The external ID of the run of the action.
    externalExecutionId :: Prelude.Maybe Prelude.Text,
    -- | A percentage of completeness of the action as it runs.
    percentComplete :: Prelude.Maybe Prelude.Natural,
    -- | The URL of a resource external to AWS that is used when running the
    -- action (for example, an external repository URL).
    externalExecutionUrl :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user who last changed the pipeline.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | The system-generated token used to identify a unique approval request.
    -- The token for each open approval request can be obtained using the
    -- @GetPipelineState@ command. It is used to validate that the approval
    -- request corresponding to this token is still valid.
    token :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDetails', 'actionExecution_errorDetails' - The details of an error returned by a URL external to AWS.
--
-- 'actionExecutionId', 'actionExecution_actionExecutionId' - ID of the workflow action execution in the current stage. Use the
-- GetPipelineState action to retrieve the current action execution details
-- of the current stage.
--
-- For older executions, this field might be empty. The action execution ID
-- is available for executions run on or after March 2020.
--
-- 'summary', 'actionExecution_summary' - A summary of the run of the action.
--
-- 'status', 'actionExecution_status' - The status of the action, or for a completed action, the last status of
-- the action.
--
-- 'lastStatusChange', 'actionExecution_lastStatusChange' - The last status change of the action.
--
-- 'externalExecutionId', 'actionExecution_externalExecutionId' - The external ID of the run of the action.
--
-- 'percentComplete', 'actionExecution_percentComplete' - A percentage of completeness of the action as it runs.
--
-- 'externalExecutionUrl', 'actionExecution_externalExecutionUrl' - The URL of a resource external to AWS that is used when running the
-- action (for example, an external repository URL).
--
-- 'lastUpdatedBy', 'actionExecution_lastUpdatedBy' - The ARN of the user who last changed the pipeline.
--
-- 'token', 'actionExecution_token' - The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- @GetPipelineState@ command. It is used to validate that the approval
-- request corresponding to this token is still valid.
newActionExecution ::
  ActionExecution
newActionExecution =
  ActionExecution'
    { errorDetails = Prelude.Nothing,
      actionExecutionId = Prelude.Nothing,
      summary = Prelude.Nothing,
      status = Prelude.Nothing,
      lastStatusChange = Prelude.Nothing,
      externalExecutionId = Prelude.Nothing,
      percentComplete = Prelude.Nothing,
      externalExecutionUrl = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing,
      token = Prelude.Nothing
    }

-- | The details of an error returned by a URL external to AWS.
actionExecution_errorDetails :: Lens.Lens' ActionExecution (Prelude.Maybe ErrorDetails)
actionExecution_errorDetails = Lens.lens (\ActionExecution' {errorDetails} -> errorDetails) (\s@ActionExecution' {} a -> s {errorDetails = a} :: ActionExecution)

-- | ID of the workflow action execution in the current stage. Use the
-- GetPipelineState action to retrieve the current action execution details
-- of the current stage.
--
-- For older executions, this field might be empty. The action execution ID
-- is available for executions run on or after March 2020.
actionExecution_actionExecutionId :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_actionExecutionId = Lens.lens (\ActionExecution' {actionExecutionId} -> actionExecutionId) (\s@ActionExecution' {} a -> s {actionExecutionId = a} :: ActionExecution)

-- | A summary of the run of the action.
actionExecution_summary :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_summary = Lens.lens (\ActionExecution' {summary} -> summary) (\s@ActionExecution' {} a -> s {summary = a} :: ActionExecution)

-- | The status of the action, or for a completed action, the last status of
-- the action.
actionExecution_status :: Lens.Lens' ActionExecution (Prelude.Maybe ActionExecutionStatus)
actionExecution_status = Lens.lens (\ActionExecution' {status} -> status) (\s@ActionExecution' {} a -> s {status = a} :: ActionExecution)

-- | The last status change of the action.
actionExecution_lastStatusChange :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.UTCTime)
actionExecution_lastStatusChange = Lens.lens (\ActionExecution' {lastStatusChange} -> lastStatusChange) (\s@ActionExecution' {} a -> s {lastStatusChange = a} :: ActionExecution) Prelude.. Lens.mapping Core._Time

-- | The external ID of the run of the action.
actionExecution_externalExecutionId :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_externalExecutionId = Lens.lens (\ActionExecution' {externalExecutionId} -> externalExecutionId) (\s@ActionExecution' {} a -> s {externalExecutionId = a} :: ActionExecution)

-- | A percentage of completeness of the action as it runs.
actionExecution_percentComplete :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Natural)
actionExecution_percentComplete = Lens.lens (\ActionExecution' {percentComplete} -> percentComplete) (\s@ActionExecution' {} a -> s {percentComplete = a} :: ActionExecution)

-- | The URL of a resource external to AWS that is used when running the
-- action (for example, an external repository URL).
actionExecution_externalExecutionUrl :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_externalExecutionUrl = Lens.lens (\ActionExecution' {externalExecutionUrl} -> externalExecutionUrl) (\s@ActionExecution' {} a -> s {externalExecutionUrl = a} :: ActionExecution)

-- | The ARN of the user who last changed the pipeline.
actionExecution_lastUpdatedBy :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_lastUpdatedBy = Lens.lens (\ActionExecution' {lastUpdatedBy} -> lastUpdatedBy) (\s@ActionExecution' {} a -> s {lastUpdatedBy = a} :: ActionExecution)

-- | The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- @GetPipelineState@ command. It is used to validate that the approval
-- request corresponding to this token is still valid.
actionExecution_token :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_token = Lens.lens (\ActionExecution' {token} -> token) (\s@ActionExecution' {} a -> s {token = a} :: ActionExecution)

instance Core.FromJSON ActionExecution where
  parseJSON =
    Core.withObject
      "ActionExecution"
      ( \x ->
          ActionExecution'
            Prelude.<$> (x Core..:? "errorDetails")
            Prelude.<*> (x Core..:? "actionExecutionId")
            Prelude.<*> (x Core..:? "summary")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastStatusChange")
            Prelude.<*> (x Core..:? "externalExecutionId")
            Prelude.<*> (x Core..:? "percentComplete")
            Prelude.<*> (x Core..:? "externalExecutionUrl")
            Prelude.<*> (x Core..:? "lastUpdatedBy")
            Prelude.<*> (x Core..:? "token")
      )

instance Prelude.Hashable ActionExecution where
  hashWithSalt _salt ActionExecution' {..} =
    _salt `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` actionExecutionId
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastStatusChange
      `Prelude.hashWithSalt` externalExecutionId
      `Prelude.hashWithSalt` percentComplete
      `Prelude.hashWithSalt` externalExecutionUrl
      `Prelude.hashWithSalt` lastUpdatedBy
      `Prelude.hashWithSalt` token

instance Prelude.NFData ActionExecution where
  rnf ActionExecution' {..} =
    Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf actionExecutionId
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastStatusChange
      `Prelude.seq` Prelude.rnf externalExecutionId
      `Prelude.seq` Prelude.rnf percentComplete
      `Prelude.seq` Prelude.rnf externalExecutionUrl
      `Prelude.seq` Prelude.rnf lastUpdatedBy
      `Prelude.seq` Prelude.rnf token
