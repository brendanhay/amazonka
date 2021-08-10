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
-- Module      : Network.AWS.CodePipeline.Types.ActionExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecution where

import Network.AWS.CodePipeline.Types.ActionExecutionStatus
import Network.AWS.CodePipeline.Types.ErrorDetails
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about the run of an action.
--
-- /See:/ 'newActionExecution' smart constructor.
data ActionExecution = ActionExecution'
  { -- | The status of the action, or for a completed action, the last status of
    -- the action.
    status :: Prelude.Maybe ActionExecutionStatus,
    -- | ID of the workflow action execution in the current stage. Use the
    -- GetPipelineState action to retrieve the current action execution details
    -- of the current stage.
    --
    -- For older executions, this field might be empty. The action execution ID
    -- is available for executions run on or after March 2020.
    actionExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The last status change of the action.
    lastStatusChange :: Prelude.Maybe Core.POSIX,
    -- | A percentage of completeness of the action as it runs.
    percentComplete :: Prelude.Maybe Prelude.Natural,
    -- | The external ID of the run of the action.
    externalExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The URL of a resource external to AWS that is used when running the
    -- action (for example, an external repository URL).
    externalExecutionUrl :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the user who last changed the pipeline.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | A summary of the run of the action.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The system-generated token used to identify a unique approval request.
    -- The token for each open approval request can be obtained using the
    -- @GetPipelineState@ command. It is used to validate that the approval
    -- request corresponding to this token is still valid.
    token :: Prelude.Maybe Prelude.Text,
    -- | The details of an error returned by a URL external to AWS.
    errorDetails :: Prelude.Maybe ErrorDetails
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
-- 'status', 'actionExecution_status' - The status of the action, or for a completed action, the last status of
-- the action.
--
-- 'actionExecutionId', 'actionExecution_actionExecutionId' - ID of the workflow action execution in the current stage. Use the
-- GetPipelineState action to retrieve the current action execution details
-- of the current stage.
--
-- For older executions, this field might be empty. The action execution ID
-- is available for executions run on or after March 2020.
--
-- 'lastStatusChange', 'actionExecution_lastStatusChange' - The last status change of the action.
--
-- 'percentComplete', 'actionExecution_percentComplete' - A percentage of completeness of the action as it runs.
--
-- 'externalExecutionId', 'actionExecution_externalExecutionId' - The external ID of the run of the action.
--
-- 'externalExecutionUrl', 'actionExecution_externalExecutionUrl' - The URL of a resource external to AWS that is used when running the
-- action (for example, an external repository URL).
--
-- 'lastUpdatedBy', 'actionExecution_lastUpdatedBy' - The ARN of the user who last changed the pipeline.
--
-- 'summary', 'actionExecution_summary' - A summary of the run of the action.
--
-- 'token', 'actionExecution_token' - The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- @GetPipelineState@ command. It is used to validate that the approval
-- request corresponding to this token is still valid.
--
-- 'errorDetails', 'actionExecution_errorDetails' - The details of an error returned by a URL external to AWS.
newActionExecution ::
  ActionExecution
newActionExecution =
  ActionExecution'
    { status = Prelude.Nothing,
      actionExecutionId = Prelude.Nothing,
      lastStatusChange = Prelude.Nothing,
      percentComplete = Prelude.Nothing,
      externalExecutionId = Prelude.Nothing,
      externalExecutionUrl = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing,
      summary = Prelude.Nothing,
      token = Prelude.Nothing,
      errorDetails = Prelude.Nothing
    }

-- | The status of the action, or for a completed action, the last status of
-- the action.
actionExecution_status :: Lens.Lens' ActionExecution (Prelude.Maybe ActionExecutionStatus)
actionExecution_status = Lens.lens (\ActionExecution' {status} -> status) (\s@ActionExecution' {} a -> s {status = a} :: ActionExecution)

-- | ID of the workflow action execution in the current stage. Use the
-- GetPipelineState action to retrieve the current action execution details
-- of the current stage.
--
-- For older executions, this field might be empty. The action execution ID
-- is available for executions run on or after March 2020.
actionExecution_actionExecutionId :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_actionExecutionId = Lens.lens (\ActionExecution' {actionExecutionId} -> actionExecutionId) (\s@ActionExecution' {} a -> s {actionExecutionId = a} :: ActionExecution)

-- | The last status change of the action.
actionExecution_lastStatusChange :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.UTCTime)
actionExecution_lastStatusChange = Lens.lens (\ActionExecution' {lastStatusChange} -> lastStatusChange) (\s@ActionExecution' {} a -> s {lastStatusChange = a} :: ActionExecution) Prelude.. Lens.mapping Core._Time

-- | A percentage of completeness of the action as it runs.
actionExecution_percentComplete :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Natural)
actionExecution_percentComplete = Lens.lens (\ActionExecution' {percentComplete} -> percentComplete) (\s@ActionExecution' {} a -> s {percentComplete = a} :: ActionExecution)

-- | The external ID of the run of the action.
actionExecution_externalExecutionId :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_externalExecutionId = Lens.lens (\ActionExecution' {externalExecutionId} -> externalExecutionId) (\s@ActionExecution' {} a -> s {externalExecutionId = a} :: ActionExecution)

-- | The URL of a resource external to AWS that is used when running the
-- action (for example, an external repository URL).
actionExecution_externalExecutionUrl :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_externalExecutionUrl = Lens.lens (\ActionExecution' {externalExecutionUrl} -> externalExecutionUrl) (\s@ActionExecution' {} a -> s {externalExecutionUrl = a} :: ActionExecution)

-- | The ARN of the user who last changed the pipeline.
actionExecution_lastUpdatedBy :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_lastUpdatedBy = Lens.lens (\ActionExecution' {lastUpdatedBy} -> lastUpdatedBy) (\s@ActionExecution' {} a -> s {lastUpdatedBy = a} :: ActionExecution)

-- | A summary of the run of the action.
actionExecution_summary :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_summary = Lens.lens (\ActionExecution' {summary} -> summary) (\s@ActionExecution' {} a -> s {summary = a} :: ActionExecution)

-- | The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- @GetPipelineState@ command. It is used to validate that the approval
-- request corresponding to this token is still valid.
actionExecution_token :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_token = Lens.lens (\ActionExecution' {token} -> token) (\s@ActionExecution' {} a -> s {token = a} :: ActionExecution)

-- | The details of an error returned by a URL external to AWS.
actionExecution_errorDetails :: Lens.Lens' ActionExecution (Prelude.Maybe ErrorDetails)
actionExecution_errorDetails = Lens.lens (\ActionExecution' {errorDetails} -> errorDetails) (\s@ActionExecution' {} a -> s {errorDetails = a} :: ActionExecution)

instance Core.FromJSON ActionExecution where
  parseJSON =
    Core.withObject
      "ActionExecution"
      ( \x ->
          ActionExecution'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "actionExecutionId")
            Prelude.<*> (x Core..:? "lastStatusChange")
            Prelude.<*> (x Core..:? "percentComplete")
            Prelude.<*> (x Core..:? "externalExecutionId")
            Prelude.<*> (x Core..:? "externalExecutionUrl")
            Prelude.<*> (x Core..:? "lastUpdatedBy")
            Prelude.<*> (x Core..:? "summary")
            Prelude.<*> (x Core..:? "token")
            Prelude.<*> (x Core..:? "errorDetails")
      )

instance Prelude.Hashable ActionExecution

instance Prelude.NFData ActionExecution
