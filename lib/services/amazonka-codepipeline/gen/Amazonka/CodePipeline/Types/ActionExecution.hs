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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecution where

import Amazonka.CodePipeline.Types.ActionExecutionStatus
import Amazonka.CodePipeline.Types.ErrorDetails
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the run of an action.
--
-- /See:/ 'newActionExecution' smart constructor.
data ActionExecution = ActionExecution'
  { -- | ID of the workflow action execution in the current stage. Use the
    -- GetPipelineState action to retrieve the current action execution details
    -- of the current stage.
    --
    -- For older executions, this field might be empty. The action execution ID
    -- is available for executions run on or after March 2020.
    actionExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The details of an error returned by a URL external to AWS.
    errorDetails :: Prelude.Maybe ErrorDetails,
    -- | The external ID of the run of the action.
    externalExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The URL of a resource external to AWS that is used when running the
    -- action (for example, an external repository URL).
    externalExecutionUrl :: Prelude.Maybe Prelude.Text,
    -- | The last status change of the action.
    lastStatusChange :: Prelude.Maybe Data.POSIX,
    -- | The ARN of the user who last changed the pipeline.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | A percentage of completeness of the action as it runs.
    percentComplete :: Prelude.Maybe Prelude.Natural,
    -- | The status of the action, or for a completed action, the last status of
    -- the action.
    status :: Prelude.Maybe ActionExecutionStatus,
    -- | A summary of the run of the action.
    summary :: Prelude.Maybe Prelude.Text,
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
-- 'actionExecutionId', 'actionExecution_actionExecutionId' - ID of the workflow action execution in the current stage. Use the
-- GetPipelineState action to retrieve the current action execution details
-- of the current stage.
--
-- For older executions, this field might be empty. The action execution ID
-- is available for executions run on or after March 2020.
--
-- 'errorDetails', 'actionExecution_errorDetails' - The details of an error returned by a URL external to AWS.
--
-- 'externalExecutionId', 'actionExecution_externalExecutionId' - The external ID of the run of the action.
--
-- 'externalExecutionUrl', 'actionExecution_externalExecutionUrl' - The URL of a resource external to AWS that is used when running the
-- action (for example, an external repository URL).
--
-- 'lastStatusChange', 'actionExecution_lastStatusChange' - The last status change of the action.
--
-- 'lastUpdatedBy', 'actionExecution_lastUpdatedBy' - The ARN of the user who last changed the pipeline.
--
-- 'percentComplete', 'actionExecution_percentComplete' - A percentage of completeness of the action as it runs.
--
-- 'status', 'actionExecution_status' - The status of the action, or for a completed action, the last status of
-- the action.
--
-- 'summary', 'actionExecution_summary' - A summary of the run of the action.
--
-- 'token', 'actionExecution_token' - The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- @GetPipelineState@ command. It is used to validate that the approval
-- request corresponding to this token is still valid.
newActionExecution ::
  ActionExecution
newActionExecution =
  ActionExecution'
    { actionExecutionId =
        Prelude.Nothing,
      errorDetails = Prelude.Nothing,
      externalExecutionId = Prelude.Nothing,
      externalExecutionUrl = Prelude.Nothing,
      lastStatusChange = Prelude.Nothing,
      lastUpdatedBy = Prelude.Nothing,
      percentComplete = Prelude.Nothing,
      status = Prelude.Nothing,
      summary = Prelude.Nothing,
      token = Prelude.Nothing
    }

-- | ID of the workflow action execution in the current stage. Use the
-- GetPipelineState action to retrieve the current action execution details
-- of the current stage.
--
-- For older executions, this field might be empty. The action execution ID
-- is available for executions run on or after March 2020.
actionExecution_actionExecutionId :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_actionExecutionId = Lens.lens (\ActionExecution' {actionExecutionId} -> actionExecutionId) (\s@ActionExecution' {} a -> s {actionExecutionId = a} :: ActionExecution)

-- | The details of an error returned by a URL external to AWS.
actionExecution_errorDetails :: Lens.Lens' ActionExecution (Prelude.Maybe ErrorDetails)
actionExecution_errorDetails = Lens.lens (\ActionExecution' {errorDetails} -> errorDetails) (\s@ActionExecution' {} a -> s {errorDetails = a} :: ActionExecution)

-- | The external ID of the run of the action.
actionExecution_externalExecutionId :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_externalExecutionId = Lens.lens (\ActionExecution' {externalExecutionId} -> externalExecutionId) (\s@ActionExecution' {} a -> s {externalExecutionId = a} :: ActionExecution)

-- | The URL of a resource external to AWS that is used when running the
-- action (for example, an external repository URL).
actionExecution_externalExecutionUrl :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_externalExecutionUrl = Lens.lens (\ActionExecution' {externalExecutionUrl} -> externalExecutionUrl) (\s@ActionExecution' {} a -> s {externalExecutionUrl = a} :: ActionExecution)

-- | The last status change of the action.
actionExecution_lastStatusChange :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.UTCTime)
actionExecution_lastStatusChange = Lens.lens (\ActionExecution' {lastStatusChange} -> lastStatusChange) (\s@ActionExecution' {} a -> s {lastStatusChange = a} :: ActionExecution) Prelude.. Lens.mapping Data._Time

-- | The ARN of the user who last changed the pipeline.
actionExecution_lastUpdatedBy :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_lastUpdatedBy = Lens.lens (\ActionExecution' {lastUpdatedBy} -> lastUpdatedBy) (\s@ActionExecution' {} a -> s {lastUpdatedBy = a} :: ActionExecution)

-- | A percentage of completeness of the action as it runs.
actionExecution_percentComplete :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Natural)
actionExecution_percentComplete = Lens.lens (\ActionExecution' {percentComplete} -> percentComplete) (\s@ActionExecution' {} a -> s {percentComplete = a} :: ActionExecution)

-- | The status of the action, or for a completed action, the last status of
-- the action.
actionExecution_status :: Lens.Lens' ActionExecution (Prelude.Maybe ActionExecutionStatus)
actionExecution_status = Lens.lens (\ActionExecution' {status} -> status) (\s@ActionExecution' {} a -> s {status = a} :: ActionExecution)

-- | A summary of the run of the action.
actionExecution_summary :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_summary = Lens.lens (\ActionExecution' {summary} -> summary) (\s@ActionExecution' {} a -> s {summary = a} :: ActionExecution)

-- | The system-generated token used to identify a unique approval request.
-- The token for each open approval request can be obtained using the
-- @GetPipelineState@ command. It is used to validate that the approval
-- request corresponding to this token is still valid.
actionExecution_token :: Lens.Lens' ActionExecution (Prelude.Maybe Prelude.Text)
actionExecution_token = Lens.lens (\ActionExecution' {token} -> token) (\s@ActionExecution' {} a -> s {token = a} :: ActionExecution)

instance Data.FromJSON ActionExecution where
  parseJSON =
    Data.withObject
      "ActionExecution"
      ( \x ->
          ActionExecution'
            Prelude.<$> (x Data..:? "actionExecutionId")
            Prelude.<*> (x Data..:? "errorDetails")
            Prelude.<*> (x Data..:? "externalExecutionId")
            Prelude.<*> (x Data..:? "externalExecutionUrl")
            Prelude.<*> (x Data..:? "lastStatusChange")
            Prelude.<*> (x Data..:? "lastUpdatedBy")
            Prelude.<*> (x Data..:? "percentComplete")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "summary")
            Prelude.<*> (x Data..:? "token")
      )

instance Prelude.Hashable ActionExecution where
  hashWithSalt _salt ActionExecution' {..} =
    _salt
      `Prelude.hashWithSalt` actionExecutionId
      `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` externalExecutionId
      `Prelude.hashWithSalt` externalExecutionUrl
      `Prelude.hashWithSalt` lastStatusChange
      `Prelude.hashWithSalt` lastUpdatedBy
      `Prelude.hashWithSalt` percentComplete
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` summary
      `Prelude.hashWithSalt` token

instance Prelude.NFData ActionExecution where
  rnf ActionExecution' {..} =
    Prelude.rnf actionExecutionId
      `Prelude.seq` Prelude.rnf errorDetails
      `Prelude.seq` Prelude.rnf externalExecutionId
      `Prelude.seq` Prelude.rnf externalExecutionUrl
      `Prelude.seq` Prelude.rnf lastStatusChange
      `Prelude.seq` Prelude.rnf lastUpdatedBy
      `Prelude.seq` Prelude.rnf percentComplete
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf summary
      `Prelude.seq` Prelude.rnf token
