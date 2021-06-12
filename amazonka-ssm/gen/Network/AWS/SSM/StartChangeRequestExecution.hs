{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartChangeRequestExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a change request for Change Manager. The runbooks (Automation
-- documents) specified in the change request run only after all required
-- approvals for the change request have been received.
module Network.AWS.SSM.StartChangeRequestExecution
  ( -- * Creating a Request
    StartChangeRequestExecution (..),
    newStartChangeRequestExecution,

    -- * Request Lenses
    startChangeRequestExecution_changeRequestName,
    startChangeRequestExecution_scheduledTime,
    startChangeRequestExecution_tags,
    startChangeRequestExecution_documentVersion,
    startChangeRequestExecution_parameters,
    startChangeRequestExecution_clientToken,
    startChangeRequestExecution_documentName,
    startChangeRequestExecution_runbooks,

    -- * Destructuring the Response
    StartChangeRequestExecutionResponse (..),
    newStartChangeRequestExecutionResponse,

    -- * Response Lenses
    startChangeRequestExecutionResponse_automationExecutionId,
    startChangeRequestExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newStartChangeRequestExecution' smart constructor.
data StartChangeRequestExecution = StartChangeRequestExecution'
  { -- | The name of the change request associated with the runbook workflow to
    -- be run.
    changeRequestName :: Core.Maybe Core.Text,
    -- | The date and time specified in the change request to run the Automation
    -- runbooks.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    scheduledTime :: Core.Maybe Core.POSIX,
    -- | Optional metadata that you assign to a resource. You can specify a
    -- maximum of five tags for a change request. Tags enable you to categorize
    -- a resource in different ways, such as by purpose, owner, or environment.
    -- For example, you might want to tag a change request to identify an
    -- environment or target AWS Region. In this case, you could specify the
    -- following key-value pairs:
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- -   @Key=Region,Value=us-east-2@
    tags :: Core.Maybe [Tag],
    -- | The version of the change template document to run during the runbook
    -- workflow.
    documentVersion :: Core.Maybe Core.Text,
    -- | A key-value map of parameters that match the declared parameters in the
    -- change template document.
    parameters :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The user-provided idempotency token. The token must be unique, is case
    -- insensitive, enforces the UUID format, and can\'t be reused.
    clientToken :: Core.Maybe Core.Text,
    -- | The name of the change template document to run during the runbook
    -- workflow.
    documentName :: Core.Text,
    -- | Information about the Automation runbooks (Automation documents) that
    -- are run during the runbook workflow.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    runbooks :: Core.NonEmpty Runbook
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartChangeRequestExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changeRequestName', 'startChangeRequestExecution_changeRequestName' - The name of the change request associated with the runbook workflow to
-- be run.
--
-- 'scheduledTime', 'startChangeRequestExecution_scheduledTime' - The date and time specified in the change request to run the Automation
-- runbooks.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- 'tags', 'startChangeRequestExecution_tags' - Optional metadata that you assign to a resource. You can specify a
-- maximum of five tags for a change request. Tags enable you to categorize
-- a resource in different ways, such as by purpose, owner, or environment.
-- For example, you might want to tag a change request to identify an
-- environment or target AWS Region. In this case, you could specify the
-- following key-value pairs:
--
-- -   @Key=Environment,Value=Production@
--
-- -   @Key=Region,Value=us-east-2@
--
-- 'documentVersion', 'startChangeRequestExecution_documentVersion' - The version of the change template document to run during the runbook
-- workflow.
--
-- 'parameters', 'startChangeRequestExecution_parameters' - A key-value map of parameters that match the declared parameters in the
-- change template document.
--
-- 'clientToken', 'startChangeRequestExecution_clientToken' - The user-provided idempotency token. The token must be unique, is case
-- insensitive, enforces the UUID format, and can\'t be reused.
--
-- 'documentName', 'startChangeRequestExecution_documentName' - The name of the change template document to run during the runbook
-- workflow.
--
-- 'runbooks', 'startChangeRequestExecution_runbooks' - Information about the Automation runbooks (Automation documents) that
-- are run during the runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
newStartChangeRequestExecution ::
  -- | 'documentName'
  Core.Text ->
  -- | 'runbooks'
  Core.NonEmpty Runbook ->
  StartChangeRequestExecution
newStartChangeRequestExecution
  pDocumentName_
  pRunbooks_ =
    StartChangeRequestExecution'
      { changeRequestName =
          Core.Nothing,
        scheduledTime = Core.Nothing,
        tags = Core.Nothing,
        documentVersion = Core.Nothing,
        parameters = Core.Nothing,
        clientToken = Core.Nothing,
        documentName = pDocumentName_,
        runbooks = Lens._Coerce Lens.# pRunbooks_
      }

-- | The name of the change request associated with the runbook workflow to
-- be run.
startChangeRequestExecution_changeRequestName :: Lens.Lens' StartChangeRequestExecution (Core.Maybe Core.Text)
startChangeRequestExecution_changeRequestName = Lens.lens (\StartChangeRequestExecution' {changeRequestName} -> changeRequestName) (\s@StartChangeRequestExecution' {} a -> s {changeRequestName = a} :: StartChangeRequestExecution)

-- | The date and time specified in the change request to run the Automation
-- runbooks.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
startChangeRequestExecution_scheduledTime :: Lens.Lens' StartChangeRequestExecution (Core.Maybe Core.UTCTime)
startChangeRequestExecution_scheduledTime = Lens.lens (\StartChangeRequestExecution' {scheduledTime} -> scheduledTime) (\s@StartChangeRequestExecution' {} a -> s {scheduledTime = a} :: StartChangeRequestExecution) Core.. Lens.mapping Core._Time

-- | Optional metadata that you assign to a resource. You can specify a
-- maximum of five tags for a change request. Tags enable you to categorize
-- a resource in different ways, such as by purpose, owner, or environment.
-- For example, you might want to tag a change request to identify an
-- environment or target AWS Region. In this case, you could specify the
-- following key-value pairs:
--
-- -   @Key=Environment,Value=Production@
--
-- -   @Key=Region,Value=us-east-2@
startChangeRequestExecution_tags :: Lens.Lens' StartChangeRequestExecution (Core.Maybe [Tag])
startChangeRequestExecution_tags = Lens.lens (\StartChangeRequestExecution' {tags} -> tags) (\s@StartChangeRequestExecution' {} a -> s {tags = a} :: StartChangeRequestExecution) Core.. Lens.mapping Lens._Coerce

-- | The version of the change template document to run during the runbook
-- workflow.
startChangeRequestExecution_documentVersion :: Lens.Lens' StartChangeRequestExecution (Core.Maybe Core.Text)
startChangeRequestExecution_documentVersion = Lens.lens (\StartChangeRequestExecution' {documentVersion} -> documentVersion) (\s@StartChangeRequestExecution' {} a -> s {documentVersion = a} :: StartChangeRequestExecution)

-- | A key-value map of parameters that match the declared parameters in the
-- change template document.
startChangeRequestExecution_parameters :: Lens.Lens' StartChangeRequestExecution (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
startChangeRequestExecution_parameters = Lens.lens (\StartChangeRequestExecution' {parameters} -> parameters) (\s@StartChangeRequestExecution' {} a -> s {parameters = a} :: StartChangeRequestExecution) Core.. Lens.mapping Lens._Coerce

-- | The user-provided idempotency token. The token must be unique, is case
-- insensitive, enforces the UUID format, and can\'t be reused.
startChangeRequestExecution_clientToken :: Lens.Lens' StartChangeRequestExecution (Core.Maybe Core.Text)
startChangeRequestExecution_clientToken = Lens.lens (\StartChangeRequestExecution' {clientToken} -> clientToken) (\s@StartChangeRequestExecution' {} a -> s {clientToken = a} :: StartChangeRequestExecution)

-- | The name of the change template document to run during the runbook
-- workflow.
startChangeRequestExecution_documentName :: Lens.Lens' StartChangeRequestExecution Core.Text
startChangeRequestExecution_documentName = Lens.lens (\StartChangeRequestExecution' {documentName} -> documentName) (\s@StartChangeRequestExecution' {} a -> s {documentName = a} :: StartChangeRequestExecution)

-- | Information about the Automation runbooks (Automation documents) that
-- are run during the runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
startChangeRequestExecution_runbooks :: Lens.Lens' StartChangeRequestExecution (Core.NonEmpty Runbook)
startChangeRequestExecution_runbooks = Lens.lens (\StartChangeRequestExecution' {runbooks} -> runbooks) (\s@StartChangeRequestExecution' {} a -> s {runbooks = a} :: StartChangeRequestExecution) Core.. Lens._Coerce

instance Core.AWSRequest StartChangeRequestExecution where
  type
    AWSResponse StartChangeRequestExecution =
      StartChangeRequestExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChangeRequestExecutionResponse'
            Core.<$> (x Core..?> "AutomationExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartChangeRequestExecution

instance Core.NFData StartChangeRequestExecution

instance Core.ToHeaders StartChangeRequestExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.StartChangeRequestExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartChangeRequestExecution where
  toJSON StartChangeRequestExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ChangeRequestName" Core..=)
              Core.<$> changeRequestName,
            ("ScheduledTime" Core..=) Core.<$> scheduledTime,
            ("Tags" Core..=) Core.<$> tags,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("Parameters" Core..=) Core.<$> parameters,
            ("ClientToken" Core..=) Core.<$> clientToken,
            Core.Just ("DocumentName" Core..= documentName),
            Core.Just ("Runbooks" Core..= runbooks)
          ]
      )

instance Core.ToPath StartChangeRequestExecution where
  toPath = Core.const "/"

instance Core.ToQuery StartChangeRequestExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartChangeRequestExecutionResponse' smart constructor.
data StartChangeRequestExecutionResponse = StartChangeRequestExecutionResponse'
  { -- | The unique ID of a runbook workflow operation. (A runbook workflow is a
    -- type of Automation operation.)
    automationExecutionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartChangeRequestExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automationExecutionId', 'startChangeRequestExecutionResponse_automationExecutionId' - The unique ID of a runbook workflow operation. (A runbook workflow is a
-- type of Automation operation.)
--
-- 'httpStatus', 'startChangeRequestExecutionResponse_httpStatus' - The response's http status code.
newStartChangeRequestExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartChangeRequestExecutionResponse
newStartChangeRequestExecutionResponse pHttpStatus_ =
  StartChangeRequestExecutionResponse'
    { automationExecutionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of a runbook workflow operation. (A runbook workflow is a
-- type of Automation operation.)
startChangeRequestExecutionResponse_automationExecutionId :: Lens.Lens' StartChangeRequestExecutionResponse (Core.Maybe Core.Text)
startChangeRequestExecutionResponse_automationExecutionId = Lens.lens (\StartChangeRequestExecutionResponse' {automationExecutionId} -> automationExecutionId) (\s@StartChangeRequestExecutionResponse' {} a -> s {automationExecutionId = a} :: StartChangeRequestExecutionResponse)

-- | The response's http status code.
startChangeRequestExecutionResponse_httpStatus :: Lens.Lens' StartChangeRequestExecutionResponse Core.Int
startChangeRequestExecutionResponse_httpStatus = Lens.lens (\StartChangeRequestExecutionResponse' {httpStatus} -> httpStatus) (\s@StartChangeRequestExecutionResponse' {} a -> s {httpStatus = a} :: StartChangeRequestExecutionResponse)

instance
  Core.NFData
    StartChangeRequestExecutionResponse
