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
-- Module      : Amazonka.SSM.StartChangeRequestExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a change request for Change Manager. The Automation runbooks
-- specified in the change request run only after all required approvals
-- for the change request have been received.
module Amazonka.SSM.StartChangeRequestExecution
  ( -- * Creating a Request
    StartChangeRequestExecution (..),
    newStartChangeRequestExecution,

    -- * Request Lenses
    startChangeRequestExecution_tags,
    startChangeRequestExecution_clientToken,
    startChangeRequestExecution_changeDetails,
    startChangeRequestExecution_autoApprove,
    startChangeRequestExecution_changeRequestName,
    startChangeRequestExecution_scheduledEndTime,
    startChangeRequestExecution_scheduledTime,
    startChangeRequestExecution_documentVersion,
    startChangeRequestExecution_parameters,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newStartChangeRequestExecution' smart constructor.
data StartChangeRequestExecution = StartChangeRequestExecution'
  { -- | Optional metadata that you assign to a resource. You can specify a
    -- maximum of five tags for a change request. Tags enable you to categorize
    -- a resource in different ways, such as by purpose, owner, or environment.
    -- For example, you might want to tag a change request to identify an
    -- environment or target Amazon Web Services Region. In this case, you
    -- could specify the following key-value pairs:
    --
    -- -   @Key=Environment,Value=Production@
    --
    -- -   @Key=Region,Value=us-east-2@
    tags :: Prelude.Maybe [Tag],
    -- | The user-provided idempotency token. The token must be unique, is case
    -- insensitive, enforces the UUID format, and can\'t be reused.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | User-provided details about the change. If no details are provided,
    -- content specified in the __Template information__ section of the
    -- associated change template is added.
    changeDetails :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the change request can be approved automatically
    -- without the need for manual approvals.
    --
    -- If @AutoApprovable@ is enabled in a change template, then setting
    -- @AutoApprove@ to @true@ in @StartChangeRequestExecution@ creates a
    -- change request that bypasses approver review.
    --
    -- Change Calendar restrictions are not bypassed in this scenario. If the
    -- state of an associated calendar is @CLOSED@, change freeze approvers
    -- must still grant permission for this change request to run. If they
    -- don\'t, the change won\'t be processed until the calendar state is again
    -- @OPEN@.
    autoApprove :: Prelude.Maybe Prelude.Bool,
    -- | The name of the change request associated with the runbook workflow to
    -- be run.
    changeRequestName :: Prelude.Maybe Prelude.Text,
    -- | The time that the requester expects the runbook workflow related to the
    -- change request to complete. The time is an estimate only that the
    -- requester provides for reviewers.
    scheduledEndTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time specified in the change request to run the Automation
    -- runbooks.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    scheduledTime :: Prelude.Maybe Data.POSIX,
    -- | The version of the change template document to run during the runbook
    -- workflow.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | A key-value map of parameters that match the declared parameters in the
    -- change template document.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The name of the change template document to run during the runbook
    -- workflow.
    documentName :: Prelude.Text,
    -- | Information about the Automation runbooks that are run during the
    -- runbook workflow.
    --
    -- The Automation runbooks specified for the runbook workflow can\'t run
    -- until all required approvals for the change request have been received.
    runbooks :: Prelude.NonEmpty Runbook
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartChangeRequestExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'startChangeRequestExecution_tags' - Optional metadata that you assign to a resource. You can specify a
-- maximum of five tags for a change request. Tags enable you to categorize
-- a resource in different ways, such as by purpose, owner, or environment.
-- For example, you might want to tag a change request to identify an
-- environment or target Amazon Web Services Region. In this case, you
-- could specify the following key-value pairs:
--
-- -   @Key=Environment,Value=Production@
--
-- -   @Key=Region,Value=us-east-2@
--
-- 'clientToken', 'startChangeRequestExecution_clientToken' - The user-provided idempotency token. The token must be unique, is case
-- insensitive, enforces the UUID format, and can\'t be reused.
--
-- 'changeDetails', 'startChangeRequestExecution_changeDetails' - User-provided details about the change. If no details are provided,
-- content specified in the __Template information__ section of the
-- associated change template is added.
--
-- 'autoApprove', 'startChangeRequestExecution_autoApprove' - Indicates whether the change request can be approved automatically
-- without the need for manual approvals.
--
-- If @AutoApprovable@ is enabled in a change template, then setting
-- @AutoApprove@ to @true@ in @StartChangeRequestExecution@ creates a
-- change request that bypasses approver review.
--
-- Change Calendar restrictions are not bypassed in this scenario. If the
-- state of an associated calendar is @CLOSED@, change freeze approvers
-- must still grant permission for this change request to run. If they
-- don\'t, the change won\'t be processed until the calendar state is again
-- @OPEN@.
--
-- 'changeRequestName', 'startChangeRequestExecution_changeRequestName' - The name of the change request associated with the runbook workflow to
-- be run.
--
-- 'scheduledEndTime', 'startChangeRequestExecution_scheduledEndTime' - The time that the requester expects the runbook workflow related to the
-- change request to complete. The time is an estimate only that the
-- requester provides for reviewers.
--
-- 'scheduledTime', 'startChangeRequestExecution_scheduledTime' - The date and time specified in the change request to run the Automation
-- runbooks.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
--
-- 'documentVersion', 'startChangeRequestExecution_documentVersion' - The version of the change template document to run during the runbook
-- workflow.
--
-- 'parameters', 'startChangeRequestExecution_parameters' - A key-value map of parameters that match the declared parameters in the
-- change template document.
--
-- 'documentName', 'startChangeRequestExecution_documentName' - The name of the change template document to run during the runbook
-- workflow.
--
-- 'runbooks', 'startChangeRequestExecution_runbooks' - Information about the Automation runbooks that are run during the
-- runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
newStartChangeRequestExecution ::
  -- | 'documentName'
  Prelude.Text ->
  -- | 'runbooks'
  Prelude.NonEmpty Runbook ->
  StartChangeRequestExecution
newStartChangeRequestExecution
  pDocumentName_
  pRunbooks_ =
    StartChangeRequestExecution'
      { tags =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        changeDetails = Prelude.Nothing,
        autoApprove = Prelude.Nothing,
        changeRequestName = Prelude.Nothing,
        scheduledEndTime = Prelude.Nothing,
        scheduledTime = Prelude.Nothing,
        documentVersion = Prelude.Nothing,
        parameters = Prelude.Nothing,
        documentName = pDocumentName_,
        runbooks = Lens.coerced Lens.# pRunbooks_
      }

-- | Optional metadata that you assign to a resource. You can specify a
-- maximum of five tags for a change request. Tags enable you to categorize
-- a resource in different ways, such as by purpose, owner, or environment.
-- For example, you might want to tag a change request to identify an
-- environment or target Amazon Web Services Region. In this case, you
-- could specify the following key-value pairs:
--
-- -   @Key=Environment,Value=Production@
--
-- -   @Key=Region,Value=us-east-2@
startChangeRequestExecution_tags :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe [Tag])
startChangeRequestExecution_tags = Lens.lens (\StartChangeRequestExecution' {tags} -> tags) (\s@StartChangeRequestExecution' {} a -> s {tags = a} :: StartChangeRequestExecution) Prelude.. Lens.mapping Lens.coerced

-- | The user-provided idempotency token. The token must be unique, is case
-- insensitive, enforces the UUID format, and can\'t be reused.
startChangeRequestExecution_clientToken :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe Prelude.Text)
startChangeRequestExecution_clientToken = Lens.lens (\StartChangeRequestExecution' {clientToken} -> clientToken) (\s@StartChangeRequestExecution' {} a -> s {clientToken = a} :: StartChangeRequestExecution)

-- | User-provided details about the change. If no details are provided,
-- content specified in the __Template information__ section of the
-- associated change template is added.
startChangeRequestExecution_changeDetails :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe Prelude.Text)
startChangeRequestExecution_changeDetails = Lens.lens (\StartChangeRequestExecution' {changeDetails} -> changeDetails) (\s@StartChangeRequestExecution' {} a -> s {changeDetails = a} :: StartChangeRequestExecution)

-- | Indicates whether the change request can be approved automatically
-- without the need for manual approvals.
--
-- If @AutoApprovable@ is enabled in a change template, then setting
-- @AutoApprove@ to @true@ in @StartChangeRequestExecution@ creates a
-- change request that bypasses approver review.
--
-- Change Calendar restrictions are not bypassed in this scenario. If the
-- state of an associated calendar is @CLOSED@, change freeze approvers
-- must still grant permission for this change request to run. If they
-- don\'t, the change won\'t be processed until the calendar state is again
-- @OPEN@.
startChangeRequestExecution_autoApprove :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe Prelude.Bool)
startChangeRequestExecution_autoApprove = Lens.lens (\StartChangeRequestExecution' {autoApprove} -> autoApprove) (\s@StartChangeRequestExecution' {} a -> s {autoApprove = a} :: StartChangeRequestExecution)

-- | The name of the change request associated with the runbook workflow to
-- be run.
startChangeRequestExecution_changeRequestName :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe Prelude.Text)
startChangeRequestExecution_changeRequestName = Lens.lens (\StartChangeRequestExecution' {changeRequestName} -> changeRequestName) (\s@StartChangeRequestExecution' {} a -> s {changeRequestName = a} :: StartChangeRequestExecution)

-- | The time that the requester expects the runbook workflow related to the
-- change request to complete. The time is an estimate only that the
-- requester provides for reviewers.
startChangeRequestExecution_scheduledEndTime :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe Prelude.UTCTime)
startChangeRequestExecution_scheduledEndTime = Lens.lens (\StartChangeRequestExecution' {scheduledEndTime} -> scheduledEndTime) (\s@StartChangeRequestExecution' {} a -> s {scheduledEndTime = a} :: StartChangeRequestExecution) Prelude.. Lens.mapping Data._Time

-- | The date and time specified in the change request to run the Automation
-- runbooks.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
startChangeRequestExecution_scheduledTime :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe Prelude.UTCTime)
startChangeRequestExecution_scheduledTime = Lens.lens (\StartChangeRequestExecution' {scheduledTime} -> scheduledTime) (\s@StartChangeRequestExecution' {} a -> s {scheduledTime = a} :: StartChangeRequestExecution) Prelude.. Lens.mapping Data._Time

-- | The version of the change template document to run during the runbook
-- workflow.
startChangeRequestExecution_documentVersion :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe Prelude.Text)
startChangeRequestExecution_documentVersion = Lens.lens (\StartChangeRequestExecution' {documentVersion} -> documentVersion) (\s@StartChangeRequestExecution' {} a -> s {documentVersion = a} :: StartChangeRequestExecution)

-- | A key-value map of parameters that match the declared parameters in the
-- change template document.
startChangeRequestExecution_parameters :: Lens.Lens' StartChangeRequestExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
startChangeRequestExecution_parameters = Lens.lens (\StartChangeRequestExecution' {parameters} -> parameters) (\s@StartChangeRequestExecution' {} a -> s {parameters = a} :: StartChangeRequestExecution) Prelude.. Lens.mapping Lens.coerced

-- | The name of the change template document to run during the runbook
-- workflow.
startChangeRequestExecution_documentName :: Lens.Lens' StartChangeRequestExecution Prelude.Text
startChangeRequestExecution_documentName = Lens.lens (\StartChangeRequestExecution' {documentName} -> documentName) (\s@StartChangeRequestExecution' {} a -> s {documentName = a} :: StartChangeRequestExecution)

-- | Information about the Automation runbooks that are run during the
-- runbook workflow.
--
-- The Automation runbooks specified for the runbook workflow can\'t run
-- until all required approvals for the change request have been received.
startChangeRequestExecution_runbooks :: Lens.Lens' StartChangeRequestExecution (Prelude.NonEmpty Runbook)
startChangeRequestExecution_runbooks = Lens.lens (\StartChangeRequestExecution' {runbooks} -> runbooks) (\s@StartChangeRequestExecution' {} a -> s {runbooks = a} :: StartChangeRequestExecution) Prelude.. Lens.coerced

instance Core.AWSRequest StartChangeRequestExecution where
  type
    AWSResponse StartChangeRequestExecution =
      StartChangeRequestExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartChangeRequestExecutionResponse'
            Prelude.<$> (x Data..?> "AutomationExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartChangeRequestExecution where
  hashWithSalt _salt StartChangeRequestExecution' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` changeDetails
      `Prelude.hashWithSalt` autoApprove
      `Prelude.hashWithSalt` changeRequestName
      `Prelude.hashWithSalt` scheduledEndTime
      `Prelude.hashWithSalt` scheduledTime
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` documentName
      `Prelude.hashWithSalt` runbooks

instance Prelude.NFData StartChangeRequestExecution where
  rnf StartChangeRequestExecution' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf changeDetails
      `Prelude.seq` Prelude.rnf autoApprove
      `Prelude.seq` Prelude.rnf changeRequestName
      `Prelude.seq` Prelude.rnf scheduledEndTime
      `Prelude.seq` Prelude.rnf scheduledTime
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf documentName
      `Prelude.seq` Prelude.rnf runbooks

instance Data.ToHeaders StartChangeRequestExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.StartChangeRequestExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartChangeRequestExecution where
  toJSON StartChangeRequestExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("ChangeDetails" Data..=) Prelude.<$> changeDetails,
            ("AutoApprove" Data..=) Prelude.<$> autoApprove,
            ("ChangeRequestName" Data..=)
              Prelude.<$> changeRequestName,
            ("ScheduledEndTime" Data..=)
              Prelude.<$> scheduledEndTime,
            ("ScheduledTime" Data..=) Prelude.<$> scheduledTime,
            ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("Parameters" Data..=) Prelude.<$> parameters,
            Prelude.Just ("DocumentName" Data..= documentName),
            Prelude.Just ("Runbooks" Data..= runbooks)
          ]
      )

instance Data.ToPath StartChangeRequestExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StartChangeRequestExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartChangeRequestExecutionResponse' smart constructor.
data StartChangeRequestExecutionResponse = StartChangeRequestExecutionResponse'
  { -- | The unique ID of a runbook workflow operation. (A runbook workflow is a
    -- type of Automation operation.)
    automationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  StartChangeRequestExecutionResponse
newStartChangeRequestExecutionResponse pHttpStatus_ =
  StartChangeRequestExecutionResponse'
    { automationExecutionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID of a runbook workflow operation. (A runbook workflow is a
-- type of Automation operation.)
startChangeRequestExecutionResponse_automationExecutionId :: Lens.Lens' StartChangeRequestExecutionResponse (Prelude.Maybe Prelude.Text)
startChangeRequestExecutionResponse_automationExecutionId = Lens.lens (\StartChangeRequestExecutionResponse' {automationExecutionId} -> automationExecutionId) (\s@StartChangeRequestExecutionResponse' {} a -> s {automationExecutionId = a} :: StartChangeRequestExecutionResponse)

-- | The response's http status code.
startChangeRequestExecutionResponse_httpStatus :: Lens.Lens' StartChangeRequestExecutionResponse Prelude.Int
startChangeRequestExecutionResponse_httpStatus = Lens.lens (\StartChangeRequestExecutionResponse' {httpStatus} -> httpStatus) (\s@StartChangeRequestExecutionResponse' {} a -> s {httpStatus = a} :: StartChangeRequestExecutionResponse)

instance
  Prelude.NFData
    StartChangeRequestExecutionResponse
  where
  rnf StartChangeRequestExecutionResponse' {..} =
    Prelude.rnf automationExecutionId
      `Prelude.seq` Prelude.rnf httpStatus
