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
-- Module      : Amazonka.ECS.SubmitTaskStateChange
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Sent to acknowledge that a task changed states.
module Amazonka.ECS.SubmitTaskStateChange
  ( -- * Creating a Request
    SubmitTaskStateChange (..),
    newSubmitTaskStateChange,

    -- * Request Lenses
    submitTaskStateChange_attachments,
    submitTaskStateChange_cluster,
    submitTaskStateChange_containers,
    submitTaskStateChange_executionStoppedAt,
    submitTaskStateChange_managedAgents,
    submitTaskStateChange_pullStartedAt,
    submitTaskStateChange_pullStoppedAt,
    submitTaskStateChange_reason,
    submitTaskStateChange_status,
    submitTaskStateChange_task,

    -- * Destructuring the Response
    SubmitTaskStateChangeResponse (..),
    newSubmitTaskStateChangeResponse,

    -- * Response Lenses
    submitTaskStateChangeResponse_acknowledgment,
    submitTaskStateChangeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSubmitTaskStateChange' smart constructor.
data SubmitTaskStateChange = SubmitTaskStateChange'
  { -- | Any attachments associated with the state change request.
    attachments :: Prelude.Maybe [AttachmentStateChange],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the task.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Any containers that\'s associated with the state change request.
    containers :: Prelude.Maybe [ContainerStateChange],
    -- | The Unix timestamp for the time when the task execution stopped.
    executionStoppedAt :: Prelude.Maybe Data.POSIX,
    -- | The details for the managed agent that\'s associated with the task.
    managedAgents :: Prelude.Maybe [ManagedAgentStateChange],
    -- | The Unix timestamp for the time when the container image pull started.
    pullStartedAt :: Prelude.Maybe Data.POSIX,
    -- | The Unix timestamp for the time when the container image pull completed.
    pullStoppedAt :: Prelude.Maybe Data.POSIX,
    -- | The reason for the state change request.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The status of the state change request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The task ID or full ARN of the task in the state change request.
    task :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitTaskStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachments', 'submitTaskStateChange_attachments' - Any attachments associated with the state change request.
--
-- 'cluster', 'submitTaskStateChange_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
--
-- 'containers', 'submitTaskStateChange_containers' - Any containers that\'s associated with the state change request.
--
-- 'executionStoppedAt', 'submitTaskStateChange_executionStoppedAt' - The Unix timestamp for the time when the task execution stopped.
--
-- 'managedAgents', 'submitTaskStateChange_managedAgents' - The details for the managed agent that\'s associated with the task.
--
-- 'pullStartedAt', 'submitTaskStateChange_pullStartedAt' - The Unix timestamp for the time when the container image pull started.
--
-- 'pullStoppedAt', 'submitTaskStateChange_pullStoppedAt' - The Unix timestamp for the time when the container image pull completed.
--
-- 'reason', 'submitTaskStateChange_reason' - The reason for the state change request.
--
-- 'status', 'submitTaskStateChange_status' - The status of the state change request.
--
-- 'task', 'submitTaskStateChange_task' - The task ID or full ARN of the task in the state change request.
newSubmitTaskStateChange ::
  SubmitTaskStateChange
newSubmitTaskStateChange =
  SubmitTaskStateChange'
    { attachments =
        Prelude.Nothing,
      cluster = Prelude.Nothing,
      containers = Prelude.Nothing,
      executionStoppedAt = Prelude.Nothing,
      managedAgents = Prelude.Nothing,
      pullStartedAt = Prelude.Nothing,
      pullStoppedAt = Prelude.Nothing,
      reason = Prelude.Nothing,
      status = Prelude.Nothing,
      task = Prelude.Nothing
    }

-- | Any attachments associated with the state change request.
submitTaskStateChange_attachments :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [AttachmentStateChange])
submitTaskStateChange_attachments = Lens.lens (\SubmitTaskStateChange' {attachments} -> attachments) (\s@SubmitTaskStateChange' {} a -> s {attachments = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Lens.coerced

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
submitTaskStateChange_cluster :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_cluster = Lens.lens (\SubmitTaskStateChange' {cluster} -> cluster) (\s@SubmitTaskStateChange' {} a -> s {cluster = a} :: SubmitTaskStateChange)

-- | Any containers that\'s associated with the state change request.
submitTaskStateChange_containers :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [ContainerStateChange])
submitTaskStateChange_containers = Lens.lens (\SubmitTaskStateChange' {containers} -> containers) (\s@SubmitTaskStateChange' {} a -> s {containers = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for the time when the task execution stopped.
submitTaskStateChange_executionStoppedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_executionStoppedAt = Lens.lens (\SubmitTaskStateChange' {executionStoppedAt} -> executionStoppedAt) (\s@SubmitTaskStateChange' {} a -> s {executionStoppedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Data._Time

-- | The details for the managed agent that\'s associated with the task.
submitTaskStateChange_managedAgents :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [ManagedAgentStateChange])
submitTaskStateChange_managedAgents = Lens.lens (\SubmitTaskStateChange' {managedAgents} -> managedAgents) (\s@SubmitTaskStateChange' {} a -> s {managedAgents = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for the time when the container image pull started.
submitTaskStateChange_pullStartedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_pullStartedAt = Lens.lens (\SubmitTaskStateChange' {pullStartedAt} -> pullStartedAt) (\s@SubmitTaskStateChange' {} a -> s {pullStartedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Data._Time

-- | The Unix timestamp for the time when the container image pull completed.
submitTaskStateChange_pullStoppedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_pullStoppedAt = Lens.lens (\SubmitTaskStateChange' {pullStoppedAt} -> pullStoppedAt) (\s@SubmitTaskStateChange' {} a -> s {pullStoppedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Data._Time

-- | The reason for the state change request.
submitTaskStateChange_reason :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_reason = Lens.lens (\SubmitTaskStateChange' {reason} -> reason) (\s@SubmitTaskStateChange' {} a -> s {reason = a} :: SubmitTaskStateChange)

-- | The status of the state change request.
submitTaskStateChange_status :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_status = Lens.lens (\SubmitTaskStateChange' {status} -> status) (\s@SubmitTaskStateChange' {} a -> s {status = a} :: SubmitTaskStateChange)

-- | The task ID or full ARN of the task in the state change request.
submitTaskStateChange_task :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_task = Lens.lens (\SubmitTaskStateChange' {task} -> task) (\s@SubmitTaskStateChange' {} a -> s {task = a} :: SubmitTaskStateChange)

instance Core.AWSRequest SubmitTaskStateChange where
  type
    AWSResponse SubmitTaskStateChange =
      SubmitTaskStateChangeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitTaskStateChangeResponse'
            Prelude.<$> (x Data..?> "acknowledgment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SubmitTaskStateChange where
  hashWithSalt _salt SubmitTaskStateChange' {..} =
    _salt `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` executionStoppedAt
      `Prelude.hashWithSalt` managedAgents
      `Prelude.hashWithSalt` pullStartedAt
      `Prelude.hashWithSalt` pullStoppedAt
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` task

instance Prelude.NFData SubmitTaskStateChange where
  rnf SubmitTaskStateChange' {..} =
    Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf containers
      `Prelude.seq` Prelude.rnf executionStoppedAt
      `Prelude.seq` Prelude.rnf managedAgents
      `Prelude.seq` Prelude.rnf pullStartedAt
      `Prelude.seq` Prelude.rnf pullStoppedAt
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf task

instance Data.ToHeaders SubmitTaskStateChange where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.SubmitTaskStateChange" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SubmitTaskStateChange where
  toJSON SubmitTaskStateChange' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attachments" Data..=) Prelude.<$> attachments,
            ("cluster" Data..=) Prelude.<$> cluster,
            ("containers" Data..=) Prelude.<$> containers,
            ("executionStoppedAt" Data..=)
              Prelude.<$> executionStoppedAt,
            ("managedAgents" Data..=) Prelude.<$> managedAgents,
            ("pullStartedAt" Data..=) Prelude.<$> pullStartedAt,
            ("pullStoppedAt" Data..=) Prelude.<$> pullStoppedAt,
            ("reason" Data..=) Prelude.<$> reason,
            ("status" Data..=) Prelude.<$> status,
            ("task" Data..=) Prelude.<$> task
          ]
      )

instance Data.ToPath SubmitTaskStateChange where
  toPath = Prelude.const "/"

instance Data.ToQuery SubmitTaskStateChange where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubmitTaskStateChangeResponse' smart constructor.
data SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitTaskStateChangeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acknowledgment', 'submitTaskStateChangeResponse_acknowledgment' - Acknowledgement of the state change.
--
-- 'httpStatus', 'submitTaskStateChangeResponse_httpStatus' - The response's http status code.
newSubmitTaskStateChangeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SubmitTaskStateChangeResponse
newSubmitTaskStateChangeResponse pHttpStatus_ =
  SubmitTaskStateChangeResponse'
    { acknowledgment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Acknowledgement of the state change.
submitTaskStateChangeResponse_acknowledgment :: Lens.Lens' SubmitTaskStateChangeResponse (Prelude.Maybe Prelude.Text)
submitTaskStateChangeResponse_acknowledgment = Lens.lens (\SubmitTaskStateChangeResponse' {acknowledgment} -> acknowledgment) (\s@SubmitTaskStateChangeResponse' {} a -> s {acknowledgment = a} :: SubmitTaskStateChangeResponse)

-- | The response's http status code.
submitTaskStateChangeResponse_httpStatus :: Lens.Lens' SubmitTaskStateChangeResponse Prelude.Int
submitTaskStateChangeResponse_httpStatus = Lens.lens (\SubmitTaskStateChangeResponse' {httpStatus} -> httpStatus) (\s@SubmitTaskStateChangeResponse' {} a -> s {httpStatus = a} :: SubmitTaskStateChangeResponse)

instance Prelude.NFData SubmitTaskStateChangeResponse where
  rnf SubmitTaskStateChangeResponse' {..} =
    Prelude.rnf acknowledgment
      `Prelude.seq` Prelude.rnf httpStatus
