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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    submitTaskStateChange_executionStoppedAt,
    submitTaskStateChange_task,
    submitTaskStateChange_pullStoppedAt,
    submitTaskStateChange_cluster,
    submitTaskStateChange_containers,
    submitTaskStateChange_pullStartedAt,
    submitTaskStateChange_status,
    submitTaskStateChange_attachments,
    submitTaskStateChange_managedAgents,
    submitTaskStateChange_reason,

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
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSubmitTaskStateChange' smart constructor.
data SubmitTaskStateChange = SubmitTaskStateChange'
  { -- | The Unix timestamp for the time when the task execution stopped.
    executionStoppedAt :: Prelude.Maybe Core.POSIX,
    -- | The task ID or full ARN of the task in the state change request.
    task :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for the time when the container image pull completed.
    pullStoppedAt :: Prelude.Maybe Core.POSIX,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the task.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Any containers that\'s associated with the state change request.
    containers :: Prelude.Maybe [ContainerStateChange],
    -- | The Unix timestamp for the time when the container image pull started.
    pullStartedAt :: Prelude.Maybe Core.POSIX,
    -- | The status of the state change request.
    status :: Prelude.Maybe Prelude.Text,
    -- | Any attachments associated with the state change request.
    attachments :: Prelude.Maybe [AttachmentStateChange],
    -- | The details for the managed agent that\'s associated with the task.
    managedAgents :: Prelude.Maybe [ManagedAgentStateChange],
    -- | The reason for the state change request.
    reason :: Prelude.Maybe Prelude.Text
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
-- 'executionStoppedAt', 'submitTaskStateChange_executionStoppedAt' - The Unix timestamp for the time when the task execution stopped.
--
-- 'task', 'submitTaskStateChange_task' - The task ID or full ARN of the task in the state change request.
--
-- 'pullStoppedAt', 'submitTaskStateChange_pullStoppedAt' - The Unix timestamp for the time when the container image pull completed.
--
-- 'cluster', 'submitTaskStateChange_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
--
-- 'containers', 'submitTaskStateChange_containers' - Any containers that\'s associated with the state change request.
--
-- 'pullStartedAt', 'submitTaskStateChange_pullStartedAt' - The Unix timestamp for the time when the container image pull started.
--
-- 'status', 'submitTaskStateChange_status' - The status of the state change request.
--
-- 'attachments', 'submitTaskStateChange_attachments' - Any attachments associated with the state change request.
--
-- 'managedAgents', 'submitTaskStateChange_managedAgents' - The details for the managed agent that\'s associated with the task.
--
-- 'reason', 'submitTaskStateChange_reason' - The reason for the state change request.
newSubmitTaskStateChange ::
  SubmitTaskStateChange
newSubmitTaskStateChange =
  SubmitTaskStateChange'
    { executionStoppedAt =
        Prelude.Nothing,
      task = Prelude.Nothing,
      pullStoppedAt = Prelude.Nothing,
      cluster = Prelude.Nothing,
      containers = Prelude.Nothing,
      pullStartedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      attachments = Prelude.Nothing,
      managedAgents = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The Unix timestamp for the time when the task execution stopped.
submitTaskStateChange_executionStoppedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_executionStoppedAt = Lens.lens (\SubmitTaskStateChange' {executionStoppedAt} -> executionStoppedAt) (\s@SubmitTaskStateChange' {} a -> s {executionStoppedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Core._Time

-- | The task ID or full ARN of the task in the state change request.
submitTaskStateChange_task :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_task = Lens.lens (\SubmitTaskStateChange' {task} -> task) (\s@SubmitTaskStateChange' {} a -> s {task = a} :: SubmitTaskStateChange)

-- | The Unix timestamp for the time when the container image pull completed.
submitTaskStateChange_pullStoppedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_pullStoppedAt = Lens.lens (\SubmitTaskStateChange' {pullStoppedAt} -> pullStoppedAt) (\s@SubmitTaskStateChange' {} a -> s {pullStoppedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Core._Time

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
submitTaskStateChange_cluster :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_cluster = Lens.lens (\SubmitTaskStateChange' {cluster} -> cluster) (\s@SubmitTaskStateChange' {} a -> s {cluster = a} :: SubmitTaskStateChange)

-- | Any containers that\'s associated with the state change request.
submitTaskStateChange_containers :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [ContainerStateChange])
submitTaskStateChange_containers = Lens.lens (\SubmitTaskStateChange' {containers} -> containers) (\s@SubmitTaskStateChange' {} a -> s {containers = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for the time when the container image pull started.
submitTaskStateChange_pullStartedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_pullStartedAt = Lens.lens (\SubmitTaskStateChange' {pullStartedAt} -> pullStartedAt) (\s@SubmitTaskStateChange' {} a -> s {pullStartedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Core._Time

-- | The status of the state change request.
submitTaskStateChange_status :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_status = Lens.lens (\SubmitTaskStateChange' {status} -> status) (\s@SubmitTaskStateChange' {} a -> s {status = a} :: SubmitTaskStateChange)

-- | Any attachments associated with the state change request.
submitTaskStateChange_attachments :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [AttachmentStateChange])
submitTaskStateChange_attachments = Lens.lens (\SubmitTaskStateChange' {attachments} -> attachments) (\s@SubmitTaskStateChange' {} a -> s {attachments = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Lens.coerced

-- | The details for the managed agent that\'s associated with the task.
submitTaskStateChange_managedAgents :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [ManagedAgentStateChange])
submitTaskStateChange_managedAgents = Lens.lens (\SubmitTaskStateChange' {managedAgents} -> managedAgents) (\s@SubmitTaskStateChange' {} a -> s {managedAgents = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Lens.coerced

-- | The reason for the state change request.
submitTaskStateChange_reason :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_reason = Lens.lens (\SubmitTaskStateChange' {reason} -> reason) (\s@SubmitTaskStateChange' {} a -> s {reason = a} :: SubmitTaskStateChange)

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
            Prelude.<$> (x Core..?> "acknowledgment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SubmitTaskStateChange where
  hashWithSalt _salt SubmitTaskStateChange' {..} =
    _salt `Prelude.hashWithSalt` executionStoppedAt
      `Prelude.hashWithSalt` task
      `Prelude.hashWithSalt` pullStoppedAt
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` pullStartedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` managedAgents
      `Prelude.hashWithSalt` reason

instance Prelude.NFData SubmitTaskStateChange where
  rnf SubmitTaskStateChange' {..} =
    Prelude.rnf executionStoppedAt
      `Prelude.seq` Prelude.rnf task
      `Prelude.seq` Prelude.rnf pullStoppedAt
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf containers
      `Prelude.seq` Prelude.rnf pullStartedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf managedAgents
      `Prelude.seq` Prelude.rnf reason

instance Core.ToHeaders SubmitTaskStateChange where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.SubmitTaskStateChange" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SubmitTaskStateChange where
  toJSON SubmitTaskStateChange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("executionStoppedAt" Core..=)
              Prelude.<$> executionStoppedAt,
            ("task" Core..=) Prelude.<$> task,
            ("pullStoppedAt" Core..=) Prelude.<$> pullStoppedAt,
            ("cluster" Core..=) Prelude.<$> cluster,
            ("containers" Core..=) Prelude.<$> containers,
            ("pullStartedAt" Core..=) Prelude.<$> pullStartedAt,
            ("status" Core..=) Prelude.<$> status,
            ("attachments" Core..=) Prelude.<$> attachments,
            ("managedAgents" Core..=) Prelude.<$> managedAgents,
            ("reason" Core..=) Prelude.<$> reason
          ]
      )

instance Core.ToPath SubmitTaskStateChange where
  toPath = Prelude.const "/"

instance Core.ToQuery SubmitTaskStateChange where
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
