{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.SubmitTaskStateChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Sent to acknowledge that a task changed states.
module Network.AWS.ECS.SubmitTaskStateChange
  ( -- * Creating a Request
    SubmitTaskStateChange (..),
    newSubmitTaskStateChange,

    -- * Request Lenses
    submitTaskStateChange_status,
    submitTaskStateChange_pullStartedAt,
    submitTaskStateChange_task,
    submitTaskStateChange_containers,
    submitTaskStateChange_reason,
    submitTaskStateChange_pullStoppedAt,
    submitTaskStateChange_executionStoppedAt,
    submitTaskStateChange_cluster,
    submitTaskStateChange_attachments,

    -- * Destructuring the Response
    SubmitTaskStateChangeResponse (..),
    newSubmitTaskStateChangeResponse,

    -- * Response Lenses
    submitTaskStateChangeResponse_acknowledgment,
    submitTaskStateChangeResponse_httpStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSubmitTaskStateChange' smart constructor.
data SubmitTaskStateChange = SubmitTaskStateChange'
  { -- | The status of the state change request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the container image pull began.
    pullStartedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The task ID or full ARN of the task in the state change request.
    task :: Prelude.Maybe Prelude.Text,
    -- | Any containers associated with the state change request.
    containers :: Prelude.Maybe [ContainerStateChange],
    -- | The reason for the state change request.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the container image pull completed.
    pullStoppedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Unix timestamp for when the task execution stopped.
    executionStoppedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the task.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Any attachments associated with the state change request.
    attachments :: Prelude.Maybe [AttachmentStateChange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SubmitTaskStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'submitTaskStateChange_status' - The status of the state change request.
--
-- 'pullStartedAt', 'submitTaskStateChange_pullStartedAt' - The Unix timestamp for when the container image pull began.
--
-- 'task', 'submitTaskStateChange_task' - The task ID or full ARN of the task in the state change request.
--
-- 'containers', 'submitTaskStateChange_containers' - Any containers associated with the state change request.
--
-- 'reason', 'submitTaskStateChange_reason' - The reason for the state change request.
--
-- 'pullStoppedAt', 'submitTaskStateChange_pullStoppedAt' - The Unix timestamp for when the container image pull completed.
--
-- 'executionStoppedAt', 'submitTaskStateChange_executionStoppedAt' - The Unix timestamp for when the task execution stopped.
--
-- 'cluster', 'submitTaskStateChange_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
--
-- 'attachments', 'submitTaskStateChange_attachments' - Any attachments associated with the state change request.
newSubmitTaskStateChange ::
  SubmitTaskStateChange
newSubmitTaskStateChange =
  SubmitTaskStateChange'
    { status = Prelude.Nothing,
      pullStartedAt = Prelude.Nothing,
      task = Prelude.Nothing,
      containers = Prelude.Nothing,
      reason = Prelude.Nothing,
      pullStoppedAt = Prelude.Nothing,
      executionStoppedAt = Prelude.Nothing,
      cluster = Prelude.Nothing,
      attachments = Prelude.Nothing
    }

-- | The status of the state change request.
submitTaskStateChange_status :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_status = Lens.lens (\SubmitTaskStateChange' {status} -> status) (\s@SubmitTaskStateChange' {} a -> s {status = a} :: SubmitTaskStateChange)

-- | The Unix timestamp for when the container image pull began.
submitTaskStateChange_pullStartedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_pullStartedAt = Lens.lens (\SubmitTaskStateChange' {pullStartedAt} -> pullStartedAt) (\s@SubmitTaskStateChange' {} a -> s {pullStartedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Prelude._Time

-- | The task ID or full ARN of the task in the state change request.
submitTaskStateChange_task :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_task = Lens.lens (\SubmitTaskStateChange' {task} -> task) (\s@SubmitTaskStateChange' {} a -> s {task = a} :: SubmitTaskStateChange)

-- | Any containers associated with the state change request.
submitTaskStateChange_containers :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [ContainerStateChange])
submitTaskStateChange_containers = Lens.lens (\SubmitTaskStateChange' {containers} -> containers) (\s@SubmitTaskStateChange' {} a -> s {containers = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Prelude._Coerce

-- | The reason for the state change request.
submitTaskStateChange_reason :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_reason = Lens.lens (\SubmitTaskStateChange' {reason} -> reason) (\s@SubmitTaskStateChange' {} a -> s {reason = a} :: SubmitTaskStateChange)

-- | The Unix timestamp for when the container image pull completed.
submitTaskStateChange_pullStoppedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_pullStoppedAt = Lens.lens (\SubmitTaskStateChange' {pullStoppedAt} -> pullStoppedAt) (\s@SubmitTaskStateChange' {} a -> s {pullStoppedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Prelude._Time

-- | The Unix timestamp for when the task execution stopped.
submitTaskStateChange_executionStoppedAt :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.UTCTime)
submitTaskStateChange_executionStoppedAt = Lens.lens (\SubmitTaskStateChange' {executionStoppedAt} -> executionStoppedAt) (\s@SubmitTaskStateChange' {} a -> s {executionStoppedAt = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Prelude._Time

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task.
submitTaskStateChange_cluster :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe Prelude.Text)
submitTaskStateChange_cluster = Lens.lens (\SubmitTaskStateChange' {cluster} -> cluster) (\s@SubmitTaskStateChange' {} a -> s {cluster = a} :: SubmitTaskStateChange)

-- | Any attachments associated with the state change request.
submitTaskStateChange_attachments :: Lens.Lens' SubmitTaskStateChange (Prelude.Maybe [AttachmentStateChange])
submitTaskStateChange_attachments = Lens.lens (\SubmitTaskStateChange' {attachments} -> attachments) (\s@SubmitTaskStateChange' {} a -> s {attachments = a} :: SubmitTaskStateChange) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest SubmitTaskStateChange where
  type
    Rs SubmitTaskStateChange =
      SubmitTaskStateChangeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitTaskStateChangeResponse'
            Prelude.<$> (x Prelude..?> "acknowledgment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SubmitTaskStateChange

instance Prelude.NFData SubmitTaskStateChange

instance Prelude.ToHeaders SubmitTaskStateChange where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.SubmitTaskStateChange" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SubmitTaskStateChange where
  toJSON SubmitTaskStateChange' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("status" Prelude..=) Prelude.<$> status,
            ("pullStartedAt" Prelude..=)
              Prelude.<$> pullStartedAt,
            ("task" Prelude..=) Prelude.<$> task,
            ("containers" Prelude..=) Prelude.<$> containers,
            ("reason" Prelude..=) Prelude.<$> reason,
            ("pullStoppedAt" Prelude..=)
              Prelude.<$> pullStoppedAt,
            ("executionStoppedAt" Prelude..=)
              Prelude.<$> executionStoppedAt,
            ("cluster" Prelude..=) Prelude.<$> cluster,
            ("attachments" Prelude..=) Prelude.<$> attachments
          ]
      )

instance Prelude.ToPath SubmitTaskStateChange where
  toPath = Prelude.const "/"

instance Prelude.ToQuery SubmitTaskStateChange where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubmitTaskStateChangeResponse' smart constructor.
data SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData SubmitTaskStateChangeResponse
