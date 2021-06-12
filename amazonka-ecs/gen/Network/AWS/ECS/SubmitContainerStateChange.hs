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
-- Module      : Network.AWS.ECS.SubmitContainerStateChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Sent to acknowledge that a container changed states.
module Network.AWS.ECS.SubmitContainerStateChange
  ( -- * Creating a Request
    SubmitContainerStateChange (..),
    newSubmitContainerStateChange,

    -- * Request Lenses
    submitContainerStateChange_status,
    submitContainerStateChange_runtimeId,
    submitContainerStateChange_task,
    submitContainerStateChange_exitCode,
    submitContainerStateChange_networkBindings,
    submitContainerStateChange_reason,
    submitContainerStateChange_containerName,
    submitContainerStateChange_cluster,

    -- * Destructuring the Response
    SubmitContainerStateChangeResponse (..),
    newSubmitContainerStateChangeResponse,

    -- * Response Lenses
    submitContainerStateChangeResponse_acknowledgment,
    submitContainerStateChangeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSubmitContainerStateChange' smart constructor.
data SubmitContainerStateChange = SubmitContainerStateChange'
  { -- | The status of the state change request.
    status :: Core.Maybe Core.Text,
    -- | The ID of the Docker container.
    runtimeId :: Core.Maybe Core.Text,
    -- | The task ID or full Amazon Resource Name (ARN) of the task that hosts
    -- the container.
    task :: Core.Maybe Core.Text,
    -- | The exit code returned for the state change request.
    exitCode :: Core.Maybe Core.Int,
    -- | The network bindings of the container.
    networkBindings :: Core.Maybe [NetworkBinding],
    -- | The reason for the state change request.
    reason :: Core.Maybe Core.Text,
    -- | The name of the container.
    containerName :: Core.Maybe Core.Text,
    -- | The short name or full ARN of the cluster that hosts the container.
    cluster :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubmitContainerStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'submitContainerStateChange_status' - The status of the state change request.
--
-- 'runtimeId', 'submitContainerStateChange_runtimeId' - The ID of the Docker container.
--
-- 'task', 'submitContainerStateChange_task' - The task ID or full Amazon Resource Name (ARN) of the task that hosts
-- the container.
--
-- 'exitCode', 'submitContainerStateChange_exitCode' - The exit code returned for the state change request.
--
-- 'networkBindings', 'submitContainerStateChange_networkBindings' - The network bindings of the container.
--
-- 'reason', 'submitContainerStateChange_reason' - The reason for the state change request.
--
-- 'containerName', 'submitContainerStateChange_containerName' - The name of the container.
--
-- 'cluster', 'submitContainerStateChange_cluster' - The short name or full ARN of the cluster that hosts the container.
newSubmitContainerStateChange ::
  SubmitContainerStateChange
newSubmitContainerStateChange =
  SubmitContainerStateChange'
    { status = Core.Nothing,
      runtimeId = Core.Nothing,
      task = Core.Nothing,
      exitCode = Core.Nothing,
      networkBindings = Core.Nothing,
      reason = Core.Nothing,
      containerName = Core.Nothing,
      cluster = Core.Nothing
    }

-- | The status of the state change request.
submitContainerStateChange_status :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
submitContainerStateChange_status = Lens.lens (\SubmitContainerStateChange' {status} -> status) (\s@SubmitContainerStateChange' {} a -> s {status = a} :: SubmitContainerStateChange)

-- | The ID of the Docker container.
submitContainerStateChange_runtimeId :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
submitContainerStateChange_runtimeId = Lens.lens (\SubmitContainerStateChange' {runtimeId} -> runtimeId) (\s@SubmitContainerStateChange' {} a -> s {runtimeId = a} :: SubmitContainerStateChange)

-- | The task ID or full Amazon Resource Name (ARN) of the task that hosts
-- the container.
submitContainerStateChange_task :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
submitContainerStateChange_task = Lens.lens (\SubmitContainerStateChange' {task} -> task) (\s@SubmitContainerStateChange' {} a -> s {task = a} :: SubmitContainerStateChange)

-- | The exit code returned for the state change request.
submitContainerStateChange_exitCode :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Int)
submitContainerStateChange_exitCode = Lens.lens (\SubmitContainerStateChange' {exitCode} -> exitCode) (\s@SubmitContainerStateChange' {} a -> s {exitCode = a} :: SubmitContainerStateChange)

-- | The network bindings of the container.
submitContainerStateChange_networkBindings :: Lens.Lens' SubmitContainerStateChange (Core.Maybe [NetworkBinding])
submitContainerStateChange_networkBindings = Lens.lens (\SubmitContainerStateChange' {networkBindings} -> networkBindings) (\s@SubmitContainerStateChange' {} a -> s {networkBindings = a} :: SubmitContainerStateChange) Core.. Lens.mapping Lens._Coerce

-- | The reason for the state change request.
submitContainerStateChange_reason :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
submitContainerStateChange_reason = Lens.lens (\SubmitContainerStateChange' {reason} -> reason) (\s@SubmitContainerStateChange' {} a -> s {reason = a} :: SubmitContainerStateChange)

-- | The name of the container.
submitContainerStateChange_containerName :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
submitContainerStateChange_containerName = Lens.lens (\SubmitContainerStateChange' {containerName} -> containerName) (\s@SubmitContainerStateChange' {} a -> s {containerName = a} :: SubmitContainerStateChange)

-- | The short name or full ARN of the cluster that hosts the container.
submitContainerStateChange_cluster :: Lens.Lens' SubmitContainerStateChange (Core.Maybe Core.Text)
submitContainerStateChange_cluster = Lens.lens (\SubmitContainerStateChange' {cluster} -> cluster) (\s@SubmitContainerStateChange' {} a -> s {cluster = a} :: SubmitContainerStateChange)

instance Core.AWSRequest SubmitContainerStateChange where
  type
    AWSResponse SubmitContainerStateChange =
      SubmitContainerStateChangeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitContainerStateChangeResponse'
            Core.<$> (x Core..?> "acknowledgment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SubmitContainerStateChange

instance Core.NFData SubmitContainerStateChange

instance Core.ToHeaders SubmitContainerStateChange where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.SubmitContainerStateChange" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SubmitContainerStateChange where
  toJSON SubmitContainerStateChange' {..} =
    Core.object
      ( Core.catMaybes
          [ ("status" Core..=) Core.<$> status,
            ("runtimeId" Core..=) Core.<$> runtimeId,
            ("task" Core..=) Core.<$> task,
            ("exitCode" Core..=) Core.<$> exitCode,
            ("networkBindings" Core..=) Core.<$> networkBindings,
            ("reason" Core..=) Core.<$> reason,
            ("containerName" Core..=) Core.<$> containerName,
            ("cluster" Core..=) Core.<$> cluster
          ]
      )

instance Core.ToPath SubmitContainerStateChange where
  toPath = Core.const "/"

instance Core.ToQuery SubmitContainerStateChange where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSubmitContainerStateChangeResponse' smart constructor.
data SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SubmitContainerStateChangeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acknowledgment', 'submitContainerStateChangeResponse_acknowledgment' - Acknowledgement of the state change.
--
-- 'httpStatus', 'submitContainerStateChangeResponse_httpStatus' - The response's http status code.
newSubmitContainerStateChangeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SubmitContainerStateChangeResponse
newSubmitContainerStateChangeResponse pHttpStatus_ =
  SubmitContainerStateChangeResponse'
    { acknowledgment =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Acknowledgement of the state change.
submitContainerStateChangeResponse_acknowledgment :: Lens.Lens' SubmitContainerStateChangeResponse (Core.Maybe Core.Text)
submitContainerStateChangeResponse_acknowledgment = Lens.lens (\SubmitContainerStateChangeResponse' {acknowledgment} -> acknowledgment) (\s@SubmitContainerStateChangeResponse' {} a -> s {acknowledgment = a} :: SubmitContainerStateChangeResponse)

-- | The response's http status code.
submitContainerStateChangeResponse_httpStatus :: Lens.Lens' SubmitContainerStateChangeResponse Core.Int
submitContainerStateChangeResponse_httpStatus = Lens.lens (\SubmitContainerStateChangeResponse' {httpStatus} -> httpStatus) (\s@SubmitContainerStateChangeResponse' {} a -> s {httpStatus = a} :: SubmitContainerStateChangeResponse)

instance
  Core.NFData
    SubmitContainerStateChangeResponse
