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
    submitContainerStateChange_networkBindings,
    submitContainerStateChange_status,
    submitContainerStateChange_cluster,
    submitContainerStateChange_containerName,
    submitContainerStateChange_reason,
    submitContainerStateChange_exitCode,
    submitContainerStateChange_task,
    submitContainerStateChange_runtimeId,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSubmitContainerStateChange' smart constructor.
data SubmitContainerStateChange = SubmitContainerStateChange'
  { -- | The network bindings of the container.
    networkBindings :: Prelude.Maybe [NetworkBinding],
    -- | The status of the state change request.
    status :: Prelude.Maybe Prelude.Text,
    -- | The short name or full ARN of the cluster that hosts the container.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The name of the container.
    containerName :: Prelude.Maybe Prelude.Text,
    -- | The reason for the state change request.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The exit code returned for the state change request.
    exitCode :: Prelude.Maybe Prelude.Int,
    -- | The task ID or full Amazon Resource Name (ARN) of the task that hosts
    -- the container.
    task :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Docker container.
    runtimeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubmitContainerStateChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkBindings', 'submitContainerStateChange_networkBindings' - The network bindings of the container.
--
-- 'status', 'submitContainerStateChange_status' - The status of the state change request.
--
-- 'cluster', 'submitContainerStateChange_cluster' - The short name or full ARN of the cluster that hosts the container.
--
-- 'containerName', 'submitContainerStateChange_containerName' - The name of the container.
--
-- 'reason', 'submitContainerStateChange_reason' - The reason for the state change request.
--
-- 'exitCode', 'submitContainerStateChange_exitCode' - The exit code returned for the state change request.
--
-- 'task', 'submitContainerStateChange_task' - The task ID or full Amazon Resource Name (ARN) of the task that hosts
-- the container.
--
-- 'runtimeId', 'submitContainerStateChange_runtimeId' - The ID of the Docker container.
newSubmitContainerStateChange ::
  SubmitContainerStateChange
newSubmitContainerStateChange =
  SubmitContainerStateChange'
    { networkBindings =
        Prelude.Nothing,
      status = Prelude.Nothing,
      cluster = Prelude.Nothing,
      containerName = Prelude.Nothing,
      reason = Prelude.Nothing,
      exitCode = Prelude.Nothing,
      task = Prelude.Nothing,
      runtimeId = Prelude.Nothing
    }

-- | The network bindings of the container.
submitContainerStateChange_networkBindings :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe [NetworkBinding])
submitContainerStateChange_networkBindings = Lens.lens (\SubmitContainerStateChange' {networkBindings} -> networkBindings) (\s@SubmitContainerStateChange' {} a -> s {networkBindings = a} :: SubmitContainerStateChange) Prelude.. Lens.mapping Lens.coerced

-- | The status of the state change request.
submitContainerStateChange_status :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe Prelude.Text)
submitContainerStateChange_status = Lens.lens (\SubmitContainerStateChange' {status} -> status) (\s@SubmitContainerStateChange' {} a -> s {status = a} :: SubmitContainerStateChange)

-- | The short name or full ARN of the cluster that hosts the container.
submitContainerStateChange_cluster :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe Prelude.Text)
submitContainerStateChange_cluster = Lens.lens (\SubmitContainerStateChange' {cluster} -> cluster) (\s@SubmitContainerStateChange' {} a -> s {cluster = a} :: SubmitContainerStateChange)

-- | The name of the container.
submitContainerStateChange_containerName :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe Prelude.Text)
submitContainerStateChange_containerName = Lens.lens (\SubmitContainerStateChange' {containerName} -> containerName) (\s@SubmitContainerStateChange' {} a -> s {containerName = a} :: SubmitContainerStateChange)

-- | The reason for the state change request.
submitContainerStateChange_reason :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe Prelude.Text)
submitContainerStateChange_reason = Lens.lens (\SubmitContainerStateChange' {reason} -> reason) (\s@SubmitContainerStateChange' {} a -> s {reason = a} :: SubmitContainerStateChange)

-- | The exit code returned for the state change request.
submitContainerStateChange_exitCode :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe Prelude.Int)
submitContainerStateChange_exitCode = Lens.lens (\SubmitContainerStateChange' {exitCode} -> exitCode) (\s@SubmitContainerStateChange' {} a -> s {exitCode = a} :: SubmitContainerStateChange)

-- | The task ID or full Amazon Resource Name (ARN) of the task that hosts
-- the container.
submitContainerStateChange_task :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe Prelude.Text)
submitContainerStateChange_task = Lens.lens (\SubmitContainerStateChange' {task} -> task) (\s@SubmitContainerStateChange' {} a -> s {task = a} :: SubmitContainerStateChange)

-- | The ID of the Docker container.
submitContainerStateChange_runtimeId :: Lens.Lens' SubmitContainerStateChange (Prelude.Maybe Prelude.Text)
submitContainerStateChange_runtimeId = Lens.lens (\SubmitContainerStateChange' {runtimeId} -> runtimeId) (\s@SubmitContainerStateChange' {} a -> s {runtimeId = a} :: SubmitContainerStateChange)

instance Core.AWSRequest SubmitContainerStateChange where
  type
    AWSResponse SubmitContainerStateChange =
      SubmitContainerStateChangeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitContainerStateChangeResponse'
            Prelude.<$> (x Core..?> "acknowledgment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SubmitContainerStateChange

instance Prelude.NFData SubmitContainerStateChange

instance Core.ToHeaders SubmitContainerStateChange where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.SubmitContainerStateChange" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SubmitContainerStateChange where
  toJSON SubmitContainerStateChange' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("networkBindings" Core..=)
              Prelude.<$> networkBindings,
            ("status" Core..=) Prelude.<$> status,
            ("cluster" Core..=) Prelude.<$> cluster,
            ("containerName" Core..=) Prelude.<$> containerName,
            ("reason" Core..=) Prelude.<$> reason,
            ("exitCode" Core..=) Prelude.<$> exitCode,
            ("task" Core..=) Prelude.<$> task,
            ("runtimeId" Core..=) Prelude.<$> runtimeId
          ]
      )

instance Core.ToPath SubmitContainerStateChange where
  toPath = Prelude.const "/"

instance Core.ToQuery SubmitContainerStateChange where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSubmitContainerStateChangeResponse' smart constructor.
data SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SubmitContainerStateChangeResponse
newSubmitContainerStateChangeResponse pHttpStatus_ =
  SubmitContainerStateChangeResponse'
    { acknowledgment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Acknowledgement of the state change.
submitContainerStateChangeResponse_acknowledgment :: Lens.Lens' SubmitContainerStateChangeResponse (Prelude.Maybe Prelude.Text)
submitContainerStateChangeResponse_acknowledgment = Lens.lens (\SubmitContainerStateChangeResponse' {acknowledgment} -> acknowledgment) (\s@SubmitContainerStateChangeResponse' {} a -> s {acknowledgment = a} :: SubmitContainerStateChangeResponse)

-- | The response's http status code.
submitContainerStateChangeResponse_httpStatus :: Lens.Lens' SubmitContainerStateChangeResponse Prelude.Int
submitContainerStateChangeResponse_httpStatus = Lens.lens (\SubmitContainerStateChangeResponse' {httpStatus} -> httpStatus) (\s@SubmitContainerStateChangeResponse' {} a -> s {httpStatus = a} :: SubmitContainerStateChangeResponse)

instance
  Prelude.NFData
    SubmitContainerStateChangeResponse
