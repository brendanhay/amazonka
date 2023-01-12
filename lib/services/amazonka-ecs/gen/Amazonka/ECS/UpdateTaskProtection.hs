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
-- Module      : Amazonka.ECS.UpdateTaskProtection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the protection status of a task. You can set @protectionEnabled@
-- to @true@ to protect your task from termination during scale-in events
-- from
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-auto-scaling.html Service Autoscaling>
-- or
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html deployments>.
--
-- Task-protection, by default, expires after 2 hours at which point Amazon
-- ECS unsets the @protectionEnabled@ property making the task eligible for
-- termination by a subsequent scale-in event.
--
-- You can specify a custom expiration period for task protection from 1
-- minute to up to 2,880 minutes (48 hours). To specify the custom
-- expiration period, set the @expiresInMinutes@ property. The
-- @expiresInMinutes@ property is always reset when you invoke this
-- operation for a task that already has @protectionEnabled@ set to @true@.
-- You can keep extending the protection expiration period of a task by
-- invoking this operation repeatedly.
--
-- To learn more about Amazon ECS task protection, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-scale-in-protection.html Task scale-in protection>
-- in the //Amazon Elastic Container Service Developer Guide// .
--
-- This operation is only supported for tasks belonging to an Amazon ECS
-- service. Invoking this operation for a standalone task will result in an
-- @TASK_NOT_VALID@ failure. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html API failure reasons>.
--
-- If you prefer to set task protection from within the container, we
-- recommend using the
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/task-scale-in-protection-endpoint.html Task scale-in protection endpoint>.
module Amazonka.ECS.UpdateTaskProtection
  ( -- * Creating a Request
    UpdateTaskProtection (..),
    newUpdateTaskProtection,

    -- * Request Lenses
    updateTaskProtection_expiresInMinutes,
    updateTaskProtection_cluster,
    updateTaskProtection_tasks,
    updateTaskProtection_protectionEnabled,

    -- * Destructuring the Response
    UpdateTaskProtectionResponse (..),
    newUpdateTaskProtectionResponse,

    -- * Response Lenses
    updateTaskProtectionResponse_failures,
    updateTaskProtectionResponse_protectedTasks,
    updateTaskProtectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTaskProtection' smart constructor.
data UpdateTaskProtection = UpdateTaskProtection'
  { -- | If you set @protectionEnabled@ to @true@, you can specify the duration
    -- for task protection in minutes. You can specify a value from 1 minute to
    -- up to 2,880 minutes (48 hours). During this time, your task will not be
    -- terminated by scale-in events from Service Auto Scaling or deployments.
    -- After this time period lapses, @protectionEnabled@ will be reset to
    -- @false@.
    --
    -- If you don’t specify the time, then the task is automatically protected
    -- for 120 minutes (2 hours).
    expiresInMinutes :: Prelude.Maybe Prelude.Int,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task sets exist in.
    cluster :: Prelude.Text,
    -- | A list of up to 10 task IDs or full ARN entries.
    tasks :: [Prelude.Text],
    -- | Specify @true@ to mark a task for protection and @false@ to unset
    -- protection, making it eligible for termination.
    protectionEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaskProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiresInMinutes', 'updateTaskProtection_expiresInMinutes' - If you set @protectionEnabled@ to @true@, you can specify the duration
-- for task protection in minutes. You can specify a value from 1 minute to
-- up to 2,880 minutes (48 hours). During this time, your task will not be
-- terminated by scale-in events from Service Auto Scaling or deployments.
-- After this time period lapses, @protectionEnabled@ will be reset to
-- @false@.
--
-- If you don’t specify the time, then the task is automatically protected
-- for 120 minutes (2 hours).
--
-- 'cluster', 'updateTaskProtection_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
--
-- 'tasks', 'updateTaskProtection_tasks' - A list of up to 10 task IDs or full ARN entries.
--
-- 'protectionEnabled', 'updateTaskProtection_protectionEnabled' - Specify @true@ to mark a task for protection and @false@ to unset
-- protection, making it eligible for termination.
newUpdateTaskProtection ::
  -- | 'cluster'
  Prelude.Text ->
  -- | 'protectionEnabled'
  Prelude.Bool ->
  UpdateTaskProtection
newUpdateTaskProtection pCluster_ pProtectionEnabled_ =
  UpdateTaskProtection'
    { expiresInMinutes =
        Prelude.Nothing,
      cluster = pCluster_,
      tasks = Prelude.mempty,
      protectionEnabled = pProtectionEnabled_
    }

-- | If you set @protectionEnabled@ to @true@, you can specify the duration
-- for task protection in minutes. You can specify a value from 1 minute to
-- up to 2,880 minutes (48 hours). During this time, your task will not be
-- terminated by scale-in events from Service Auto Scaling or deployments.
-- After this time period lapses, @protectionEnabled@ will be reset to
-- @false@.
--
-- If you don’t specify the time, then the task is automatically protected
-- for 120 minutes (2 hours).
updateTaskProtection_expiresInMinutes :: Lens.Lens' UpdateTaskProtection (Prelude.Maybe Prelude.Int)
updateTaskProtection_expiresInMinutes = Lens.lens (\UpdateTaskProtection' {expiresInMinutes} -> expiresInMinutes) (\s@UpdateTaskProtection' {} a -> s {expiresInMinutes = a} :: UpdateTaskProtection)

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
updateTaskProtection_cluster :: Lens.Lens' UpdateTaskProtection Prelude.Text
updateTaskProtection_cluster = Lens.lens (\UpdateTaskProtection' {cluster} -> cluster) (\s@UpdateTaskProtection' {} a -> s {cluster = a} :: UpdateTaskProtection)

-- | A list of up to 10 task IDs or full ARN entries.
updateTaskProtection_tasks :: Lens.Lens' UpdateTaskProtection [Prelude.Text]
updateTaskProtection_tasks = Lens.lens (\UpdateTaskProtection' {tasks} -> tasks) (\s@UpdateTaskProtection' {} a -> s {tasks = a} :: UpdateTaskProtection) Prelude.. Lens.coerced

-- | Specify @true@ to mark a task for protection and @false@ to unset
-- protection, making it eligible for termination.
updateTaskProtection_protectionEnabled :: Lens.Lens' UpdateTaskProtection Prelude.Bool
updateTaskProtection_protectionEnabled = Lens.lens (\UpdateTaskProtection' {protectionEnabled} -> protectionEnabled) (\s@UpdateTaskProtection' {} a -> s {protectionEnabled = a} :: UpdateTaskProtection)

instance Core.AWSRequest UpdateTaskProtection where
  type
    AWSResponse UpdateTaskProtection =
      UpdateTaskProtectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateTaskProtectionResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "protectedTasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTaskProtection where
  hashWithSalt _salt UpdateTaskProtection' {..} =
    _salt `Prelude.hashWithSalt` expiresInMinutes
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` tasks
      `Prelude.hashWithSalt` protectionEnabled

instance Prelude.NFData UpdateTaskProtection where
  rnf UpdateTaskProtection' {..} =
    Prelude.rnf expiresInMinutes
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf tasks
      `Prelude.seq` Prelude.rnf protectionEnabled

instance Data.ToHeaders UpdateTaskProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateTaskProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateTaskProtection where
  toJSON UpdateTaskProtection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("expiresInMinutes" Data..=)
              Prelude.<$> expiresInMinutes,
            Prelude.Just ("cluster" Data..= cluster),
            Prelude.Just ("tasks" Data..= tasks),
            Prelude.Just
              ("protectionEnabled" Data..= protectionEnabled)
          ]
      )

instance Data.ToPath UpdateTaskProtection where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateTaskProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTaskProtectionResponse' smart constructor.
data UpdateTaskProtectionResponse = UpdateTaskProtectionResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | A list of tasks with the following information.
    --
    -- -   @taskArn@: The task ARN.
    --
    -- -   @protectionEnabled@: The protection status of the task. If scale-in
    --     protection is enabled for a task, the value is @true@. Otherwise, it
    --     is @false@.
    --
    -- -   @expirationDate@: The epoch time when protection for the task will
    --     expire.
    protectedTasks :: Prelude.Maybe [ProtectedTask],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTaskProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'updateTaskProtectionResponse_failures' - Any failures associated with the call.
--
-- 'protectedTasks', 'updateTaskProtectionResponse_protectedTasks' - A list of tasks with the following information.
--
-- -   @taskArn@: The task ARN.
--
-- -   @protectionEnabled@: The protection status of the task. If scale-in
--     protection is enabled for a task, the value is @true@. Otherwise, it
--     is @false@.
--
-- -   @expirationDate@: The epoch time when protection for the task will
--     expire.
--
-- 'httpStatus', 'updateTaskProtectionResponse_httpStatus' - The response's http status code.
newUpdateTaskProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTaskProtectionResponse
newUpdateTaskProtectionResponse pHttpStatus_ =
  UpdateTaskProtectionResponse'
    { failures =
        Prelude.Nothing,
      protectedTasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
updateTaskProtectionResponse_failures :: Lens.Lens' UpdateTaskProtectionResponse (Prelude.Maybe [Failure])
updateTaskProtectionResponse_failures = Lens.lens (\UpdateTaskProtectionResponse' {failures} -> failures) (\s@UpdateTaskProtectionResponse' {} a -> s {failures = a} :: UpdateTaskProtectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of tasks with the following information.
--
-- -   @taskArn@: The task ARN.
--
-- -   @protectionEnabled@: The protection status of the task. If scale-in
--     protection is enabled for a task, the value is @true@. Otherwise, it
--     is @false@.
--
-- -   @expirationDate@: The epoch time when protection for the task will
--     expire.
updateTaskProtectionResponse_protectedTasks :: Lens.Lens' UpdateTaskProtectionResponse (Prelude.Maybe [ProtectedTask])
updateTaskProtectionResponse_protectedTasks = Lens.lens (\UpdateTaskProtectionResponse' {protectedTasks} -> protectedTasks) (\s@UpdateTaskProtectionResponse' {} a -> s {protectedTasks = a} :: UpdateTaskProtectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateTaskProtectionResponse_httpStatus :: Lens.Lens' UpdateTaskProtectionResponse Prelude.Int
updateTaskProtectionResponse_httpStatus = Lens.lens (\UpdateTaskProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateTaskProtectionResponse' {} a -> s {httpStatus = a} :: UpdateTaskProtectionResponse)

instance Prelude.NFData UpdateTaskProtectionResponse where
  rnf UpdateTaskProtectionResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf protectedTasks
      `Prelude.seq` Prelude.rnf httpStatus
