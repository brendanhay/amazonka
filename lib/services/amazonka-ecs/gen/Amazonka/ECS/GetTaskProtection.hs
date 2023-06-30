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
-- Module      : Amazonka.ECS.GetTaskProtection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the protection status of tasks in an Amazon ECS service.
module Amazonka.ECS.GetTaskProtection
  ( -- * Creating a Request
    GetTaskProtection (..),
    newGetTaskProtection,

    -- * Request Lenses
    getTaskProtection_tasks,
    getTaskProtection_cluster,

    -- * Destructuring the Response
    GetTaskProtectionResponse (..),
    newGetTaskProtectionResponse,

    -- * Response Lenses
    getTaskProtectionResponse_failures,
    getTaskProtectionResponse_protectedTasks,
    getTaskProtectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTaskProtection' smart constructor.
data GetTaskProtection = GetTaskProtection'
  { -- | A list of up to 100 task IDs or full ARN entries.
    tasks :: Prelude.Maybe [Prelude.Text],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task sets exist in.
    cluster :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTaskProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tasks', 'getTaskProtection_tasks' - A list of up to 100 task IDs or full ARN entries.
--
-- 'cluster', 'getTaskProtection_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
newGetTaskProtection ::
  -- | 'cluster'
  Prelude.Text ->
  GetTaskProtection
newGetTaskProtection pCluster_ =
  GetTaskProtection'
    { tasks = Prelude.Nothing,
      cluster = pCluster_
    }

-- | A list of up to 100 task IDs or full ARN entries.
getTaskProtection_tasks :: Lens.Lens' GetTaskProtection (Prelude.Maybe [Prelude.Text])
getTaskProtection_tasks = Lens.lens (\GetTaskProtection' {tasks} -> tasks) (\s@GetTaskProtection' {} a -> s {tasks = a} :: GetTaskProtection) Prelude.. Lens.mapping Lens.coerced

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
getTaskProtection_cluster :: Lens.Lens' GetTaskProtection Prelude.Text
getTaskProtection_cluster = Lens.lens (\GetTaskProtection' {cluster} -> cluster) (\s@GetTaskProtection' {} a -> s {cluster = a} :: GetTaskProtection)

instance Core.AWSRequest GetTaskProtection where
  type
    AWSResponse GetTaskProtection =
      GetTaskProtectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTaskProtectionResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "protectedTasks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTaskProtection where
  hashWithSalt _salt GetTaskProtection' {..} =
    _salt
      `Prelude.hashWithSalt` tasks
      `Prelude.hashWithSalt` cluster

instance Prelude.NFData GetTaskProtection where
  rnf GetTaskProtection' {..} =
    Prelude.rnf tasks `Prelude.seq` Prelude.rnf cluster

instance Data.ToHeaders GetTaskProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.GetTaskProtection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetTaskProtection where
  toJSON GetTaskProtection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tasks" Data..=) Prelude.<$> tasks,
            Prelude.Just ("cluster" Data..= cluster)
          ]
      )

instance Data.ToPath GetTaskProtection where
  toPath = Prelude.const "/"

instance Data.ToQuery GetTaskProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTaskProtectionResponse' smart constructor.
data GetTaskProtectionResponse = GetTaskProtectionResponse'
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
-- Create a value of 'GetTaskProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'getTaskProtectionResponse_failures' - Any failures associated with the call.
--
-- 'protectedTasks', 'getTaskProtectionResponse_protectedTasks' - A list of tasks with the following information.
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
-- 'httpStatus', 'getTaskProtectionResponse_httpStatus' - The response's http status code.
newGetTaskProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTaskProtectionResponse
newGetTaskProtectionResponse pHttpStatus_ =
  GetTaskProtectionResponse'
    { failures =
        Prelude.Nothing,
      protectedTasks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
getTaskProtectionResponse_failures :: Lens.Lens' GetTaskProtectionResponse (Prelude.Maybe [Failure])
getTaskProtectionResponse_failures = Lens.lens (\GetTaskProtectionResponse' {failures} -> failures) (\s@GetTaskProtectionResponse' {} a -> s {failures = a} :: GetTaskProtectionResponse) Prelude.. Lens.mapping Lens.coerced

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
getTaskProtectionResponse_protectedTasks :: Lens.Lens' GetTaskProtectionResponse (Prelude.Maybe [ProtectedTask])
getTaskProtectionResponse_protectedTasks = Lens.lens (\GetTaskProtectionResponse' {protectedTasks} -> protectedTasks) (\s@GetTaskProtectionResponse' {} a -> s {protectedTasks = a} :: GetTaskProtectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTaskProtectionResponse_httpStatus :: Lens.Lens' GetTaskProtectionResponse Prelude.Int
getTaskProtectionResponse_httpStatus = Lens.lens (\GetTaskProtectionResponse' {httpStatus} -> httpStatus) (\s@GetTaskProtectionResponse' {} a -> s {httpStatus = a} :: GetTaskProtectionResponse)

instance Prelude.NFData GetTaskProtectionResponse where
  rnf GetTaskProtectionResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf protectedTasks
      `Prelude.seq` Prelude.rnf httpStatus
