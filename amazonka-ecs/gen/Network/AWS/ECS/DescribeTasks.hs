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
-- Module      : Network.AWS.ECS.DescribeTasks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specified task or tasks.
module Network.AWS.ECS.DescribeTasks
  ( -- * Creating a Request
    DescribeTasks (..),
    newDescribeTasks,

    -- * Request Lenses
    describeTasks_include,
    describeTasks_cluster,
    describeTasks_tasks,

    -- * Destructuring the Response
    DescribeTasksResponse (..),
    newDescribeTasksResponse,

    -- * Response Lenses
    describeTasksResponse_tasks,
    describeTasksResponse_failures,
    describeTasksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTasks' smart constructor.
data DescribeTasks = DescribeTasks'
  { -- | Specifies whether you want to see the resource tags for the task. If
    -- @TAGS@ is specified, the tags are included in the response. If this
    -- field is omitted, tags are not included in the response.
    include :: Core.Maybe [TaskField],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the task or tasks to describe. If you do not specify a cluster,
    -- the default cluster is assumed. This parameter is required if the task
    -- or tasks you are describing were launched in any cluster other than the
    -- default cluster.
    cluster :: Core.Maybe Core.Text,
    -- | A list of up to 100 task IDs or full ARN entries.
    tasks :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTasks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeTasks_include' - Specifies whether you want to see the resource tags for the task. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags are not included in the response.
--
-- 'cluster', 'describeTasks_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task or tasks to describe. If you do not specify a cluster,
-- the default cluster is assumed. This parameter is required if the task
-- or tasks you are describing were launched in any cluster other than the
-- default cluster.
--
-- 'tasks', 'describeTasks_tasks' - A list of up to 100 task IDs or full ARN entries.
newDescribeTasks ::
  DescribeTasks
newDescribeTasks =
  DescribeTasks'
    { include = Core.Nothing,
      cluster = Core.Nothing,
      tasks = Core.mempty
    }

-- | Specifies whether you want to see the resource tags for the task. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags are not included in the response.
describeTasks_include :: Lens.Lens' DescribeTasks (Core.Maybe [TaskField])
describeTasks_include = Lens.lens (\DescribeTasks' {include} -> include) (\s@DescribeTasks' {} a -> s {include = a} :: DescribeTasks) Core.. Lens.mapping Lens._Coerce

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task or tasks to describe. If you do not specify a cluster,
-- the default cluster is assumed. This parameter is required if the task
-- or tasks you are describing were launched in any cluster other than the
-- default cluster.
describeTasks_cluster :: Lens.Lens' DescribeTasks (Core.Maybe Core.Text)
describeTasks_cluster = Lens.lens (\DescribeTasks' {cluster} -> cluster) (\s@DescribeTasks' {} a -> s {cluster = a} :: DescribeTasks)

-- | A list of up to 100 task IDs or full ARN entries.
describeTasks_tasks :: Lens.Lens' DescribeTasks [Core.Text]
describeTasks_tasks = Lens.lens (\DescribeTasks' {tasks} -> tasks) (\s@DescribeTasks' {} a -> s {tasks = a} :: DescribeTasks) Core.. Lens._Coerce

instance Core.AWSRequest DescribeTasks where
  type
    AWSResponse DescribeTasks =
      DescribeTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTasksResponse'
            Core.<$> (x Core..?> "tasks" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failures" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTasks

instance Core.NFData DescribeTasks

instance Core.ToHeaders DescribeTasks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTasks" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTasks where
  toJSON DescribeTasks' {..} =
    Core.object
      ( Core.catMaybes
          [ ("include" Core..=) Core.<$> include,
            ("cluster" Core..=) Core.<$> cluster,
            Core.Just ("tasks" Core..= tasks)
          ]
      )

instance Core.ToPath DescribeTasks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTasks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTasksResponse' smart constructor.
data DescribeTasksResponse = DescribeTasksResponse'
  { -- | The list of tasks.
    tasks :: Core.Maybe [Task],
    -- | Any failures associated with the call.
    failures :: Core.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTasksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tasks', 'describeTasksResponse_tasks' - The list of tasks.
--
-- 'failures', 'describeTasksResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'describeTasksResponse_httpStatus' - The response's http status code.
newDescribeTasksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTasksResponse
newDescribeTasksResponse pHttpStatus_ =
  DescribeTasksResponse'
    { tasks = Core.Nothing,
      failures = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of tasks.
describeTasksResponse_tasks :: Lens.Lens' DescribeTasksResponse (Core.Maybe [Task])
describeTasksResponse_tasks = Lens.lens (\DescribeTasksResponse' {tasks} -> tasks) (\s@DescribeTasksResponse' {} a -> s {tasks = a} :: DescribeTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | Any failures associated with the call.
describeTasksResponse_failures :: Lens.Lens' DescribeTasksResponse (Core.Maybe [Failure])
describeTasksResponse_failures = Lens.lens (\DescribeTasksResponse' {failures} -> failures) (\s@DescribeTasksResponse' {} a -> s {failures = a} :: DescribeTasksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTasksResponse_httpStatus :: Lens.Lens' DescribeTasksResponse Core.Int
describeTasksResponse_httpStatus = Lens.lens (\DescribeTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeTasksResponse' {} a -> s {httpStatus = a} :: DescribeTasksResponse)

instance Core.NFData DescribeTasksResponse
