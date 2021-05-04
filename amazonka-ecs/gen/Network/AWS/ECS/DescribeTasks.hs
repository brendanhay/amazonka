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

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTasks' smart constructor.
data DescribeTasks = DescribeTasks'
  { -- | Specifies whether you want to see the resource tags for the task. If
    -- @TAGS@ is specified, the tags are included in the response. If this
    -- field is omitted, tags are not included in the response.
    include :: Prelude.Maybe [TaskField],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the task or tasks to describe. If you do not specify a cluster,
    -- the default cluster is assumed. This parameter is required if the task
    -- or tasks you are describing were launched in any cluster other than the
    -- default cluster.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 100 task IDs or full ARN entries.
    tasks :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { include = Prelude.Nothing,
      cluster = Prelude.Nothing,
      tasks = Prelude.mempty
    }

-- | Specifies whether you want to see the resource tags for the task. If
-- @TAGS@ is specified, the tags are included in the response. If this
-- field is omitted, tags are not included in the response.
describeTasks_include :: Lens.Lens' DescribeTasks (Prelude.Maybe [TaskField])
describeTasks_include = Lens.lens (\DescribeTasks' {include} -> include) (\s@DescribeTasks' {} a -> s {include = a} :: DescribeTasks) Prelude.. Lens.mapping Prelude._Coerce

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the task or tasks to describe. If you do not specify a cluster,
-- the default cluster is assumed. This parameter is required if the task
-- or tasks you are describing were launched in any cluster other than the
-- default cluster.
describeTasks_cluster :: Lens.Lens' DescribeTasks (Prelude.Maybe Prelude.Text)
describeTasks_cluster = Lens.lens (\DescribeTasks' {cluster} -> cluster) (\s@DescribeTasks' {} a -> s {cluster = a} :: DescribeTasks)

-- | A list of up to 100 task IDs or full ARN entries.
describeTasks_tasks :: Lens.Lens' DescribeTasks [Prelude.Text]
describeTasks_tasks = Lens.lens (\DescribeTasks' {tasks} -> tasks) (\s@DescribeTasks' {} a -> s {tasks = a} :: DescribeTasks) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DescribeTasks where
  type Rs DescribeTasks = DescribeTasksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTasksResponse'
            Prelude.<$> (x Prelude..?> "tasks" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "failures" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTasks

instance Prelude.NFData DescribeTasks

instance Prelude.ToHeaders DescribeTasks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTasks" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeTasks where
  toJSON DescribeTasks' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("include" Prelude..=) Prelude.<$> include,
            ("cluster" Prelude..=) Prelude.<$> cluster,
            Prelude.Just ("tasks" Prelude..= tasks)
          ]
      )

instance Prelude.ToPath DescribeTasks where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTasks where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTasksResponse' smart constructor.
data DescribeTasksResponse = DescribeTasksResponse'
  { -- | The list of tasks.
    tasks :: Prelude.Maybe [Task],
    -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeTasksResponse
newDescribeTasksResponse pHttpStatus_ =
  DescribeTasksResponse'
    { tasks = Prelude.Nothing,
      failures = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of tasks.
describeTasksResponse_tasks :: Lens.Lens' DescribeTasksResponse (Prelude.Maybe [Task])
describeTasksResponse_tasks = Lens.lens (\DescribeTasksResponse' {tasks} -> tasks) (\s@DescribeTasksResponse' {} a -> s {tasks = a} :: DescribeTasksResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Any failures associated with the call.
describeTasksResponse_failures :: Lens.Lens' DescribeTasksResponse (Prelude.Maybe [Failure])
describeTasksResponse_failures = Lens.lens (\DescribeTasksResponse' {failures} -> failures) (\s@DescribeTasksResponse' {} a -> s {failures = a} :: DescribeTasksResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeTasksResponse_httpStatus :: Lens.Lens' DescribeTasksResponse Prelude.Int
describeTasksResponse_httpStatus = Lens.lens (\DescribeTasksResponse' {httpStatus} -> httpStatus) (\s@DescribeTasksResponse' {} a -> s {httpStatus = a} :: DescribeTasksResponse)

instance Prelude.NFData DescribeTasksResponse
