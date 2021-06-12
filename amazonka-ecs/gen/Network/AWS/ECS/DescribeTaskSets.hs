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
-- Module      : Network.AWS.ECS.DescribeTaskSets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the task sets in the specified cluster and service. This is
-- used when a service uses the @EXTERNAL@ deployment controller type. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types>
-- in the /Amazon Elastic Container Service Developer Guide/.
module Network.AWS.ECS.DescribeTaskSets
  ( -- * Creating a Request
    DescribeTaskSets (..),
    newDescribeTaskSets,

    -- * Request Lenses
    describeTaskSets_taskSets,
    describeTaskSets_include,
    describeTaskSets_cluster,
    describeTaskSets_service,

    -- * Destructuring the Response
    DescribeTaskSetsResponse (..),
    newDescribeTaskSetsResponse,

    -- * Response Lenses
    describeTaskSetsResponse_taskSets,
    describeTaskSetsResponse_failures,
    describeTaskSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTaskSets' smart constructor.
data DescribeTaskSets = DescribeTaskSets'
  { -- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
    taskSets :: Core.Maybe [Core.Text],
    -- | Specifies whether to see the resource tags for the task set. If @TAGS@
    -- is specified, the tags are included in the response. If this field is
    -- omitted, tags are not included in the response.
    include :: Core.Maybe [TaskSetField],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task sets exist in.
    cluster :: Core.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that
    -- the task sets exist in.
    service :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTaskSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSets', 'describeTaskSets_taskSets' - The ID or full Amazon Resource Name (ARN) of task sets to describe.
--
-- 'include', 'describeTaskSets_include' - Specifies whether to see the resource tags for the task set. If @TAGS@
-- is specified, the tags are included in the response. If this field is
-- omitted, tags are not included in the response.
--
-- 'cluster', 'describeTaskSets_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
--
-- 'service', 'describeTaskSets_service' - The short name or full Amazon Resource Name (ARN) of the service that
-- the task sets exist in.
newDescribeTaskSets ::
  -- | 'cluster'
  Core.Text ->
  -- | 'service'
  Core.Text ->
  DescribeTaskSets
newDescribeTaskSets pCluster_ pService_ =
  DescribeTaskSets'
    { taskSets = Core.Nothing,
      include = Core.Nothing,
      cluster = pCluster_,
      service = pService_
    }

-- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
describeTaskSets_taskSets :: Lens.Lens' DescribeTaskSets (Core.Maybe [Core.Text])
describeTaskSets_taskSets = Lens.lens (\DescribeTaskSets' {taskSets} -> taskSets) (\s@DescribeTaskSets' {} a -> s {taskSets = a} :: DescribeTaskSets) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether to see the resource tags for the task set. If @TAGS@
-- is specified, the tags are included in the response. If this field is
-- omitted, tags are not included in the response.
describeTaskSets_include :: Lens.Lens' DescribeTaskSets (Core.Maybe [TaskSetField])
describeTaskSets_include = Lens.lens (\DescribeTaskSets' {include} -> include) (\s@DescribeTaskSets' {} a -> s {include = a} :: DescribeTaskSets) Core.. Lens.mapping Lens._Coerce

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
describeTaskSets_cluster :: Lens.Lens' DescribeTaskSets Core.Text
describeTaskSets_cluster = Lens.lens (\DescribeTaskSets' {cluster} -> cluster) (\s@DescribeTaskSets' {} a -> s {cluster = a} :: DescribeTaskSets)

-- | The short name or full Amazon Resource Name (ARN) of the service that
-- the task sets exist in.
describeTaskSets_service :: Lens.Lens' DescribeTaskSets Core.Text
describeTaskSets_service = Lens.lens (\DescribeTaskSets' {service} -> service) (\s@DescribeTaskSets' {} a -> s {service = a} :: DescribeTaskSets)

instance Core.AWSRequest DescribeTaskSets where
  type
    AWSResponse DescribeTaskSets =
      DescribeTaskSetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskSetsResponse'
            Core.<$> (x Core..?> "taskSets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "failures" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTaskSets

instance Core.NFData DescribeTaskSets

instance Core.ToHeaders DescribeTaskSets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTaskSets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTaskSets where
  toJSON DescribeTaskSets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("taskSets" Core..=) Core.<$> taskSets,
            ("include" Core..=) Core.<$> include,
            Core.Just ("cluster" Core..= cluster),
            Core.Just ("service" Core..= service)
          ]
      )

instance Core.ToPath DescribeTaskSets where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTaskSets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTaskSetsResponse' smart constructor.
data DescribeTaskSetsResponse = DescribeTaskSetsResponse'
  { -- | The list of task sets described.
    taskSets :: Core.Maybe [TaskSet],
    -- | Any failures associated with the call.
    failures :: Core.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTaskSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskSets', 'describeTaskSetsResponse_taskSets' - The list of task sets described.
--
-- 'failures', 'describeTaskSetsResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'describeTaskSetsResponse_httpStatus' - The response's http status code.
newDescribeTaskSetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTaskSetsResponse
newDescribeTaskSetsResponse pHttpStatus_ =
  DescribeTaskSetsResponse'
    { taskSets = Core.Nothing,
      failures = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of task sets described.
describeTaskSetsResponse_taskSets :: Lens.Lens' DescribeTaskSetsResponse (Core.Maybe [TaskSet])
describeTaskSetsResponse_taskSets = Lens.lens (\DescribeTaskSetsResponse' {taskSets} -> taskSets) (\s@DescribeTaskSetsResponse' {} a -> s {taskSets = a} :: DescribeTaskSetsResponse) Core.. Lens.mapping Lens._Coerce

-- | Any failures associated with the call.
describeTaskSetsResponse_failures :: Lens.Lens' DescribeTaskSetsResponse (Core.Maybe [Failure])
describeTaskSetsResponse_failures = Lens.lens (\DescribeTaskSetsResponse' {failures} -> failures) (\s@DescribeTaskSetsResponse' {} a -> s {failures = a} :: DescribeTaskSetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTaskSetsResponse_httpStatus :: Lens.Lens' DescribeTaskSetsResponse Core.Int
describeTaskSetsResponse_httpStatus = Lens.lens (\DescribeTaskSetsResponse' {httpStatus} -> httpStatus) (\s@DescribeTaskSetsResponse' {} a -> s {httpStatus = a} :: DescribeTaskSetsResponse)

instance Core.NFData DescribeTaskSetsResponse
