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
-- Module      : Amazonka.ECS.DescribeTaskSets
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ECS.DescribeTaskSets
  ( -- * Creating a Request
    DescribeTaskSets (..),
    newDescribeTaskSets,

    -- * Request Lenses
    describeTaskSets_include,
    describeTaskSets_taskSets,
    describeTaskSets_cluster,
    describeTaskSets_service,

    -- * Destructuring the Response
    DescribeTaskSetsResponse (..),
    newDescribeTaskSetsResponse,

    -- * Response Lenses
    describeTaskSetsResponse_failures,
    describeTaskSetsResponse_taskSets,
    describeTaskSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTaskSets' smart constructor.
data DescribeTaskSets = DescribeTaskSets'
  { -- | Specifies whether to see the resource tags for the task set. If @TAGS@
    -- is specified, the tags are included in the response. If this field is
    -- omitted, tags aren\'t included in the response.
    include :: Prelude.Maybe [TaskSetField],
    -- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
    taskSets :: Prelude.Maybe [Prelude.Text],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the service that the task sets exist in.
    cluster :: Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that
    -- the task sets exist in.
    service :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTaskSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'include', 'describeTaskSets_include' - Specifies whether to see the resource tags for the task set. If @TAGS@
-- is specified, the tags are included in the response. If this field is
-- omitted, tags aren\'t included in the response.
--
-- 'taskSets', 'describeTaskSets_taskSets' - The ID or full Amazon Resource Name (ARN) of task sets to describe.
--
-- 'cluster', 'describeTaskSets_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
--
-- 'service', 'describeTaskSets_service' - The short name or full Amazon Resource Name (ARN) of the service that
-- the task sets exist in.
newDescribeTaskSets ::
  -- | 'cluster'
  Prelude.Text ->
  -- | 'service'
  Prelude.Text ->
  DescribeTaskSets
newDescribeTaskSets pCluster_ pService_ =
  DescribeTaskSets'
    { include = Prelude.Nothing,
      taskSets = Prelude.Nothing,
      cluster = pCluster_,
      service = pService_
    }

-- | Specifies whether to see the resource tags for the task set. If @TAGS@
-- is specified, the tags are included in the response. If this field is
-- omitted, tags aren\'t included in the response.
describeTaskSets_include :: Lens.Lens' DescribeTaskSets (Prelude.Maybe [TaskSetField])
describeTaskSets_include = Lens.lens (\DescribeTaskSets' {include} -> include) (\s@DescribeTaskSets' {} a -> s {include = a} :: DescribeTaskSets) Prelude.. Lens.mapping Lens.coerced

-- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
describeTaskSets_taskSets :: Lens.Lens' DescribeTaskSets (Prelude.Maybe [Prelude.Text])
describeTaskSets_taskSets = Lens.lens (\DescribeTaskSets' {taskSets} -> taskSets) (\s@DescribeTaskSets' {} a -> s {taskSets = a} :: DescribeTaskSets) Prelude.. Lens.mapping Lens.coerced

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the service that the task sets exist in.
describeTaskSets_cluster :: Lens.Lens' DescribeTaskSets Prelude.Text
describeTaskSets_cluster = Lens.lens (\DescribeTaskSets' {cluster} -> cluster) (\s@DescribeTaskSets' {} a -> s {cluster = a} :: DescribeTaskSets)

-- | The short name or full Amazon Resource Name (ARN) of the service that
-- the task sets exist in.
describeTaskSets_service :: Lens.Lens' DescribeTaskSets Prelude.Text
describeTaskSets_service = Lens.lens (\DescribeTaskSets' {service} -> service) (\s@DescribeTaskSets' {} a -> s {service = a} :: DescribeTaskSets)

instance Core.AWSRequest DescribeTaskSets where
  type
    AWSResponse DescribeTaskSets =
      DescribeTaskSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTaskSetsResponse'
            Prelude.<$> (x Data..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "taskSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTaskSets where
  hashWithSalt _salt DescribeTaskSets' {..} =
    _salt
      `Prelude.hashWithSalt` include
      `Prelude.hashWithSalt` taskSets
      `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` service

instance Prelude.NFData DescribeTaskSets where
  rnf DescribeTaskSets' {..} =
    Prelude.rnf include
      `Prelude.seq` Prelude.rnf taskSets
      `Prelude.seq` Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf service

instance Data.ToHeaders DescribeTaskSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTaskSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTaskSets where
  toJSON DescribeTaskSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("include" Data..=) Prelude.<$> include,
            ("taskSets" Data..=) Prelude.<$> taskSets,
            Prelude.Just ("cluster" Data..= cluster),
            Prelude.Just ("service" Data..= service)
          ]
      )

instance Data.ToPath DescribeTaskSets where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTaskSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTaskSetsResponse' smart constructor.
data DescribeTaskSetsResponse = DescribeTaskSetsResponse'
  { -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The list of task sets described.
    taskSets :: Prelude.Maybe [TaskSet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTaskSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failures', 'describeTaskSetsResponse_failures' - Any failures associated with the call.
--
-- 'taskSets', 'describeTaskSetsResponse_taskSets' - The list of task sets described.
--
-- 'httpStatus', 'describeTaskSetsResponse_httpStatus' - The response's http status code.
newDescribeTaskSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTaskSetsResponse
newDescribeTaskSetsResponse pHttpStatus_ =
  DescribeTaskSetsResponse'
    { failures =
        Prelude.Nothing,
      taskSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Any failures associated with the call.
describeTaskSetsResponse_failures :: Lens.Lens' DescribeTaskSetsResponse (Prelude.Maybe [Failure])
describeTaskSetsResponse_failures = Lens.lens (\DescribeTaskSetsResponse' {failures} -> failures) (\s@DescribeTaskSetsResponse' {} a -> s {failures = a} :: DescribeTaskSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of task sets described.
describeTaskSetsResponse_taskSets :: Lens.Lens' DescribeTaskSetsResponse (Prelude.Maybe [TaskSet])
describeTaskSetsResponse_taskSets = Lens.lens (\DescribeTaskSetsResponse' {taskSets} -> taskSets) (\s@DescribeTaskSetsResponse' {} a -> s {taskSets = a} :: DescribeTaskSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeTaskSetsResponse_httpStatus :: Lens.Lens' DescribeTaskSetsResponse Prelude.Int
describeTaskSetsResponse_httpStatus = Lens.lens (\DescribeTaskSetsResponse' {httpStatus} -> httpStatus) (\s@DescribeTaskSetsResponse' {} a -> s {httpStatus = a} :: DescribeTaskSetsResponse)

instance Prelude.NFData DescribeTaskSetsResponse where
  rnf DescribeTaskSetsResponse' {..} =
    Prelude.rnf failures
      `Prelude.seq` Prelude.rnf taskSets
      `Prelude.seq` Prelude.rnf httpStatus
