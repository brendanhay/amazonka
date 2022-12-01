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
-- Module      : Amazonka.ECS.DescribeContainerInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more container instances. Returns metadata about each
-- container instance requested.
module Amazonka.ECS.DescribeContainerInstances
  ( -- * Creating a Request
    DescribeContainerInstances (..),
    newDescribeContainerInstances,

    -- * Request Lenses
    describeContainerInstances_cluster,
    describeContainerInstances_include,
    describeContainerInstances_containerInstances,

    -- * Destructuring the Response
    DescribeContainerInstancesResponse (..),
    newDescribeContainerInstancesResponse,

    -- * Response Lenses
    describeContainerInstancesResponse_containerInstances,
    describeContainerInstancesResponse_failures,
    describeContainerInstancesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeContainerInstances' smart constructor.
data DescribeContainerInstances = DescribeContainerInstances'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- hosts the container instances to describe. If you do not specify a
    -- cluster, the default cluster is assumed. This parameter is required if
    -- the container instance or container instances you are describing were
    -- launched in any cluster other than the default cluster.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether you want to see the resource tags for the container
    -- instance. If @TAGS@ is specified, the tags are included in the response.
    -- If @CONTAINER_INSTANCE_HEALTH@ is specified, the container instance
    -- health is included in the response. If this field is omitted, tags and
    -- container instance health status aren\'t included in the response.
    include :: Prelude.Maybe [ContainerInstanceField],
    -- | A list of up to 100 container instance IDs or full Amazon Resource Name
    -- (ARN) entries.
    containerInstances :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContainerInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'describeContainerInstances_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to describe. If you do not specify a
-- cluster, the default cluster is assumed. This parameter is required if
-- the container instance or container instances you are describing were
-- launched in any cluster other than the default cluster.
--
-- 'include', 'describeContainerInstances_include' - Specifies whether you want to see the resource tags for the container
-- instance. If @TAGS@ is specified, the tags are included in the response.
-- If @CONTAINER_INSTANCE_HEALTH@ is specified, the container instance
-- health is included in the response. If this field is omitted, tags and
-- container instance health status aren\'t included in the response.
--
-- 'containerInstances', 'describeContainerInstances_containerInstances' - A list of up to 100 container instance IDs or full Amazon Resource Name
-- (ARN) entries.
newDescribeContainerInstances ::
  DescribeContainerInstances
newDescribeContainerInstances =
  DescribeContainerInstances'
    { cluster =
        Prelude.Nothing,
      include = Prelude.Nothing,
      containerInstances = Prelude.mempty
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- hosts the container instances to describe. If you do not specify a
-- cluster, the default cluster is assumed. This parameter is required if
-- the container instance or container instances you are describing were
-- launched in any cluster other than the default cluster.
describeContainerInstances_cluster :: Lens.Lens' DescribeContainerInstances (Prelude.Maybe Prelude.Text)
describeContainerInstances_cluster = Lens.lens (\DescribeContainerInstances' {cluster} -> cluster) (\s@DescribeContainerInstances' {} a -> s {cluster = a} :: DescribeContainerInstances)

-- | Specifies whether you want to see the resource tags for the container
-- instance. If @TAGS@ is specified, the tags are included in the response.
-- If @CONTAINER_INSTANCE_HEALTH@ is specified, the container instance
-- health is included in the response. If this field is omitted, tags and
-- container instance health status aren\'t included in the response.
describeContainerInstances_include :: Lens.Lens' DescribeContainerInstances (Prelude.Maybe [ContainerInstanceField])
describeContainerInstances_include = Lens.lens (\DescribeContainerInstances' {include} -> include) (\s@DescribeContainerInstances' {} a -> s {include = a} :: DescribeContainerInstances) Prelude.. Lens.mapping Lens.coerced

-- | A list of up to 100 container instance IDs or full Amazon Resource Name
-- (ARN) entries.
describeContainerInstances_containerInstances :: Lens.Lens' DescribeContainerInstances [Prelude.Text]
describeContainerInstances_containerInstances = Lens.lens (\DescribeContainerInstances' {containerInstances} -> containerInstances) (\s@DescribeContainerInstances' {} a -> s {containerInstances = a} :: DescribeContainerInstances) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeContainerInstances where
  type
    AWSResponse DescribeContainerInstances =
      DescribeContainerInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeContainerInstancesResponse'
            Prelude.<$> ( x Core..?> "containerInstances"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeContainerInstances where
  hashWithSalt _salt DescribeContainerInstances' {..} =
    _salt `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` include
      `Prelude.hashWithSalt` containerInstances

instance Prelude.NFData DescribeContainerInstances where
  rnf DescribeContainerInstances' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf include
      `Prelude.seq` Prelude.rnf containerInstances

instance Core.ToHeaders DescribeContainerInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DescribeContainerInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeContainerInstances where
  toJSON DescribeContainerInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cluster" Core..=) Prelude.<$> cluster,
            ("include" Core..=) Prelude.<$> include,
            Prelude.Just
              ("containerInstances" Core..= containerInstances)
          ]
      )

instance Core.ToPath DescribeContainerInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeContainerInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeContainerInstancesResponse' smart constructor.
data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse'
  { -- | The list of container instances.
    containerInstances :: Prelude.Maybe [ContainerInstance],
    -- | Any failures associated with the call.
    failures :: Prelude.Maybe [Failure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeContainerInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerInstances', 'describeContainerInstancesResponse_containerInstances' - The list of container instances.
--
-- 'failures', 'describeContainerInstancesResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'describeContainerInstancesResponse_httpStatus' - The response's http status code.
newDescribeContainerInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeContainerInstancesResponse
newDescribeContainerInstancesResponse pHttpStatus_ =
  DescribeContainerInstancesResponse'
    { containerInstances =
        Prelude.Nothing,
      failures = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of container instances.
describeContainerInstancesResponse_containerInstances :: Lens.Lens' DescribeContainerInstancesResponse (Prelude.Maybe [ContainerInstance])
describeContainerInstancesResponse_containerInstances = Lens.lens (\DescribeContainerInstancesResponse' {containerInstances} -> containerInstances) (\s@DescribeContainerInstancesResponse' {} a -> s {containerInstances = a} :: DescribeContainerInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Any failures associated with the call.
describeContainerInstancesResponse_failures :: Lens.Lens' DescribeContainerInstancesResponse (Prelude.Maybe [Failure])
describeContainerInstancesResponse_failures = Lens.lens (\DescribeContainerInstancesResponse' {failures} -> failures) (\s@DescribeContainerInstancesResponse' {} a -> s {failures = a} :: DescribeContainerInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeContainerInstancesResponse_httpStatus :: Lens.Lens' DescribeContainerInstancesResponse Prelude.Int
describeContainerInstancesResponse_httpStatus = Lens.lens (\DescribeContainerInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeContainerInstancesResponse' {} a -> s {httpStatus = a} :: DescribeContainerInstancesResponse)

instance
  Prelude.NFData
    DescribeContainerInstancesResponse
  where
  rnf DescribeContainerInstancesResponse' {..} =
    Prelude.rnf containerInstances
      `Prelude.seq` Prelude.rnf failures
      `Prelude.seq` Prelude.rnf httpStatus
