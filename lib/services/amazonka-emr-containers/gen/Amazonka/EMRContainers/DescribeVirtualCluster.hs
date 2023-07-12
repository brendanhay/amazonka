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
-- Module      : Amazonka.EMRContainers.DescribeVirtualCluster
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays detailed information about a specified virtual cluster. Virtual
-- cluster is a managed entity on Amazon EMR on EKS. You can create,
-- describe, list and delete virtual clusters. They do not consume any
-- additional resource in your system. A single virtual cluster maps to a
-- single Kubernetes namespace. Given this relationship, you can model
-- virtual clusters the same way you model Kubernetes namespaces to meet
-- your requirements.
module Amazonka.EMRContainers.DescribeVirtualCluster
  ( -- * Creating a Request
    DescribeVirtualCluster (..),
    newDescribeVirtualCluster,

    -- * Request Lenses
    describeVirtualCluster_id,

    -- * Destructuring the Response
    DescribeVirtualClusterResponse (..),
    newDescribeVirtualClusterResponse,

    -- * Response Lenses
    describeVirtualClusterResponse_virtualCluster,
    describeVirtualClusterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRContainers.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVirtualCluster' smart constructor.
data DescribeVirtualCluster = DescribeVirtualCluster'
  { -- | The ID of the virtual cluster that will be described.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeVirtualCluster_id' - The ID of the virtual cluster that will be described.
newDescribeVirtualCluster ::
  -- | 'id'
  Prelude.Text ->
  DescribeVirtualCluster
newDescribeVirtualCluster pId_ =
  DescribeVirtualCluster' {id = pId_}

-- | The ID of the virtual cluster that will be described.
describeVirtualCluster_id :: Lens.Lens' DescribeVirtualCluster Prelude.Text
describeVirtualCluster_id = Lens.lens (\DescribeVirtualCluster' {id} -> id) (\s@DescribeVirtualCluster' {} a -> s {id = a} :: DescribeVirtualCluster)

instance Core.AWSRequest DescribeVirtualCluster where
  type
    AWSResponse DescribeVirtualCluster =
      DescribeVirtualClusterResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVirtualClusterResponse'
            Prelude.<$> (x Data..?> "virtualCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVirtualCluster where
  hashWithSalt _salt DescribeVirtualCluster' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DescribeVirtualCluster where
  rnf DescribeVirtualCluster' {..} = Prelude.rnf id

instance Data.ToHeaders DescribeVirtualCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeVirtualCluster where
  toPath DescribeVirtualCluster' {..} =
    Prelude.mconcat ["/virtualclusters/", Data.toBS id]

instance Data.ToQuery DescribeVirtualCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeVirtualClusterResponse' smart constructor.
data DescribeVirtualClusterResponse = DescribeVirtualClusterResponse'
  { -- | This output displays information about the specified virtual cluster.
    virtualCluster :: Prelude.Maybe VirtualCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVirtualClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualCluster', 'describeVirtualClusterResponse_virtualCluster' - This output displays information about the specified virtual cluster.
--
-- 'httpStatus', 'describeVirtualClusterResponse_httpStatus' - The response's http status code.
newDescribeVirtualClusterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVirtualClusterResponse
newDescribeVirtualClusterResponse pHttpStatus_ =
  DescribeVirtualClusterResponse'
    { virtualCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This output displays information about the specified virtual cluster.
describeVirtualClusterResponse_virtualCluster :: Lens.Lens' DescribeVirtualClusterResponse (Prelude.Maybe VirtualCluster)
describeVirtualClusterResponse_virtualCluster = Lens.lens (\DescribeVirtualClusterResponse' {virtualCluster} -> virtualCluster) (\s@DescribeVirtualClusterResponse' {} a -> s {virtualCluster = a} :: DescribeVirtualClusterResponse)

-- | The response's http status code.
describeVirtualClusterResponse_httpStatus :: Lens.Lens' DescribeVirtualClusterResponse Prelude.Int
describeVirtualClusterResponse_httpStatus = Lens.lens (\DescribeVirtualClusterResponse' {httpStatus} -> httpStatus) (\s@DescribeVirtualClusterResponse' {} a -> s {httpStatus = a} :: DescribeVirtualClusterResponse)

instance
  Prelude.NFData
    DescribeVirtualClusterResponse
  where
  rnf DescribeVirtualClusterResponse' {..} =
    Prelude.rnf virtualCluster
      `Prelude.seq` Prelude.rnf httpStatus
