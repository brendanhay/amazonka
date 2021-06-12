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
-- Module      : Network.AWS.EKS.DescribeNodegroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptive information about an Amazon EKS node group.
module Network.AWS.EKS.DescribeNodegroup
  ( -- * Creating a Request
    DescribeNodegroup (..),
    newDescribeNodegroup,

    -- * Request Lenses
    describeNodegroup_clusterName,
    describeNodegroup_nodegroupName,

    -- * Destructuring the Response
    DescribeNodegroupResponse (..),
    newDescribeNodegroupResponse,

    -- * Response Lenses
    describeNodegroupResponse_nodegroup,
    describeNodegroupResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNodegroup' smart constructor.
data DescribeNodegroup = DescribeNodegroup'
  { -- | The name of the Amazon EKS cluster associated with the node group.
    clusterName :: Core.Text,
    -- | The name of the node group to describe.
    nodegroupName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNodegroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterName', 'describeNodegroup_clusterName' - The name of the Amazon EKS cluster associated with the node group.
--
-- 'nodegroupName', 'describeNodegroup_nodegroupName' - The name of the node group to describe.
newDescribeNodegroup ::
  -- | 'clusterName'
  Core.Text ->
  -- | 'nodegroupName'
  Core.Text ->
  DescribeNodegroup
newDescribeNodegroup pClusterName_ pNodegroupName_ =
  DescribeNodegroup'
    { clusterName = pClusterName_,
      nodegroupName = pNodegroupName_
    }

-- | The name of the Amazon EKS cluster associated with the node group.
describeNodegroup_clusterName :: Lens.Lens' DescribeNodegroup Core.Text
describeNodegroup_clusterName = Lens.lens (\DescribeNodegroup' {clusterName} -> clusterName) (\s@DescribeNodegroup' {} a -> s {clusterName = a} :: DescribeNodegroup)

-- | The name of the node group to describe.
describeNodegroup_nodegroupName :: Lens.Lens' DescribeNodegroup Core.Text
describeNodegroup_nodegroupName = Lens.lens (\DescribeNodegroup' {nodegroupName} -> nodegroupName) (\s@DescribeNodegroup' {} a -> s {nodegroupName = a} :: DescribeNodegroup)

instance Core.AWSRequest DescribeNodegroup where
  type
    AWSResponse DescribeNodegroup =
      DescribeNodegroupResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNodegroupResponse'
            Core.<$> (x Core..?> "nodegroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeNodegroup

instance Core.NFData DescribeNodegroup

instance Core.ToHeaders DescribeNodegroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DescribeNodegroup where
  toPath DescribeNodegroup' {..} =
    Core.mconcat
      [ "/clusters/",
        Core.toBS clusterName,
        "/node-groups/",
        Core.toBS nodegroupName
      ]

instance Core.ToQuery DescribeNodegroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeNodegroupResponse' smart constructor.
data DescribeNodegroupResponse = DescribeNodegroupResponse'
  { -- | The full description of your node group.
    nodegroup :: Core.Maybe Nodegroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNodegroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodegroup', 'describeNodegroupResponse_nodegroup' - The full description of your node group.
--
-- 'httpStatus', 'describeNodegroupResponse_httpStatus' - The response's http status code.
newDescribeNodegroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNodegroupResponse
newDescribeNodegroupResponse pHttpStatus_ =
  DescribeNodegroupResponse'
    { nodegroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your node group.
describeNodegroupResponse_nodegroup :: Lens.Lens' DescribeNodegroupResponse (Core.Maybe Nodegroup)
describeNodegroupResponse_nodegroup = Lens.lens (\DescribeNodegroupResponse' {nodegroup} -> nodegroup) (\s@DescribeNodegroupResponse' {} a -> s {nodegroup = a} :: DescribeNodegroupResponse)

-- | The response's http status code.
describeNodegroupResponse_httpStatus :: Lens.Lens' DescribeNodegroupResponse Core.Int
describeNodegroupResponse_httpStatus = Lens.lens (\DescribeNodegroupResponse' {httpStatus} -> httpStatus) (\s@DescribeNodegroupResponse' {} a -> s {httpStatus = a} :: DescribeNodegroupResponse)

instance Core.NFData DescribeNodegroupResponse
