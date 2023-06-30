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
-- Module      : Amazonka.EKS.DescribeNodegroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns descriptive information about an Amazon EKS node group.
module Amazonka.EKS.DescribeNodegroup
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeNodegroup' smart constructor.
data DescribeNodegroup = DescribeNodegroup'
  { -- | The name of the Amazon EKS cluster associated with the node group.
    clusterName :: Prelude.Text,
    -- | The name of the node group to describe.
    nodegroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'nodegroupName'
  Prelude.Text ->
  DescribeNodegroup
newDescribeNodegroup pClusterName_ pNodegroupName_ =
  DescribeNodegroup'
    { clusterName = pClusterName_,
      nodegroupName = pNodegroupName_
    }

-- | The name of the Amazon EKS cluster associated with the node group.
describeNodegroup_clusterName :: Lens.Lens' DescribeNodegroup Prelude.Text
describeNodegroup_clusterName = Lens.lens (\DescribeNodegroup' {clusterName} -> clusterName) (\s@DescribeNodegroup' {} a -> s {clusterName = a} :: DescribeNodegroup)

-- | The name of the node group to describe.
describeNodegroup_nodegroupName :: Lens.Lens' DescribeNodegroup Prelude.Text
describeNodegroup_nodegroupName = Lens.lens (\DescribeNodegroup' {nodegroupName} -> nodegroupName) (\s@DescribeNodegroup' {} a -> s {nodegroupName = a} :: DescribeNodegroup)

instance Core.AWSRequest DescribeNodegroup where
  type
    AWSResponse DescribeNodegroup =
      DescribeNodegroupResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNodegroupResponse'
            Prelude.<$> (x Data..?> "nodegroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeNodegroup where
  hashWithSalt _salt DescribeNodegroup' {..} =
    _salt
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` nodegroupName

instance Prelude.NFData DescribeNodegroup where
  rnf DescribeNodegroup' {..} =
    Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf nodegroupName

instance Data.ToHeaders DescribeNodegroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeNodegroup where
  toPath DescribeNodegroup' {..} =
    Prelude.mconcat
      [ "/clusters/",
        Data.toBS clusterName,
        "/node-groups/",
        Data.toBS nodegroupName
      ]

instance Data.ToQuery DescribeNodegroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNodegroupResponse' smart constructor.
data DescribeNodegroupResponse = DescribeNodegroupResponse'
  { -- | The full description of your node group.
    nodegroup :: Prelude.Maybe Nodegroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeNodegroupResponse
newDescribeNodegroupResponse pHttpStatus_ =
  DescribeNodegroupResponse'
    { nodegroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The full description of your node group.
describeNodegroupResponse_nodegroup :: Lens.Lens' DescribeNodegroupResponse (Prelude.Maybe Nodegroup)
describeNodegroupResponse_nodegroup = Lens.lens (\DescribeNodegroupResponse' {nodegroup} -> nodegroup) (\s@DescribeNodegroupResponse' {} a -> s {nodegroup = a} :: DescribeNodegroupResponse)

-- | The response's http status code.
describeNodegroupResponse_httpStatus :: Lens.Lens' DescribeNodegroupResponse Prelude.Int
describeNodegroupResponse_httpStatus = Lens.lens (\DescribeNodegroupResponse' {httpStatus} -> httpStatus) (\s@DescribeNodegroupResponse' {} a -> s {httpStatus = a} :: DescribeNodegroupResponse)

instance Prelude.NFData DescribeNodegroupResponse where
  rnf DescribeNodegroupResponse' {..} =
    Prelude.rnf nodegroup
      `Prelude.seq` Prelude.rnf httpStatus
