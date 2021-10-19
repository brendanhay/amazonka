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
-- Module      : Network.AWS.OpsWorks.DescribeInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Requests a description of a set of instances.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeInstances
  ( -- * Creating a Request
    DescribeInstances (..),
    newDescribeInstances,

    -- * Request Lenses
    describeInstances_instanceIds,
    describeInstances_stackId,
    describeInstances_layerId,

    -- * Destructuring the Response
    DescribeInstancesResponse (..),
    newDescribeInstancesResponse,

    -- * Response Lenses
    describeInstancesResponse_instances,
    describeInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { -- | An array of instance IDs to be described. If you use this parameter,
    -- @DescribeInstances@ returns a description of the specified instances.
    -- Otherwise, it returns a description of every instance.
    instanceIds :: Prelude.Maybe [Prelude.Text],
    -- | A stack ID. If you use this parameter, @DescribeInstances@ returns
    -- descriptions of the instances associated with the specified stack.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | A layer ID. If you use this parameter, @DescribeInstances@ returns
    -- descriptions of the instances associated with the specified layer.
    layerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceIds', 'describeInstances_instanceIds' - An array of instance IDs to be described. If you use this parameter,
-- @DescribeInstances@ returns a description of the specified instances.
-- Otherwise, it returns a description of every instance.
--
-- 'stackId', 'describeInstances_stackId' - A stack ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified stack.
--
-- 'layerId', 'describeInstances_layerId' - A layer ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified layer.
newDescribeInstances ::
  DescribeInstances
newDescribeInstances =
  DescribeInstances'
    { instanceIds = Prelude.Nothing,
      stackId = Prelude.Nothing,
      layerId = Prelude.Nothing
    }

-- | An array of instance IDs to be described. If you use this parameter,
-- @DescribeInstances@ returns a description of the specified instances.
-- Otherwise, it returns a description of every instance.
describeInstances_instanceIds :: Lens.Lens' DescribeInstances (Prelude.Maybe [Prelude.Text])
describeInstances_instanceIds = Lens.lens (\DescribeInstances' {instanceIds} -> instanceIds) (\s@DescribeInstances' {} a -> s {instanceIds = a} :: DescribeInstances) Prelude.. Lens.mapping Lens.coerced

-- | A stack ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified stack.
describeInstances_stackId :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_stackId = Lens.lens (\DescribeInstances' {stackId} -> stackId) (\s@DescribeInstances' {} a -> s {stackId = a} :: DescribeInstances)

-- | A layer ID. If you use this parameter, @DescribeInstances@ returns
-- descriptions of the instances associated with the specified layer.
describeInstances_layerId :: Lens.Lens' DescribeInstances (Prelude.Maybe Prelude.Text)
describeInstances_layerId = Lens.lens (\DescribeInstances' {layerId} -> layerId) (\s@DescribeInstances' {} a -> s {layerId = a} :: DescribeInstances)

instance Core.AWSRequest DescribeInstances where
  type
    AWSResponse DescribeInstances =
      DescribeInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstancesResponse'
            Prelude.<$> (x Core..?> "Instances" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstances

instance Prelude.NFData DescribeInstances

instance Core.ToHeaders DescribeInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeInstances where
  toJSON DescribeInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("InstanceIds" Core..=) Prelude.<$> instanceIds,
            ("StackId" Core..=) Prelude.<$> stackId,
            ("LayerId" Core..=) Prelude.<$> layerId
          ]
      )

instance Core.ToPath DescribeInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstances where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeInstances@ request.
--
-- /See:/ 'newDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { -- | An array of @Instance@ objects that describe the instances.
    instances :: Prelude.Maybe [Instance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'describeInstancesResponse_instances' - An array of @Instance@ objects that describe the instances.
--
-- 'httpStatus', 'describeInstancesResponse_httpStatus' - The response's http status code.
newDescribeInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstancesResponse
newDescribeInstancesResponse pHttpStatus_ =
  DescribeInstancesResponse'
    { instances =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Instance@ objects that describe the instances.
describeInstancesResponse_instances :: Lens.Lens' DescribeInstancesResponse (Prelude.Maybe [Instance])
describeInstancesResponse_instances = Lens.lens (\DescribeInstancesResponse' {instances} -> instances) (\s@DescribeInstancesResponse' {} a -> s {instances = a} :: DescribeInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInstancesResponse_httpStatus :: Lens.Lens' DescribeInstancesResponse Prelude.Int
describeInstancesResponse_httpStatus = Lens.lens (\DescribeInstancesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstancesResponse' {} a -> s {httpStatus = a} :: DescribeInstancesResponse)

instance Prelude.NFData DescribeInstancesResponse
