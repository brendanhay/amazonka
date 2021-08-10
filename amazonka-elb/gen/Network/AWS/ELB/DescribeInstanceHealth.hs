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
-- Module      : Network.AWS.ELB.DescribeInstanceHealth
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the state of the specified instances with respect to the
-- specified load balancer. If no instances are specified, the call
-- describes the state of all instances that are currently registered with
-- the load balancer. If instances are specified, their state is returned
-- even if they are no longer registered with the load balancer. The state
-- of terminated instances is not returned.
module Network.AWS.ELB.DescribeInstanceHealth
  ( -- * Creating a Request
    DescribeInstanceHealth (..),
    newDescribeInstanceHealth,

    -- * Request Lenses
    describeInstanceHealth_instances,
    describeInstanceHealth_loadBalancerName,

    -- * Destructuring the Response
    DescribeInstanceHealthResponse (..),
    newDescribeInstanceHealthResponse,

    -- * Response Lenses
    describeInstanceHealthResponse_instanceStates,
    describeInstanceHealthResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeInstanceHealth.
--
-- /See:/ 'newDescribeInstanceHealth' smart constructor.
data DescribeInstanceHealth = DescribeInstanceHealth'
  { -- | The IDs of the instances.
    instances :: Prelude.Maybe [Instance],
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceHealth' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'describeInstanceHealth_instances' - The IDs of the instances.
--
-- 'loadBalancerName', 'describeInstanceHealth_loadBalancerName' - The name of the load balancer.
newDescribeInstanceHealth ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  DescribeInstanceHealth
newDescribeInstanceHealth pLoadBalancerName_ =
  DescribeInstanceHealth'
    { instances =
        Prelude.Nothing,
      loadBalancerName = pLoadBalancerName_
    }

-- | The IDs of the instances.
describeInstanceHealth_instances :: Lens.Lens' DescribeInstanceHealth (Prelude.Maybe [Instance])
describeInstanceHealth_instances = Lens.lens (\DescribeInstanceHealth' {instances} -> instances) (\s@DescribeInstanceHealth' {} a -> s {instances = a} :: DescribeInstanceHealth) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the load balancer.
describeInstanceHealth_loadBalancerName :: Lens.Lens' DescribeInstanceHealth Prelude.Text
describeInstanceHealth_loadBalancerName = Lens.lens (\DescribeInstanceHealth' {loadBalancerName} -> loadBalancerName) (\s@DescribeInstanceHealth' {} a -> s {loadBalancerName = a} :: DescribeInstanceHealth)

instance Core.AWSRequest DescribeInstanceHealth where
  type
    AWSResponse DescribeInstanceHealth =
      DescribeInstanceHealthResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeInstanceHealthResult"
      ( \s h x ->
          DescribeInstanceHealthResponse'
            Prelude.<$> ( x Core..@? "InstanceStates" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstanceHealth

instance Prelude.NFData DescribeInstanceHealth

instance Core.ToHeaders DescribeInstanceHealth where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeInstanceHealth where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeInstanceHealth where
  toQuery DescribeInstanceHealth' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeInstanceHealth" :: Prelude.ByteString),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "Instances"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> instances),
        "LoadBalancerName" Core.=: loadBalancerName
      ]

-- | Contains the output for DescribeInstanceHealth.
--
-- /See:/ 'newDescribeInstanceHealthResponse' smart constructor.
data DescribeInstanceHealthResponse = DescribeInstanceHealthResponse'
  { -- | Information about the health of the instances.
    instanceStates :: Prelude.Maybe [InstanceState],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceHealthResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceStates', 'describeInstanceHealthResponse_instanceStates' - Information about the health of the instances.
--
-- 'httpStatus', 'describeInstanceHealthResponse_httpStatus' - The response's http status code.
newDescribeInstanceHealthResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceHealthResponse
newDescribeInstanceHealthResponse pHttpStatus_ =
  DescribeInstanceHealthResponse'
    { instanceStates =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the health of the instances.
describeInstanceHealthResponse_instanceStates :: Lens.Lens' DescribeInstanceHealthResponse (Prelude.Maybe [InstanceState])
describeInstanceHealthResponse_instanceStates = Lens.lens (\DescribeInstanceHealthResponse' {instanceStates} -> instanceStates) (\s@DescribeInstanceHealthResponse' {} a -> s {instanceStates = a} :: DescribeInstanceHealthResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInstanceHealthResponse_httpStatus :: Lens.Lens' DescribeInstanceHealthResponse Prelude.Int
describeInstanceHealthResponse_httpStatus = Lens.lens (\DescribeInstanceHealthResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceHealthResponse' {} a -> s {httpStatus = a} :: DescribeInstanceHealthResponse)

instance
  Prelude.NFData
    DescribeInstanceHealthResponse
