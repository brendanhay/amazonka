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
-- Module      : Network.AWS.ELB.RegisterInstancesWithLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified instances to the specified load balancer.
--
-- The instance must be a running instance in the same network as the load
-- balancer (EC2-Classic or the same VPC). If you have EC2-Classic
-- instances and a load balancer in a VPC with ClassicLink enabled, you can
-- link the EC2-Classic instances to that VPC and then register the linked
-- EC2-Classic instances with the load balancer in the VPC.
--
-- Note that @RegisterInstanceWithLoadBalancer@ completes when the request
-- has been registered. Instance registration takes a little time to
-- complete. To check the state of the registered instances, use
-- DescribeLoadBalancers or DescribeInstanceHealth.
--
-- After the instance is registered, it starts receiving traffic and
-- requests from the load balancer. Any instance that is not in one of the
-- Availability Zones registered for the load balancer is moved to the
-- @OutOfService@ state. If an Availability Zone is added to the load
-- balancer later, any instances registered with the load balancer move to
-- the @InService@ state.
--
-- To deregister instances from a load balancer, use
-- DeregisterInstancesFromLoadBalancer.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html Register or De-Register EC2 Instances>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.RegisterInstancesWithLoadBalancer
  ( -- * Creating a Request
    RegisterInstancesWithLoadBalancer (..),
    newRegisterInstancesWithLoadBalancer,

    -- * Request Lenses
    registerInstancesWithLoadBalancer_loadBalancerName,
    registerInstancesWithLoadBalancer_instances,

    -- * Destructuring the Response
    RegisterInstancesWithLoadBalancerResponse (..),
    newRegisterInstancesWithLoadBalancerResponse,

    -- * Response Lenses
    registerInstancesWithLoadBalancerResponse_instances,
    registerInstancesWithLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RegisterInstancesWithLoadBalancer.
--
-- /See:/ 'newRegisterInstancesWithLoadBalancer' smart constructor.
data RegisterInstancesWithLoadBalancer = RegisterInstancesWithLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The IDs of the instances.
    instances :: [Instance]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstancesWithLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'registerInstancesWithLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- 'instances', 'registerInstancesWithLoadBalancer_instances' - The IDs of the instances.
newRegisterInstancesWithLoadBalancer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  RegisterInstancesWithLoadBalancer
newRegisterInstancesWithLoadBalancer
  pLoadBalancerName_ =
    RegisterInstancesWithLoadBalancer'
      { loadBalancerName =
          pLoadBalancerName_,
        instances = Prelude.mempty
      }

-- | The name of the load balancer.
registerInstancesWithLoadBalancer_loadBalancerName :: Lens.Lens' RegisterInstancesWithLoadBalancer Prelude.Text
registerInstancesWithLoadBalancer_loadBalancerName = Lens.lens (\RegisterInstancesWithLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@RegisterInstancesWithLoadBalancer' {} a -> s {loadBalancerName = a} :: RegisterInstancesWithLoadBalancer)

-- | The IDs of the instances.
registerInstancesWithLoadBalancer_instances :: Lens.Lens' RegisterInstancesWithLoadBalancer [Instance]
registerInstancesWithLoadBalancer_instances = Lens.lens (\RegisterInstancesWithLoadBalancer' {instances} -> instances) (\s@RegisterInstancesWithLoadBalancer' {} a -> s {instances = a} :: RegisterInstancesWithLoadBalancer) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    RegisterInstancesWithLoadBalancer
  where
  type
    AWSResponse RegisterInstancesWithLoadBalancer =
      RegisterInstancesWithLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RegisterInstancesWithLoadBalancerResult"
      ( \s h x ->
          RegisterInstancesWithLoadBalancerResponse'
            Prelude.<$> ( x Core..@? "Instances" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterInstancesWithLoadBalancer

instance
  Prelude.NFData
    RegisterInstancesWithLoadBalancer

instance
  Core.ToHeaders
    RegisterInstancesWithLoadBalancer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    RegisterInstancesWithLoadBalancer
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    RegisterInstancesWithLoadBalancer
  where
  toQuery RegisterInstancesWithLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RegisterInstancesWithLoadBalancer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "Instances"
          Core.=: Core.toQueryList "member" instances
      ]

-- | Contains the output of RegisterInstancesWithLoadBalancer.
--
-- /See:/ 'newRegisterInstancesWithLoadBalancerResponse' smart constructor.
data RegisterInstancesWithLoadBalancerResponse = RegisterInstancesWithLoadBalancerResponse'
  { -- | The updated list of instances for the load balancer.
    instances :: Prelude.Maybe [Instance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterInstancesWithLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'registerInstancesWithLoadBalancerResponse_instances' - The updated list of instances for the load balancer.
--
-- 'httpStatus', 'registerInstancesWithLoadBalancerResponse_httpStatus' - The response's http status code.
newRegisterInstancesWithLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterInstancesWithLoadBalancerResponse
newRegisterInstancesWithLoadBalancerResponse
  pHttpStatus_ =
    RegisterInstancesWithLoadBalancerResponse'
      { instances =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The updated list of instances for the load balancer.
registerInstancesWithLoadBalancerResponse_instances :: Lens.Lens' RegisterInstancesWithLoadBalancerResponse (Prelude.Maybe [Instance])
registerInstancesWithLoadBalancerResponse_instances = Lens.lens (\RegisterInstancesWithLoadBalancerResponse' {instances} -> instances) (\s@RegisterInstancesWithLoadBalancerResponse' {} a -> s {instances = a} :: RegisterInstancesWithLoadBalancerResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
registerInstancesWithLoadBalancerResponse_httpStatus :: Lens.Lens' RegisterInstancesWithLoadBalancerResponse Prelude.Int
registerInstancesWithLoadBalancerResponse_httpStatus = Lens.lens (\RegisterInstancesWithLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@RegisterInstancesWithLoadBalancerResponse' {} a -> s {httpStatus = a} :: RegisterInstancesWithLoadBalancerResponse)

instance
  Prelude.NFData
    RegisterInstancesWithLoadBalancerResponse
