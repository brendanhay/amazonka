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
-- Module      : Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified instances from the specified load balancer.
-- After the instance is deregistered, it no longer receives traffic from
-- the load balancer.
--
-- You can use DescribeLoadBalancers to verify that the instance is
-- deregistered from the load balancer.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-deregister-register-instances.html Register or De-Register EC2 Instances>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.DeregisterInstancesFromLoadBalancer
  ( -- * Creating a Request
    DeregisterInstancesFromLoadBalancer (..),
    newDeregisterInstancesFromLoadBalancer,

    -- * Request Lenses
    deregisterInstancesFromLoadBalancer_loadBalancerName,
    deregisterInstancesFromLoadBalancer_instances,

    -- * Destructuring the Response
    DeregisterInstancesFromLoadBalancerResponse (..),
    newDeregisterInstancesFromLoadBalancerResponse,

    -- * Response Lenses
    deregisterInstancesFromLoadBalancerResponse_instances,
    deregisterInstancesFromLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeregisterInstancesFromLoadBalancer.
--
-- /See:/ 'newDeregisterInstancesFromLoadBalancer' smart constructor.
data DeregisterInstancesFromLoadBalancer = DeregisterInstancesFromLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The IDs of the instances.
    instances :: [Instance]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstancesFromLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'deregisterInstancesFromLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- 'instances', 'deregisterInstancesFromLoadBalancer_instances' - The IDs of the instances.
newDeregisterInstancesFromLoadBalancer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  DeregisterInstancesFromLoadBalancer
newDeregisterInstancesFromLoadBalancer
  pLoadBalancerName_ =
    DeregisterInstancesFromLoadBalancer'
      { loadBalancerName =
          pLoadBalancerName_,
        instances = Prelude.mempty
      }

-- | The name of the load balancer.
deregisterInstancesFromLoadBalancer_loadBalancerName :: Lens.Lens' DeregisterInstancesFromLoadBalancer Prelude.Text
deregisterInstancesFromLoadBalancer_loadBalancerName = Lens.lens (\DeregisterInstancesFromLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DeregisterInstancesFromLoadBalancer' {} a -> s {loadBalancerName = a} :: DeregisterInstancesFromLoadBalancer)

-- | The IDs of the instances.
deregisterInstancesFromLoadBalancer_instances :: Lens.Lens' DeregisterInstancesFromLoadBalancer [Instance]
deregisterInstancesFromLoadBalancer_instances = Lens.lens (\DeregisterInstancesFromLoadBalancer' {instances} -> instances) (\s@DeregisterInstancesFromLoadBalancer' {} a -> s {instances = a} :: DeregisterInstancesFromLoadBalancer) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    DeregisterInstancesFromLoadBalancer
  where
  type
    AWSResponse DeregisterInstancesFromLoadBalancer =
      DeregisterInstancesFromLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeregisterInstancesFromLoadBalancerResult"
      ( \s h x ->
          DeregisterInstancesFromLoadBalancerResponse'
            Prelude.<$> ( x Core..@? "Instances" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterInstancesFromLoadBalancer

instance
  Prelude.NFData
    DeregisterInstancesFromLoadBalancer

instance
  Core.ToHeaders
    DeregisterInstancesFromLoadBalancer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DeregisterInstancesFromLoadBalancer
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeregisterInstancesFromLoadBalancer
  where
  toQuery DeregisterInstancesFromLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeregisterInstancesFromLoadBalancer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "Instances"
          Core.=: Core.toQueryList "member" instances
      ]

-- | Contains the output of DeregisterInstancesFromLoadBalancer.
--
-- /See:/ 'newDeregisterInstancesFromLoadBalancerResponse' smart constructor.
data DeregisterInstancesFromLoadBalancerResponse = DeregisterInstancesFromLoadBalancerResponse'
  { -- | The remaining instances registered with the load balancer.
    instances :: Prelude.Maybe [Instance],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterInstancesFromLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'deregisterInstancesFromLoadBalancerResponse_instances' - The remaining instances registered with the load balancer.
--
-- 'httpStatus', 'deregisterInstancesFromLoadBalancerResponse_httpStatus' - The response's http status code.
newDeregisterInstancesFromLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeregisterInstancesFromLoadBalancerResponse
newDeregisterInstancesFromLoadBalancerResponse
  pHttpStatus_ =
    DeregisterInstancesFromLoadBalancerResponse'
      { instances =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The remaining instances registered with the load balancer.
deregisterInstancesFromLoadBalancerResponse_instances :: Lens.Lens' DeregisterInstancesFromLoadBalancerResponse (Prelude.Maybe [Instance])
deregisterInstancesFromLoadBalancerResponse_instances = Lens.lens (\DeregisterInstancesFromLoadBalancerResponse' {instances} -> instances) (\s@DeregisterInstancesFromLoadBalancerResponse' {} a -> s {instances = a} :: DeregisterInstancesFromLoadBalancerResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deregisterInstancesFromLoadBalancerResponse_httpStatus :: Lens.Lens' DeregisterInstancesFromLoadBalancerResponse Prelude.Int
deregisterInstancesFromLoadBalancerResponse_httpStatus = Lens.lens (\DeregisterInstancesFromLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DeregisterInstancesFromLoadBalancerResponse' {} a -> s {httpStatus = a} :: DeregisterInstancesFromLoadBalancerResponse)

instance
  Prelude.NFData
    DeregisterInstancesFromLoadBalancerResponse
