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
-- Module      : Amazonka.ELB.DeregisterInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ELB.DeregisterInstancesFromLoadBalancer
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
deregisterInstancesFromLoadBalancer_instances = Lens.lens (\DeregisterInstancesFromLoadBalancer' {instances} -> instances) (\s@DeregisterInstancesFromLoadBalancer' {} a -> s {instances = a} :: DeregisterInstancesFromLoadBalancer) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DeregisterInstancesFromLoadBalancer
  where
  type
    AWSResponse DeregisterInstancesFromLoadBalancer =
      DeregisterInstancesFromLoadBalancerResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeregisterInstancesFromLoadBalancerResult"
      ( \s h x ->
          DeregisterInstancesFromLoadBalancerResponse'
            Prelude.<$> ( x
                            Data..@? "Instances"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeregisterInstancesFromLoadBalancer
  where
  hashWithSalt
    _salt
    DeregisterInstancesFromLoadBalancer' {..} =
      _salt
        `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` instances

instance
  Prelude.NFData
    DeregisterInstancesFromLoadBalancer
  where
  rnf DeregisterInstancesFromLoadBalancer' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf instances

instance
  Data.ToHeaders
    DeregisterInstancesFromLoadBalancer
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeregisterInstancesFromLoadBalancer
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeregisterInstancesFromLoadBalancer
  where
  toQuery DeregisterInstancesFromLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DeregisterInstancesFromLoadBalancer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Data.=: loadBalancerName,
        "Instances"
          Data.=: Data.toQueryList "member" instances
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
deregisterInstancesFromLoadBalancerResponse_instances = Lens.lens (\DeregisterInstancesFromLoadBalancerResponse' {instances} -> instances) (\s@DeregisterInstancesFromLoadBalancerResponse' {} a -> s {instances = a} :: DeregisterInstancesFromLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deregisterInstancesFromLoadBalancerResponse_httpStatus :: Lens.Lens' DeregisterInstancesFromLoadBalancerResponse Prelude.Int
deregisterInstancesFromLoadBalancerResponse_httpStatus = Lens.lens (\DeregisterInstancesFromLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DeregisterInstancesFromLoadBalancerResponse' {} a -> s {httpStatus = a} :: DeregisterInstancesFromLoadBalancerResponse)

instance
  Prelude.NFData
    DeregisterInstancesFromLoadBalancerResponse
  where
  rnf DeregisterInstancesFromLoadBalancerResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf httpStatus
