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
-- Module      : Amazonka.Lightsail.DetachInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified instances from a Lightsail load balancer.
--
-- This operation waits until the instances are no longer needed before
-- they are detached from the load balancer.
--
-- The @detach instances from load balancer@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DetachInstancesFromLoadBalancer
  ( -- * Creating a Request
    DetachInstancesFromLoadBalancer (..),
    newDetachInstancesFromLoadBalancer,

    -- * Request Lenses
    detachInstancesFromLoadBalancer_loadBalancerName,
    detachInstancesFromLoadBalancer_instanceNames,

    -- * Destructuring the Response
    DetachInstancesFromLoadBalancerResponse (..),
    newDetachInstancesFromLoadBalancerResponse,

    -- * Response Lenses
    detachInstancesFromLoadBalancerResponse_operations,
    detachInstancesFromLoadBalancerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachInstancesFromLoadBalancer' smart constructor.
data DetachInstancesFromLoadBalancer = DetachInstancesFromLoadBalancer'
  { -- | The name of the Lightsail load balancer.
    loadBalancerName :: Prelude.Text,
    -- | An array of strings containing the names of the instances you want to
    -- detach from the load balancer.
    instanceNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachInstancesFromLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'detachInstancesFromLoadBalancer_loadBalancerName' - The name of the Lightsail load balancer.
--
-- 'instanceNames', 'detachInstancesFromLoadBalancer_instanceNames' - An array of strings containing the names of the instances you want to
-- detach from the load balancer.
newDetachInstancesFromLoadBalancer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  DetachInstancesFromLoadBalancer
newDetachInstancesFromLoadBalancer pLoadBalancerName_ =
  DetachInstancesFromLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instanceNames = Prelude.mempty
    }

-- | The name of the Lightsail load balancer.
detachInstancesFromLoadBalancer_loadBalancerName :: Lens.Lens' DetachInstancesFromLoadBalancer Prelude.Text
detachInstancesFromLoadBalancer_loadBalancerName = Lens.lens (\DetachInstancesFromLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DetachInstancesFromLoadBalancer' {} a -> s {loadBalancerName = a} :: DetachInstancesFromLoadBalancer)

-- | An array of strings containing the names of the instances you want to
-- detach from the load balancer.
detachInstancesFromLoadBalancer_instanceNames :: Lens.Lens' DetachInstancesFromLoadBalancer [Prelude.Text]
detachInstancesFromLoadBalancer_instanceNames = Lens.lens (\DetachInstancesFromLoadBalancer' {instanceNames} -> instanceNames) (\s@DetachInstancesFromLoadBalancer' {} a -> s {instanceNames = a} :: DetachInstancesFromLoadBalancer) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    DetachInstancesFromLoadBalancer
  where
  type
    AWSResponse DetachInstancesFromLoadBalancer =
      DetachInstancesFromLoadBalancerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachInstancesFromLoadBalancerResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DetachInstancesFromLoadBalancer
  where
  hashWithSalt
    _salt
    DetachInstancesFromLoadBalancer' {..} =
      _salt `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` instanceNames

instance
  Prelude.NFData
    DetachInstancesFromLoadBalancer
  where
  rnf DetachInstancesFromLoadBalancer' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf instanceNames

instance
  Core.ToHeaders
    DetachInstancesFromLoadBalancer
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DetachInstancesFromLoadBalancer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DetachInstancesFromLoadBalancer where
  toJSON DetachInstancesFromLoadBalancer' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("loadBalancerName" Core..= loadBalancerName),
            Prelude.Just
              ("instanceNames" Core..= instanceNames)
          ]
      )

instance Core.ToPath DetachInstancesFromLoadBalancer where
  toPath = Prelude.const "/"

instance Core.ToQuery DetachInstancesFromLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachInstancesFromLoadBalancerResponse' smart constructor.
data DetachInstancesFromLoadBalancerResponse = DetachInstancesFromLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetachInstancesFromLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'detachInstancesFromLoadBalancerResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'detachInstancesFromLoadBalancerResponse_httpStatus' - The response's http status code.
newDetachInstancesFromLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachInstancesFromLoadBalancerResponse
newDetachInstancesFromLoadBalancerResponse
  pHttpStatus_ =
    DetachInstancesFromLoadBalancerResponse'
      { operations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
detachInstancesFromLoadBalancerResponse_operations :: Lens.Lens' DetachInstancesFromLoadBalancerResponse (Prelude.Maybe [Operation])
detachInstancesFromLoadBalancerResponse_operations = Lens.lens (\DetachInstancesFromLoadBalancerResponse' {operations} -> operations) (\s@DetachInstancesFromLoadBalancerResponse' {} a -> s {operations = a} :: DetachInstancesFromLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detachInstancesFromLoadBalancerResponse_httpStatus :: Lens.Lens' DetachInstancesFromLoadBalancerResponse Prelude.Int
detachInstancesFromLoadBalancerResponse_httpStatus = Lens.lens (\DetachInstancesFromLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DetachInstancesFromLoadBalancerResponse' {} a -> s {httpStatus = a} :: DetachInstancesFromLoadBalancerResponse)

instance
  Prelude.NFData
    DetachInstancesFromLoadBalancerResponse
  where
  rnf DetachInstancesFromLoadBalancerResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
