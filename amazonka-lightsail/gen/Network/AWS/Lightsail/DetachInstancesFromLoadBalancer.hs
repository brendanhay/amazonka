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
-- Module      : Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DetachInstancesFromLoadBalancer
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachInstancesFromLoadBalancer' smart constructor.
data DetachInstancesFromLoadBalancer = DetachInstancesFromLoadBalancer'
  { -- | The name of the Lightsail load balancer.
    loadBalancerName :: Core.Text,
    -- | An array of strings containing the names of the instances you want to
    -- detach from the load balancer.
    instanceNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DetachInstancesFromLoadBalancer
newDetachInstancesFromLoadBalancer pLoadBalancerName_ =
  DetachInstancesFromLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instanceNames = Core.mempty
    }

-- | The name of the Lightsail load balancer.
detachInstancesFromLoadBalancer_loadBalancerName :: Lens.Lens' DetachInstancesFromLoadBalancer Core.Text
detachInstancesFromLoadBalancer_loadBalancerName = Lens.lens (\DetachInstancesFromLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DetachInstancesFromLoadBalancer' {} a -> s {loadBalancerName = a} :: DetachInstancesFromLoadBalancer)

-- | An array of strings containing the names of the instances you want to
-- detach from the load balancer.
detachInstancesFromLoadBalancer_instanceNames :: Lens.Lens' DetachInstancesFromLoadBalancer [Core.Text]
detachInstancesFromLoadBalancer_instanceNames = Lens.lens (\DetachInstancesFromLoadBalancer' {instanceNames} -> instanceNames) (\s@DetachInstancesFromLoadBalancer' {} a -> s {instanceNames = a} :: DetachInstancesFromLoadBalancer) Core.. Lens._Coerce

instance
  Core.AWSRequest
    DetachInstancesFromLoadBalancer
  where
  type
    AWSResponse DetachInstancesFromLoadBalancer =
      DetachInstancesFromLoadBalancerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachInstancesFromLoadBalancerResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DetachInstancesFromLoadBalancer

instance Core.NFData DetachInstancesFromLoadBalancer

instance
  Core.ToHeaders
    DetachInstancesFromLoadBalancer
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DetachInstancesFromLoadBalancer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DetachInstancesFromLoadBalancer where
  toJSON DetachInstancesFromLoadBalancer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loadBalancerName" Core..= loadBalancerName),
            Core.Just ("instanceNames" Core..= instanceNames)
          ]
      )

instance Core.ToPath DetachInstancesFromLoadBalancer where
  toPath = Core.const "/"

instance Core.ToQuery DetachInstancesFromLoadBalancer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetachInstancesFromLoadBalancerResponse' smart constructor.
data DetachInstancesFromLoadBalancerResponse = DetachInstancesFromLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DetachInstancesFromLoadBalancerResponse
newDetachInstancesFromLoadBalancerResponse
  pHttpStatus_ =
    DetachInstancesFromLoadBalancerResponse'
      { operations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
detachInstancesFromLoadBalancerResponse_operations :: Lens.Lens' DetachInstancesFromLoadBalancerResponse (Core.Maybe [Operation])
detachInstancesFromLoadBalancerResponse_operations = Lens.lens (\DetachInstancesFromLoadBalancerResponse' {operations} -> operations) (\s@DetachInstancesFromLoadBalancerResponse' {} a -> s {operations = a} :: DetachInstancesFromLoadBalancerResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
detachInstancesFromLoadBalancerResponse_httpStatus :: Lens.Lens' DetachInstancesFromLoadBalancerResponse Core.Int
detachInstancesFromLoadBalancerResponse_httpStatus = Lens.lens (\DetachInstancesFromLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DetachInstancesFromLoadBalancerResponse' {} a -> s {httpStatus = a} :: DetachInstancesFromLoadBalancerResponse)

instance
  Core.NFData
    DetachInstancesFromLoadBalancerResponse
