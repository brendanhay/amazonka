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
-- Module      : Network.AWS.Lightsail.AttachInstancesToLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more Lightsail instances to a load balancer.
--
-- After some time, the instances are attached to the load balancer and the
-- health check status is available.
--
-- The @attach instances to load balancer@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.AttachInstancesToLoadBalancer
  ( -- * Creating a Request
    AttachInstancesToLoadBalancer (..),
    newAttachInstancesToLoadBalancer,

    -- * Request Lenses
    attachInstancesToLoadBalancer_loadBalancerName,
    attachInstancesToLoadBalancer_instanceNames,

    -- * Destructuring the Response
    AttachInstancesToLoadBalancerResponse (..),
    newAttachInstancesToLoadBalancerResponse,

    -- * Response Lenses
    attachInstancesToLoadBalancerResponse_operations,
    attachInstancesToLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachInstancesToLoadBalancer' smart constructor.
data AttachInstancesToLoadBalancer = AttachInstancesToLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | An array of strings representing the instance name(s) you want to attach
    -- to your load balancer.
    --
    -- An instance must be @running@ before you can attach it to your load
    -- balancer.
    --
    -- There are no additional limits on the number of instances you can attach
    -- to your load balancer, aside from the limit of Lightsail instances you
    -- can create in your account (20).
    instanceNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachInstancesToLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'attachInstancesToLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- 'instanceNames', 'attachInstancesToLoadBalancer_instanceNames' - An array of strings representing the instance name(s) you want to attach
-- to your load balancer.
--
-- An instance must be @running@ before you can attach it to your load
-- balancer.
--
-- There are no additional limits on the number of instances you can attach
-- to your load balancer, aside from the limit of Lightsail instances you
-- can create in your account (20).
newAttachInstancesToLoadBalancer ::
  -- | 'loadBalancerName'
  Core.Text ->
  AttachInstancesToLoadBalancer
newAttachInstancesToLoadBalancer pLoadBalancerName_ =
  AttachInstancesToLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instanceNames = Core.mempty
    }

-- | The name of the load balancer.
attachInstancesToLoadBalancer_loadBalancerName :: Lens.Lens' AttachInstancesToLoadBalancer Core.Text
attachInstancesToLoadBalancer_loadBalancerName = Lens.lens (\AttachInstancesToLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@AttachInstancesToLoadBalancer' {} a -> s {loadBalancerName = a} :: AttachInstancesToLoadBalancer)

-- | An array of strings representing the instance name(s) you want to attach
-- to your load balancer.
--
-- An instance must be @running@ before you can attach it to your load
-- balancer.
--
-- There are no additional limits on the number of instances you can attach
-- to your load balancer, aside from the limit of Lightsail instances you
-- can create in your account (20).
attachInstancesToLoadBalancer_instanceNames :: Lens.Lens' AttachInstancesToLoadBalancer [Core.Text]
attachInstancesToLoadBalancer_instanceNames = Lens.lens (\AttachInstancesToLoadBalancer' {instanceNames} -> instanceNames) (\s@AttachInstancesToLoadBalancer' {} a -> s {instanceNames = a} :: AttachInstancesToLoadBalancer) Core.. Lens._Coerce

instance
  Core.AWSRequest
    AttachInstancesToLoadBalancer
  where
  type
    AWSResponse AttachInstancesToLoadBalancer =
      AttachInstancesToLoadBalancerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachInstancesToLoadBalancerResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachInstancesToLoadBalancer

instance Core.NFData AttachInstancesToLoadBalancer

instance Core.ToHeaders AttachInstancesToLoadBalancer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.AttachInstancesToLoadBalancer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AttachInstancesToLoadBalancer where
  toJSON AttachInstancesToLoadBalancer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loadBalancerName" Core..= loadBalancerName),
            Core.Just ("instanceNames" Core..= instanceNames)
          ]
      )

instance Core.ToPath AttachInstancesToLoadBalancer where
  toPath = Core.const "/"

instance Core.ToQuery AttachInstancesToLoadBalancer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAttachInstancesToLoadBalancerResponse' smart constructor.
data AttachInstancesToLoadBalancerResponse = AttachInstancesToLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachInstancesToLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'attachInstancesToLoadBalancerResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'attachInstancesToLoadBalancerResponse_httpStatus' - The response's http status code.
newAttachInstancesToLoadBalancerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AttachInstancesToLoadBalancerResponse
newAttachInstancesToLoadBalancerResponse pHttpStatus_ =
  AttachInstancesToLoadBalancerResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
attachInstancesToLoadBalancerResponse_operations :: Lens.Lens' AttachInstancesToLoadBalancerResponse (Core.Maybe [Operation])
attachInstancesToLoadBalancerResponse_operations = Lens.lens (\AttachInstancesToLoadBalancerResponse' {operations} -> operations) (\s@AttachInstancesToLoadBalancerResponse' {} a -> s {operations = a} :: AttachInstancesToLoadBalancerResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
attachInstancesToLoadBalancerResponse_httpStatus :: Lens.Lens' AttachInstancesToLoadBalancerResponse Core.Int
attachInstancesToLoadBalancerResponse_httpStatus = Lens.lens (\AttachInstancesToLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@AttachInstancesToLoadBalancerResponse' {} a -> s {httpStatus = a} :: AttachInstancesToLoadBalancerResponse)

instance
  Core.NFData
    AttachInstancesToLoadBalancerResponse
