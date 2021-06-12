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
-- Module      : Network.AWS.Lightsail.GetLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Lightsail load balancer.
module Network.AWS.Lightsail.GetLoadBalancer
  ( -- * Creating a Request
    GetLoadBalancer (..),
    newGetLoadBalancer,

    -- * Request Lenses
    getLoadBalancer_loadBalancerName,

    -- * Destructuring the Response
    GetLoadBalancerResponse (..),
    newGetLoadBalancerResponse,

    -- * Response Lenses
    getLoadBalancerResponse_loadBalancer,
    getLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLoadBalancer' smart constructor.
data GetLoadBalancer = GetLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'getLoadBalancer_loadBalancerName' - The name of the load balancer.
newGetLoadBalancer ::
  -- | 'loadBalancerName'
  Core.Text ->
  GetLoadBalancer
newGetLoadBalancer pLoadBalancerName_ =
  GetLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer.
getLoadBalancer_loadBalancerName :: Lens.Lens' GetLoadBalancer Core.Text
getLoadBalancer_loadBalancerName = Lens.lens (\GetLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@GetLoadBalancer' {} a -> s {loadBalancerName = a} :: GetLoadBalancer)

instance Core.AWSRequest GetLoadBalancer where
  type
    AWSResponse GetLoadBalancer =
      GetLoadBalancerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancerResponse'
            Core.<$> (x Core..?> "loadBalancer")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLoadBalancer

instance Core.NFData GetLoadBalancer

instance Core.ToHeaders GetLoadBalancer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetLoadBalancer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetLoadBalancer where
  toJSON GetLoadBalancer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loadBalancerName" Core..= loadBalancerName)
          ]
      )

instance Core.ToPath GetLoadBalancer where
  toPath = Core.const "/"

instance Core.ToQuery GetLoadBalancer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLoadBalancerResponse' smart constructor.
data GetLoadBalancerResponse = GetLoadBalancerResponse'
  { -- | An object containing information about your load balancer.
    loadBalancer :: Core.Maybe LoadBalancer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancer', 'getLoadBalancerResponse_loadBalancer' - An object containing information about your load balancer.
--
-- 'httpStatus', 'getLoadBalancerResponse_httpStatus' - The response's http status code.
newGetLoadBalancerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetLoadBalancerResponse
newGetLoadBalancerResponse pHttpStatus_ =
  GetLoadBalancerResponse'
    { loadBalancer =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing information about your load balancer.
getLoadBalancerResponse_loadBalancer :: Lens.Lens' GetLoadBalancerResponse (Core.Maybe LoadBalancer)
getLoadBalancerResponse_loadBalancer = Lens.lens (\GetLoadBalancerResponse' {loadBalancer} -> loadBalancer) (\s@GetLoadBalancerResponse' {} a -> s {loadBalancer = a} :: GetLoadBalancerResponse)

-- | The response's http status code.
getLoadBalancerResponse_httpStatus :: Lens.Lens' GetLoadBalancerResponse Core.Int
getLoadBalancerResponse_httpStatus = Lens.lens (\GetLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancerResponse' {} a -> s {httpStatus = a} :: GetLoadBalancerResponse)

instance Core.NFData GetLoadBalancerResponse
