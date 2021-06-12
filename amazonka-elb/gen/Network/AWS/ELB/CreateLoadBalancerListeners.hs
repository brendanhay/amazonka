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
-- Module      : Network.AWS.ELB.CreateLoadBalancerListeners
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more listeners for the specified load balancer. If a
-- listener with the specified port does not already exist, it is created;
-- otherwise, the properties of the new listener must match the properties
-- of the existing listener.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-listener-config.html Listeners for Your Classic Load Balancer>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.CreateLoadBalancerListeners
  ( -- * Creating a Request
    CreateLoadBalancerListeners (..),
    newCreateLoadBalancerListeners,

    -- * Request Lenses
    createLoadBalancerListeners_loadBalancerName,
    createLoadBalancerListeners_listeners,

    -- * Destructuring the Response
    CreateLoadBalancerListenersResponse (..),
    newCreateLoadBalancerListenersResponse,

    -- * Response Lenses
    createLoadBalancerListenersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CreateLoadBalancerListeners.
--
-- /See:/ 'newCreateLoadBalancerListeners' smart constructor.
data CreateLoadBalancerListeners = CreateLoadBalancerListeners'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The listeners.
    listeners :: [Listener]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoadBalancerListeners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'createLoadBalancerListeners_loadBalancerName' - The name of the load balancer.
--
-- 'listeners', 'createLoadBalancerListeners_listeners' - The listeners.
newCreateLoadBalancerListeners ::
  -- | 'loadBalancerName'
  Core.Text ->
  CreateLoadBalancerListeners
newCreateLoadBalancerListeners pLoadBalancerName_ =
  CreateLoadBalancerListeners'
    { loadBalancerName =
        pLoadBalancerName_,
      listeners = Core.mempty
    }

-- | The name of the load balancer.
createLoadBalancerListeners_loadBalancerName :: Lens.Lens' CreateLoadBalancerListeners Core.Text
createLoadBalancerListeners_loadBalancerName = Lens.lens (\CreateLoadBalancerListeners' {loadBalancerName} -> loadBalancerName) (\s@CreateLoadBalancerListeners' {} a -> s {loadBalancerName = a} :: CreateLoadBalancerListeners)

-- | The listeners.
createLoadBalancerListeners_listeners :: Lens.Lens' CreateLoadBalancerListeners [Listener]
createLoadBalancerListeners_listeners = Lens.lens (\CreateLoadBalancerListeners' {listeners} -> listeners) (\s@CreateLoadBalancerListeners' {} a -> s {listeners = a} :: CreateLoadBalancerListeners) Core.. Lens._Coerce

instance Core.AWSRequest CreateLoadBalancerListeners where
  type
    AWSResponse CreateLoadBalancerListeners =
      CreateLoadBalancerListenersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerListenersResult"
      ( \s h x ->
          CreateLoadBalancerListenersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateLoadBalancerListeners

instance Core.NFData CreateLoadBalancerListeners

instance Core.ToHeaders CreateLoadBalancerListeners where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateLoadBalancerListeners where
  toPath = Core.const "/"

instance Core.ToQuery CreateLoadBalancerListeners where
  toQuery CreateLoadBalancerListeners' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateLoadBalancerListeners" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "Listeners"
          Core.=: Core.toQueryList "member" listeners
      ]

-- | Contains the parameters for CreateLoadBalancerListener.
--
-- /See:/ 'newCreateLoadBalancerListenersResponse' smart constructor.
data CreateLoadBalancerListenersResponse = CreateLoadBalancerListenersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLoadBalancerListenersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createLoadBalancerListenersResponse_httpStatus' - The response's http status code.
newCreateLoadBalancerListenersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateLoadBalancerListenersResponse
newCreateLoadBalancerListenersResponse pHttpStatus_ =
  CreateLoadBalancerListenersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createLoadBalancerListenersResponse_httpStatus :: Lens.Lens' CreateLoadBalancerListenersResponse Core.Int
createLoadBalancerListenersResponse_httpStatus = Lens.lens (\CreateLoadBalancerListenersResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerListenersResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerListenersResponse)

instance
  Core.NFData
    CreateLoadBalancerListenersResponse
