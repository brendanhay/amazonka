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
-- Module      : Network.AWS.ELB.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listeners from the specified load balancer.
module Network.AWS.ELB.DeleteLoadBalancerListeners
  ( -- * Creating a Request
    DeleteLoadBalancerListeners (..),
    newDeleteLoadBalancerListeners,

    -- * Request Lenses
    deleteLoadBalancerListeners_loadBalancerName,
    deleteLoadBalancerListeners_loadBalancerPorts,

    -- * Destructuring the Response
    DeleteLoadBalancerListenersResponse (..),
    newDeleteLoadBalancerListenersResponse,

    -- * Response Lenses
    deleteLoadBalancerListenersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteLoadBalancerListeners.
--
-- /See:/ 'newDeleteLoadBalancerListeners' smart constructor.
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The client port numbers of the listeners.
    loadBalancerPorts :: [Core.Int]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerListeners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'deleteLoadBalancerListeners_loadBalancerName' - The name of the load balancer.
--
-- 'loadBalancerPorts', 'deleteLoadBalancerListeners_loadBalancerPorts' - The client port numbers of the listeners.
newDeleteLoadBalancerListeners ::
  -- | 'loadBalancerName'
  Core.Text ->
  DeleteLoadBalancerListeners
newDeleteLoadBalancerListeners pLoadBalancerName_ =
  DeleteLoadBalancerListeners'
    { loadBalancerName =
        pLoadBalancerName_,
      loadBalancerPorts = Core.mempty
    }

-- | The name of the load balancer.
deleteLoadBalancerListeners_loadBalancerName :: Lens.Lens' DeleteLoadBalancerListeners Core.Text
deleteLoadBalancerListeners_loadBalancerName = Lens.lens (\DeleteLoadBalancerListeners' {loadBalancerName} -> loadBalancerName) (\s@DeleteLoadBalancerListeners' {} a -> s {loadBalancerName = a} :: DeleteLoadBalancerListeners)

-- | The client port numbers of the listeners.
deleteLoadBalancerListeners_loadBalancerPorts :: Lens.Lens' DeleteLoadBalancerListeners [Core.Int]
deleteLoadBalancerListeners_loadBalancerPorts = Lens.lens (\DeleteLoadBalancerListeners' {loadBalancerPorts} -> loadBalancerPorts) (\s@DeleteLoadBalancerListeners' {} a -> s {loadBalancerPorts = a} :: DeleteLoadBalancerListeners) Core.. Lens._Coerce

instance Core.AWSRequest DeleteLoadBalancerListeners where
  type
    AWSResponse DeleteLoadBalancerListeners =
      DeleteLoadBalancerListenersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerListenersResult"
      ( \s h x ->
          DeleteLoadBalancerListenersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLoadBalancerListeners

instance Core.NFData DeleteLoadBalancerListeners

instance Core.ToHeaders DeleteLoadBalancerListeners where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteLoadBalancerListeners where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLoadBalancerListeners where
  toQuery DeleteLoadBalancerListeners' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteLoadBalancerListeners" :: Core.ByteString),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "LoadBalancerPorts"
          Core.=: Core.toQueryList "member" loadBalancerPorts
      ]

-- | Contains the output of DeleteLoadBalancerListeners.
--
-- /See:/ 'newDeleteLoadBalancerListenersResponse' smart constructor.
data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerListenersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLoadBalancerListenersResponse_httpStatus' - The response's http status code.
newDeleteLoadBalancerListenersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLoadBalancerListenersResponse
newDeleteLoadBalancerListenersResponse pHttpStatus_ =
  DeleteLoadBalancerListenersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoadBalancerListenersResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerListenersResponse Core.Int
deleteLoadBalancerListenersResponse_httpStatus = Lens.lens (\DeleteLoadBalancerListenersResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerListenersResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerListenersResponse)

instance
  Core.NFData
    DeleteLoadBalancerListenersResponse
