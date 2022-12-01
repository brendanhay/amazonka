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
-- Module      : Amazonka.ELB.DeleteLoadBalancerListeners
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified listeners from the specified load balancer.
module Amazonka.ELB.DeleteLoadBalancerListeners
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DeleteLoadBalancerListeners.
--
-- /See:/ 'newDeleteLoadBalancerListeners' smart constructor.
data DeleteLoadBalancerListeners = DeleteLoadBalancerListeners'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The client port numbers of the listeners.
    loadBalancerPorts :: [Prelude.Int]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteLoadBalancerListeners
newDeleteLoadBalancerListeners pLoadBalancerName_ =
  DeleteLoadBalancerListeners'
    { loadBalancerName =
        pLoadBalancerName_,
      loadBalancerPorts = Prelude.mempty
    }

-- | The name of the load balancer.
deleteLoadBalancerListeners_loadBalancerName :: Lens.Lens' DeleteLoadBalancerListeners Prelude.Text
deleteLoadBalancerListeners_loadBalancerName = Lens.lens (\DeleteLoadBalancerListeners' {loadBalancerName} -> loadBalancerName) (\s@DeleteLoadBalancerListeners' {} a -> s {loadBalancerName = a} :: DeleteLoadBalancerListeners)

-- | The client port numbers of the listeners.
deleteLoadBalancerListeners_loadBalancerPorts :: Lens.Lens' DeleteLoadBalancerListeners [Prelude.Int]
deleteLoadBalancerListeners_loadBalancerPorts = Lens.lens (\DeleteLoadBalancerListeners' {loadBalancerPorts} -> loadBalancerPorts) (\s@DeleteLoadBalancerListeners' {} a -> s {loadBalancerPorts = a} :: DeleteLoadBalancerListeners) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteLoadBalancerListeners where
  type
    AWSResponse DeleteLoadBalancerListeners =
      DeleteLoadBalancerListenersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerListenersResult"
      ( \s h x ->
          DeleteLoadBalancerListenersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoadBalancerListeners where
  hashWithSalt _salt DeleteLoadBalancerListeners' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` loadBalancerPorts

instance Prelude.NFData DeleteLoadBalancerListeners where
  rnf DeleteLoadBalancerListeners' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf loadBalancerPorts

instance Core.ToHeaders DeleteLoadBalancerListeners where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteLoadBalancerListeners where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLoadBalancerListeners where
  toQuery DeleteLoadBalancerListeners' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DeleteLoadBalancerListeners" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "LoadBalancerPorts"
          Core.=: Core.toQueryList "member" loadBalancerPorts
      ]

-- | Contains the output of DeleteLoadBalancerListeners.
--
-- /See:/ 'newDeleteLoadBalancerListenersResponse' smart constructor.
data DeleteLoadBalancerListenersResponse = DeleteLoadBalancerListenersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteLoadBalancerListenersResponse
newDeleteLoadBalancerListenersResponse pHttpStatus_ =
  DeleteLoadBalancerListenersResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoadBalancerListenersResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerListenersResponse Prelude.Int
deleteLoadBalancerListenersResponse_httpStatus = Lens.lens (\DeleteLoadBalancerListenersResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerListenersResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerListenersResponse)

instance
  Prelude.NFData
    DeleteLoadBalancerListenersResponse
  where
  rnf DeleteLoadBalancerListenersResponse' {..} =
    Prelude.rnf httpStatus
