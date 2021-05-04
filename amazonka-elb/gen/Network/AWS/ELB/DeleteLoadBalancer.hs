{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ELB.DeleteLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified load balancer.
--
-- If you are attempting to recreate a load balancer, you must reconfigure
-- all settings. The DNS name associated with a deleted load balancer are
-- no longer usable. The name and associated DNS record of the deleted load
-- balancer no longer exist and traffic sent to any of its IP addresses is
-- no longer delivered to your instances.
--
-- If the load balancer does not exist or has already been deleted, the
-- call to @DeleteLoadBalancer@ still succeeds.
module Network.AWS.ELB.DeleteLoadBalancer
  ( -- * Creating a Request
    DeleteLoadBalancer (..),
    newDeleteLoadBalancer,

    -- * Request Lenses
    deleteLoadBalancer_loadBalancerName,

    -- * Destructuring the Response
    DeleteLoadBalancerResponse (..),
    newDeleteLoadBalancerResponse,

    -- * Response Lenses
    deleteLoadBalancerResponse_httpStatus,
  )
where

import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteLoadBalancer.
--
-- /See:/ 'newDeleteLoadBalancer' smart constructor.
data DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'deleteLoadBalancer_loadBalancerName' - The name of the load balancer.
newDeleteLoadBalancer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  DeleteLoadBalancer
newDeleteLoadBalancer pLoadBalancerName_ =
  DeleteLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer.
deleteLoadBalancer_loadBalancerName :: Lens.Lens' DeleteLoadBalancer Prelude.Text
deleteLoadBalancer_loadBalancerName = Lens.lens (\DeleteLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DeleteLoadBalancer' {} a -> s {loadBalancerName = a} :: DeleteLoadBalancer)

instance Prelude.AWSRequest DeleteLoadBalancer where
  type
    Rs DeleteLoadBalancer =
      DeleteLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerResult"
      ( \s h x ->
          DeleteLoadBalancerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoadBalancer

instance Prelude.NFData DeleteLoadBalancer

instance Prelude.ToHeaders DeleteLoadBalancer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteLoadBalancer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLoadBalancer where
  toQuery DeleteLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteLoadBalancer" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Prelude.=: loadBalancerName
      ]

-- | Contains the output of DeleteLoadBalancer.
--
-- /See:/ 'newDeleteLoadBalancerResponse' smart constructor.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLoadBalancerResponse_httpStatus' - The response's http status code.
newDeleteLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLoadBalancerResponse
newDeleteLoadBalancerResponse pHttpStatus_ =
  DeleteLoadBalancerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoadBalancerResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerResponse Prelude.Int
deleteLoadBalancerResponse_httpStatus = Lens.lens (\DeleteLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerResponse)

instance Prelude.NFData DeleteLoadBalancerResponse
