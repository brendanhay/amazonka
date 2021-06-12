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
-- Module      : Network.AWS.ELBv2.DeleteLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Application Load Balancer, Network Load Balancer,
-- or Gateway Load Balancer. Deleting a load balancer also deletes its
-- listeners.
--
-- You can\'t delete a load balancer if deletion protection is enabled. If
-- the load balancer does not exist or has already been deleted, the call
-- succeeds.
--
-- Deleting a load balancer does not affect its registered targets. For
-- example, your EC2 instances continue to run and are still registered to
-- their target groups. If you no longer need these EC2 instances, you can
-- stop or terminate them.
module Network.AWS.ELBv2.DeleteLoadBalancer
  ( -- * Creating a Request
    DeleteLoadBalancer (..),
    newDeleteLoadBalancer,

    -- * Request Lenses
    deleteLoadBalancer_loadBalancerArn,

    -- * Destructuring the Response
    DeleteLoadBalancerResponse (..),
    newDeleteLoadBalancerResponse,

    -- * Response Lenses
    deleteLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLoadBalancer' smart constructor.
data DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'deleteLoadBalancer_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
newDeleteLoadBalancer ::
  -- | 'loadBalancerArn'
  Core.Text ->
  DeleteLoadBalancer
newDeleteLoadBalancer pLoadBalancerArn_ =
  DeleteLoadBalancer'
    { loadBalancerArn =
        pLoadBalancerArn_
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
deleteLoadBalancer_loadBalancerArn :: Lens.Lens' DeleteLoadBalancer Core.Text
deleteLoadBalancer_loadBalancerArn = Lens.lens (\DeleteLoadBalancer' {loadBalancerArn} -> loadBalancerArn) (\s@DeleteLoadBalancer' {} a -> s {loadBalancerArn = a} :: DeleteLoadBalancer)

instance Core.AWSRequest DeleteLoadBalancer where
  type
    AWSResponse DeleteLoadBalancer =
      DeleteLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerResult"
      ( \s h x ->
          DeleteLoadBalancerResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLoadBalancer

instance Core.NFData DeleteLoadBalancer

instance Core.ToHeaders DeleteLoadBalancer where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteLoadBalancer where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLoadBalancer where
  toQuery DeleteLoadBalancer' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteLoadBalancer" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "LoadBalancerArn" Core.=: loadBalancerArn
      ]

-- | /See:/ 'newDeleteLoadBalancerResponse' smart constructor.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteLoadBalancerResponse
newDeleteLoadBalancerResponse pHttpStatus_ =
  DeleteLoadBalancerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoadBalancerResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerResponse Core.Int
deleteLoadBalancerResponse_httpStatus = Lens.lens (\DeleteLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerResponse)

instance Core.NFData DeleteLoadBalancerResponse
