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
-- Module      : Network.AWS.Lightsail.DeleteLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Lightsail load balancer and all its associated SSL\/TLS
-- certificates. Once the load balancer is deleted, you will need to create
-- a new load balancer, create a new certificate, and verify domain
-- ownership again.
--
-- The @delete load balancer@ operation supports tag-based access control
-- via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.DeleteLoadBalancer
  ( -- * Creating a Request
    DeleteLoadBalancer (..),
    newDeleteLoadBalancer,

    -- * Request Lenses
    deleteLoadBalancer_loadBalancerName,

    -- * Destructuring the Response
    DeleteLoadBalancerResponse (..),
    newDeleteLoadBalancerResponse,

    -- * Response Lenses
    deleteLoadBalancerResponse_operations,
    deleteLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLoadBalancer' smart constructor.
data DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The name of the load balancer you want to delete.
    loadBalancerName :: Core.Text
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
-- 'loadBalancerName', 'deleteLoadBalancer_loadBalancerName' - The name of the load balancer you want to delete.
newDeleteLoadBalancer ::
  -- | 'loadBalancerName'
  Core.Text ->
  DeleteLoadBalancer
newDeleteLoadBalancer pLoadBalancerName_ =
  DeleteLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer you want to delete.
deleteLoadBalancer_loadBalancerName :: Lens.Lens' DeleteLoadBalancer Core.Text
deleteLoadBalancer_loadBalancerName = Lens.lens (\DeleteLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DeleteLoadBalancer' {} a -> s {loadBalancerName = a} :: DeleteLoadBalancer)

instance Core.AWSRequest DeleteLoadBalancer where
  type
    AWSResponse DeleteLoadBalancer =
      DeleteLoadBalancerResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLoadBalancerResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteLoadBalancer

instance Core.NFData DeleteLoadBalancer

instance Core.ToHeaders DeleteLoadBalancer where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteLoadBalancer" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteLoadBalancer where
  toJSON DeleteLoadBalancer' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loadBalancerName" Core..= loadBalancerName)
          ]
      )

instance Core.ToPath DeleteLoadBalancer where
  toPath = Core.const "/"

instance Core.ToQuery DeleteLoadBalancer where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteLoadBalancerResponse' smart constructor.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
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
-- 'operations', 'deleteLoadBalancerResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteLoadBalancerResponse_httpStatus' - The response's http status code.
newDeleteLoadBalancerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteLoadBalancerResponse
newDeleteLoadBalancerResponse pHttpStatus_ =
  DeleteLoadBalancerResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteLoadBalancerResponse_operations :: Lens.Lens' DeleteLoadBalancerResponse (Core.Maybe [Operation])
deleteLoadBalancerResponse_operations = Lens.lens (\DeleteLoadBalancerResponse' {operations} -> operations) (\s@DeleteLoadBalancerResponse' {} a -> s {operations = a} :: DeleteLoadBalancerResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteLoadBalancerResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerResponse Core.Int
deleteLoadBalancerResponse_httpStatus = Lens.lens (\DeleteLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerResponse)

instance Core.NFData DeleteLoadBalancerResponse
