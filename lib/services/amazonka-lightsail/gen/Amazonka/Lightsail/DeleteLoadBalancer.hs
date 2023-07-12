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
-- Module      : Amazonka.Lightsail.DeleteLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.DeleteLoadBalancer
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLoadBalancer' smart constructor.
data DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The name of the load balancer you want to delete.
    loadBalancerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteLoadBalancer
newDeleteLoadBalancer pLoadBalancerName_ =
  DeleteLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer you want to delete.
deleteLoadBalancer_loadBalancerName :: Lens.Lens' DeleteLoadBalancer Prelude.Text
deleteLoadBalancer_loadBalancerName = Lens.lens (\DeleteLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@DeleteLoadBalancer' {} a -> s {loadBalancerName = a} :: DeleteLoadBalancer)

instance Core.AWSRequest DeleteLoadBalancer where
  type
    AWSResponse DeleteLoadBalancer =
      DeleteLoadBalancerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteLoadBalancerResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoadBalancer where
  hashWithSalt _salt DeleteLoadBalancer' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerName

instance Prelude.NFData DeleteLoadBalancer where
  rnf DeleteLoadBalancer' {..} =
    Prelude.rnf loadBalancerName

instance Data.ToHeaders DeleteLoadBalancer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.DeleteLoadBalancer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLoadBalancer where
  toJSON DeleteLoadBalancer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("loadBalancerName" Data..= loadBalancerName)
          ]
      )

instance Data.ToPath DeleteLoadBalancer where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLoadBalancerResponse' smart constructor.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteLoadBalancerResponse
newDeleteLoadBalancerResponse pHttpStatus_ =
  DeleteLoadBalancerResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteLoadBalancerResponse_operations :: Lens.Lens' DeleteLoadBalancerResponse (Prelude.Maybe [Operation])
deleteLoadBalancerResponse_operations = Lens.lens (\DeleteLoadBalancerResponse' {operations} -> operations) (\s@DeleteLoadBalancerResponse' {} a -> s {operations = a} :: DeleteLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteLoadBalancerResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerResponse Prelude.Int
deleteLoadBalancerResponse_httpStatus = Lens.lens (\DeleteLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerResponse)

instance Prelude.NFData DeleteLoadBalancerResponse where
  rnf DeleteLoadBalancerResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
