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
-- Module      : Amazonka.Lightsail.GetLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified Lightsail load balancer.
module Amazonka.Lightsail.GetLoadBalancer
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoadBalancer' smart constructor.
data GetLoadBalancer = GetLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetLoadBalancer
newGetLoadBalancer pLoadBalancerName_ =
  GetLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer.
getLoadBalancer_loadBalancerName :: Lens.Lens' GetLoadBalancer Prelude.Text
getLoadBalancer_loadBalancerName = Lens.lens (\GetLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@GetLoadBalancer' {} a -> s {loadBalancerName = a} :: GetLoadBalancer)

instance Core.AWSRequest GetLoadBalancer where
  type
    AWSResponse GetLoadBalancer =
      GetLoadBalancerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancerResponse'
            Prelude.<$> (x Data..?> "loadBalancer")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLoadBalancer where
  hashWithSalt _salt GetLoadBalancer' {..} =
    _salt `Prelude.hashWithSalt` loadBalancerName

instance Prelude.NFData GetLoadBalancer where
  rnf GetLoadBalancer' {..} =
    Prelude.rnf loadBalancerName

instance Data.ToHeaders GetLoadBalancer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetLoadBalancer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLoadBalancer where
  toJSON GetLoadBalancer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("loadBalancerName" Data..= loadBalancerName)
          ]
      )

instance Data.ToPath GetLoadBalancer where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoadBalancerResponse' smart constructor.
data GetLoadBalancerResponse = GetLoadBalancerResponse'
  { -- | An object containing information about your load balancer.
    loadBalancer :: Prelude.Maybe LoadBalancer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetLoadBalancerResponse
newGetLoadBalancerResponse pHttpStatus_ =
  GetLoadBalancerResponse'
    { loadBalancer =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing information about your load balancer.
getLoadBalancerResponse_loadBalancer :: Lens.Lens' GetLoadBalancerResponse (Prelude.Maybe LoadBalancer)
getLoadBalancerResponse_loadBalancer = Lens.lens (\GetLoadBalancerResponse' {loadBalancer} -> loadBalancer) (\s@GetLoadBalancerResponse' {} a -> s {loadBalancer = a} :: GetLoadBalancerResponse)

-- | The response's http status code.
getLoadBalancerResponse_httpStatus :: Lens.Lens' GetLoadBalancerResponse Prelude.Int
getLoadBalancerResponse_httpStatus = Lens.lens (\GetLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancerResponse' {} a -> s {httpStatus = a} :: GetLoadBalancerResponse)

instance Prelude.NFData GetLoadBalancerResponse where
  rnf GetLoadBalancerResponse' {..} =
    Prelude.rnf loadBalancer
      `Prelude.seq` Prelude.rnf httpStatus
