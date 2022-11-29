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
-- Module      : Amazonka.Lightsail.GetLoadBalancerTlsCertificates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the TLS certificates that are associated with
-- the specified Lightsail load balancer.
--
-- TLS is just an updated, more secure version of Secure Socket Layer
-- (SSL).
--
-- You can have a maximum of 2 certificates associated with a Lightsail
-- load balancer. One is active and the other is inactive.
module Amazonka.Lightsail.GetLoadBalancerTlsCertificates
  ( -- * Creating a Request
    GetLoadBalancerTlsCertificates (..),
    newGetLoadBalancerTlsCertificates,

    -- * Request Lenses
    getLoadBalancerTlsCertificates_loadBalancerName,

    -- * Destructuring the Response
    GetLoadBalancerTlsCertificatesResponse (..),
    newGetLoadBalancerTlsCertificatesResponse,

    -- * Response Lenses
    getLoadBalancerTlsCertificatesResponse_tlsCertificates,
    getLoadBalancerTlsCertificatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLoadBalancerTlsCertificates' smart constructor.
data GetLoadBalancerTlsCertificates = GetLoadBalancerTlsCertificates'
  { -- | The name of the load balancer you associated with your SSL\/TLS
    -- certificate.
    loadBalancerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoadBalancerTlsCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'getLoadBalancerTlsCertificates_loadBalancerName' - The name of the load balancer you associated with your SSL\/TLS
-- certificate.
newGetLoadBalancerTlsCertificates ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  GetLoadBalancerTlsCertificates
newGetLoadBalancerTlsCertificates pLoadBalancerName_ =
  GetLoadBalancerTlsCertificates'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer you associated with your SSL\/TLS
-- certificate.
getLoadBalancerTlsCertificates_loadBalancerName :: Lens.Lens' GetLoadBalancerTlsCertificates Prelude.Text
getLoadBalancerTlsCertificates_loadBalancerName = Lens.lens (\GetLoadBalancerTlsCertificates' {loadBalancerName} -> loadBalancerName) (\s@GetLoadBalancerTlsCertificates' {} a -> s {loadBalancerName = a} :: GetLoadBalancerTlsCertificates)

instance
  Core.AWSRequest
    GetLoadBalancerTlsCertificates
  where
  type
    AWSResponse GetLoadBalancerTlsCertificates =
      GetLoadBalancerTlsCertificatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancerTlsCertificatesResponse'
            Prelude.<$> ( x Core..?> "tlsCertificates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetLoadBalancerTlsCertificates
  where
  hashWithSalt
    _salt
    GetLoadBalancerTlsCertificates' {..} =
      _salt `Prelude.hashWithSalt` loadBalancerName

instance
  Prelude.NFData
    GetLoadBalancerTlsCertificates
  where
  rnf GetLoadBalancerTlsCertificates' {..} =
    Prelude.rnf loadBalancerName

instance
  Core.ToHeaders
    GetLoadBalancerTlsCertificates
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetLoadBalancerTlsCertificates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetLoadBalancerTlsCertificates where
  toJSON GetLoadBalancerTlsCertificates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("loadBalancerName" Core..= loadBalancerName)
          ]
      )

instance Core.ToPath GetLoadBalancerTlsCertificates where
  toPath = Prelude.const "/"

instance Core.ToQuery GetLoadBalancerTlsCertificates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLoadBalancerTlsCertificatesResponse' smart constructor.
data GetLoadBalancerTlsCertificatesResponse = GetLoadBalancerTlsCertificatesResponse'
  { -- | An array of LoadBalancerTlsCertificate objects describing your SSL\/TLS
    -- certificates.
    tlsCertificates :: Prelude.Maybe [LoadBalancerTlsCertificate],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLoadBalancerTlsCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tlsCertificates', 'getLoadBalancerTlsCertificatesResponse_tlsCertificates' - An array of LoadBalancerTlsCertificate objects describing your SSL\/TLS
-- certificates.
--
-- 'httpStatus', 'getLoadBalancerTlsCertificatesResponse_httpStatus' - The response's http status code.
newGetLoadBalancerTlsCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLoadBalancerTlsCertificatesResponse
newGetLoadBalancerTlsCertificatesResponse
  pHttpStatus_ =
    GetLoadBalancerTlsCertificatesResponse'
      { tlsCertificates =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of LoadBalancerTlsCertificate objects describing your SSL\/TLS
-- certificates.
getLoadBalancerTlsCertificatesResponse_tlsCertificates :: Lens.Lens' GetLoadBalancerTlsCertificatesResponse (Prelude.Maybe [LoadBalancerTlsCertificate])
getLoadBalancerTlsCertificatesResponse_tlsCertificates = Lens.lens (\GetLoadBalancerTlsCertificatesResponse' {tlsCertificates} -> tlsCertificates) (\s@GetLoadBalancerTlsCertificatesResponse' {} a -> s {tlsCertificates = a} :: GetLoadBalancerTlsCertificatesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getLoadBalancerTlsCertificatesResponse_httpStatus :: Lens.Lens' GetLoadBalancerTlsCertificatesResponse Prelude.Int
getLoadBalancerTlsCertificatesResponse_httpStatus = Lens.lens (\GetLoadBalancerTlsCertificatesResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancerTlsCertificatesResponse' {} a -> s {httpStatus = a} :: GetLoadBalancerTlsCertificatesResponse)

instance
  Prelude.NFData
    GetLoadBalancerTlsCertificatesResponse
  where
  rnf GetLoadBalancerTlsCertificatesResponse' {..} =
    Prelude.rnf tlsCertificates
      `Prelude.seq` Prelude.rnf httpStatus
