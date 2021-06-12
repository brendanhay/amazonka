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
-- Module      : Network.AWS.Lightsail.GetLoadBalancerTlsCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Lightsail.GetLoadBalancerTlsCertificates
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLoadBalancerTlsCertificates' smart constructor.
data GetLoadBalancerTlsCertificates = GetLoadBalancerTlsCertificates'
  { -- | The name of the load balancer you associated with your SSL\/TLS
    -- certificate.
    loadBalancerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetLoadBalancerTlsCertificates
newGetLoadBalancerTlsCertificates pLoadBalancerName_ =
  GetLoadBalancerTlsCertificates'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer you associated with your SSL\/TLS
-- certificate.
getLoadBalancerTlsCertificates_loadBalancerName :: Lens.Lens' GetLoadBalancerTlsCertificates Core.Text
getLoadBalancerTlsCertificates_loadBalancerName = Lens.lens (\GetLoadBalancerTlsCertificates' {loadBalancerName} -> loadBalancerName) (\s@GetLoadBalancerTlsCertificates' {} a -> s {loadBalancerName = a} :: GetLoadBalancerTlsCertificates)

instance
  Core.AWSRequest
    GetLoadBalancerTlsCertificates
  where
  type
    AWSResponse GetLoadBalancerTlsCertificates =
      GetLoadBalancerTlsCertificatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoadBalancerTlsCertificatesResponse'
            Core.<$> (x Core..?> "tlsCertificates" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetLoadBalancerTlsCertificates

instance Core.NFData GetLoadBalancerTlsCertificates

instance
  Core.ToHeaders
    GetLoadBalancerTlsCertificates
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetLoadBalancerTlsCertificates" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetLoadBalancerTlsCertificates where
  toJSON GetLoadBalancerTlsCertificates' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loadBalancerName" Core..= loadBalancerName)
          ]
      )

instance Core.ToPath GetLoadBalancerTlsCertificates where
  toPath = Core.const "/"

instance Core.ToQuery GetLoadBalancerTlsCertificates where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetLoadBalancerTlsCertificatesResponse' smart constructor.
data GetLoadBalancerTlsCertificatesResponse = GetLoadBalancerTlsCertificatesResponse'
  { -- | An array of LoadBalancerTlsCertificate objects describing your SSL\/TLS
    -- certificates.
    tlsCertificates :: Core.Maybe [LoadBalancerTlsCertificate],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetLoadBalancerTlsCertificatesResponse
newGetLoadBalancerTlsCertificatesResponse
  pHttpStatus_ =
    GetLoadBalancerTlsCertificatesResponse'
      { tlsCertificates =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of LoadBalancerTlsCertificate objects describing your SSL\/TLS
-- certificates.
getLoadBalancerTlsCertificatesResponse_tlsCertificates :: Lens.Lens' GetLoadBalancerTlsCertificatesResponse (Core.Maybe [LoadBalancerTlsCertificate])
getLoadBalancerTlsCertificatesResponse_tlsCertificates = Lens.lens (\GetLoadBalancerTlsCertificatesResponse' {tlsCertificates} -> tlsCertificates) (\s@GetLoadBalancerTlsCertificatesResponse' {} a -> s {tlsCertificates = a} :: GetLoadBalancerTlsCertificatesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getLoadBalancerTlsCertificatesResponse_httpStatus :: Lens.Lens' GetLoadBalancerTlsCertificatesResponse Core.Int
getLoadBalancerTlsCertificatesResponse_httpStatus = Lens.lens (\GetLoadBalancerTlsCertificatesResponse' {httpStatus} -> httpStatus) (\s@GetLoadBalancerTlsCertificatesResponse' {} a -> s {httpStatus = a} :: GetLoadBalancerTlsCertificatesResponse)

instance
  Core.NFData
    GetLoadBalancerTlsCertificatesResponse
