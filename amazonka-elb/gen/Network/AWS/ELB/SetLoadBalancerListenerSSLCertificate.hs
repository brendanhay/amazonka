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
-- Module      : Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the certificate that terminates the specified listener\'s SSL
-- connections. The specified certificate replaces any prior certificate
-- that was used on the same load balancer and port.
--
-- For more information about updating your SSL certificate, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/elb-update-ssl-cert.html Replace the SSL Certificate for Your Load Balancer>
-- in the /Classic Load Balancers Guide/.
module Network.AWS.ELB.SetLoadBalancerListenerSSLCertificate
  ( -- * Creating a Request
    SetLoadBalancerListenerSSLCertificate (..),
    newSetLoadBalancerListenerSSLCertificate,

    -- * Request Lenses
    setLoadBalancerListenerSSLCertificate_loadBalancerName,
    setLoadBalancerListenerSSLCertificate_loadBalancerPort,
    setLoadBalancerListenerSSLCertificate_sSLCertificateId,

    -- * Destructuring the Response
    SetLoadBalancerListenerSSLCertificateResponse (..),
    newSetLoadBalancerListenerSSLCertificateResponse,

    -- * Response Lenses
    setLoadBalancerListenerSSLCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'newSetLoadBalancerListenerSSLCertificate' smart constructor.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text,
    -- | The port that uses the specified SSL certificate.
    loadBalancerPort :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the SSL certificate.
    sSLCertificateId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetLoadBalancerListenerSSLCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'setLoadBalancerListenerSSLCertificate_loadBalancerName' - The name of the load balancer.
--
-- 'loadBalancerPort', 'setLoadBalancerListenerSSLCertificate_loadBalancerPort' - The port that uses the specified SSL certificate.
--
-- 'sSLCertificateId', 'setLoadBalancerListenerSSLCertificate_sSLCertificateId' - The Amazon Resource Name (ARN) of the SSL certificate.
newSetLoadBalancerListenerSSLCertificate ::
  -- | 'loadBalancerName'
  Core.Text ->
  -- | 'loadBalancerPort'
  Core.Int ->
  -- | 'sSLCertificateId'
  Core.Text ->
  SetLoadBalancerListenerSSLCertificate
newSetLoadBalancerListenerSSLCertificate
  pLoadBalancerName_
  pLoadBalancerPort_
  pSSLCertificateId_ =
    SetLoadBalancerListenerSSLCertificate'
      { loadBalancerName =
          pLoadBalancerName_,
        loadBalancerPort =
          pLoadBalancerPort_,
        sSLCertificateId =
          pSSLCertificateId_
      }

-- | The name of the load balancer.
setLoadBalancerListenerSSLCertificate_loadBalancerName :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Core.Text
setLoadBalancerListenerSSLCertificate_loadBalancerName = Lens.lens (\SetLoadBalancerListenerSSLCertificate' {loadBalancerName} -> loadBalancerName) (\s@SetLoadBalancerListenerSSLCertificate' {} a -> s {loadBalancerName = a} :: SetLoadBalancerListenerSSLCertificate)

-- | The port that uses the specified SSL certificate.
setLoadBalancerListenerSSLCertificate_loadBalancerPort :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Core.Int
setLoadBalancerListenerSSLCertificate_loadBalancerPort = Lens.lens (\SetLoadBalancerListenerSSLCertificate' {loadBalancerPort} -> loadBalancerPort) (\s@SetLoadBalancerListenerSSLCertificate' {} a -> s {loadBalancerPort = a} :: SetLoadBalancerListenerSSLCertificate)

-- | The Amazon Resource Name (ARN) of the SSL certificate.
setLoadBalancerListenerSSLCertificate_sSLCertificateId :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Core.Text
setLoadBalancerListenerSSLCertificate_sSLCertificateId = Lens.lens (\SetLoadBalancerListenerSSLCertificate' {sSLCertificateId} -> sSLCertificateId) (\s@SetLoadBalancerListenerSSLCertificate' {} a -> s {sSLCertificateId = a} :: SetLoadBalancerListenerSSLCertificate)

instance
  Core.AWSRequest
    SetLoadBalancerListenerSSLCertificate
  where
  type
    AWSResponse
      SetLoadBalancerListenerSSLCertificate =
      SetLoadBalancerListenerSSLCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetLoadBalancerListenerSSLCertificateResult"
      ( \s h x ->
          SetLoadBalancerListenerSSLCertificateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    SetLoadBalancerListenerSSLCertificate

instance
  Core.NFData
    SetLoadBalancerListenerSSLCertificate

instance
  Core.ToHeaders
    SetLoadBalancerListenerSSLCertificate
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    SetLoadBalancerListenerSSLCertificate
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    SetLoadBalancerListenerSSLCertificate
  where
  toQuery SetLoadBalancerListenerSSLCertificate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "SetLoadBalancerListenerSSLCertificate" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName,
        "LoadBalancerPort" Core.=: loadBalancerPort,
        "SSLCertificateId" Core.=: sSLCertificateId
      ]

-- | Contains the output of SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'newSetLoadBalancerListenerSSLCertificateResponse' smart constructor.
data SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetLoadBalancerListenerSSLCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setLoadBalancerListenerSSLCertificateResponse_httpStatus' - The response's http status code.
newSetLoadBalancerListenerSSLCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetLoadBalancerListenerSSLCertificateResponse
newSetLoadBalancerListenerSSLCertificateResponse
  pHttpStatus_ =
    SetLoadBalancerListenerSSLCertificateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
setLoadBalancerListenerSSLCertificateResponse_httpStatus :: Lens.Lens' SetLoadBalancerListenerSSLCertificateResponse Core.Int
setLoadBalancerListenerSSLCertificateResponse_httpStatus = Lens.lens (\SetLoadBalancerListenerSSLCertificateResponse' {httpStatus} -> httpStatus) (\s@SetLoadBalancerListenerSSLCertificateResponse' {} a -> s {httpStatus = a} :: SetLoadBalancerListenerSSLCertificateResponse)

instance
  Core.NFData
    SetLoadBalancerListenerSSLCertificateResponse
