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
-- Module      : Amazonka.ELB.SetLoadBalancerListenerSSLCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ELB.SetLoadBalancerListenerSSLCertificate
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'newSetLoadBalancerListenerSSLCertificate' smart constructor.
data SetLoadBalancerListenerSSLCertificate = SetLoadBalancerListenerSSLCertificate'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | The port that uses the specified SSL certificate.
    loadBalancerPort :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the SSL certificate.
    sSLCertificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'loadBalancerPort'
  Prelude.Int ->
  -- | 'sSLCertificateId'
  Prelude.Text ->
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
setLoadBalancerListenerSSLCertificate_loadBalancerName :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Prelude.Text
setLoadBalancerListenerSSLCertificate_loadBalancerName = Lens.lens (\SetLoadBalancerListenerSSLCertificate' {loadBalancerName} -> loadBalancerName) (\s@SetLoadBalancerListenerSSLCertificate' {} a -> s {loadBalancerName = a} :: SetLoadBalancerListenerSSLCertificate)

-- | The port that uses the specified SSL certificate.
setLoadBalancerListenerSSLCertificate_loadBalancerPort :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Prelude.Int
setLoadBalancerListenerSSLCertificate_loadBalancerPort = Lens.lens (\SetLoadBalancerListenerSSLCertificate' {loadBalancerPort} -> loadBalancerPort) (\s@SetLoadBalancerListenerSSLCertificate' {} a -> s {loadBalancerPort = a} :: SetLoadBalancerListenerSSLCertificate)

-- | The Amazon Resource Name (ARN) of the SSL certificate.
setLoadBalancerListenerSSLCertificate_sSLCertificateId :: Lens.Lens' SetLoadBalancerListenerSSLCertificate Prelude.Text
setLoadBalancerListenerSSLCertificate_sSLCertificateId = Lens.lens (\SetLoadBalancerListenerSSLCertificate' {sSLCertificateId} -> sSLCertificateId) (\s@SetLoadBalancerListenerSSLCertificate' {} a -> s {sSLCertificateId = a} :: SetLoadBalancerListenerSSLCertificate)

instance
  Core.AWSRequest
    SetLoadBalancerListenerSSLCertificate
  where
  type
    AWSResponse
      SetLoadBalancerListenerSSLCertificate =
      SetLoadBalancerListenerSSLCertificateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetLoadBalancerListenerSSLCertificateResult"
      ( \s h x ->
          SetLoadBalancerListenerSSLCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetLoadBalancerListenerSSLCertificate
  where
  hashWithSalt
    _salt
    SetLoadBalancerListenerSSLCertificate' {..} =
      _salt
        `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` loadBalancerPort
        `Prelude.hashWithSalt` sSLCertificateId

instance
  Prelude.NFData
    SetLoadBalancerListenerSSLCertificate
  where
  rnf SetLoadBalancerListenerSSLCertificate' {..} =
    Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf loadBalancerPort
      `Prelude.seq` Prelude.rnf sSLCertificateId

instance
  Data.ToHeaders
    SetLoadBalancerListenerSSLCertificate
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    SetLoadBalancerListenerSSLCertificate
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    SetLoadBalancerListenerSSLCertificate
  where
  toQuery SetLoadBalancerListenerSSLCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "SetLoadBalancerListenerSSLCertificate" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Data.=: loadBalancerName,
        "LoadBalancerPort" Data.=: loadBalancerPort,
        "SSLCertificateId" Data.=: sSLCertificateId
      ]

-- | Contains the output of SetLoadBalancerListenerSSLCertificate.
--
-- /See:/ 'newSetLoadBalancerListenerSSLCertificateResponse' smart constructor.
data SetLoadBalancerListenerSSLCertificateResponse = SetLoadBalancerListenerSSLCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  SetLoadBalancerListenerSSLCertificateResponse
newSetLoadBalancerListenerSSLCertificateResponse
  pHttpStatus_ =
    SetLoadBalancerListenerSSLCertificateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
setLoadBalancerListenerSSLCertificateResponse_httpStatus :: Lens.Lens' SetLoadBalancerListenerSSLCertificateResponse Prelude.Int
setLoadBalancerListenerSSLCertificateResponse_httpStatus = Lens.lens (\SetLoadBalancerListenerSSLCertificateResponse' {httpStatus} -> httpStatus) (\s@SetLoadBalancerListenerSSLCertificateResponse' {} a -> s {httpStatus = a} :: SetLoadBalancerListenerSSLCertificateResponse)

instance
  Prelude.NFData
    SetLoadBalancerListenerSSLCertificateResponse
  where
  rnf
    SetLoadBalancerListenerSSLCertificateResponse' {..} =
      Prelude.rnf httpStatus
