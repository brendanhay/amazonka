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
-- Module      : Amazonka.Lightsail.AttachLoadBalancerTlsCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a Transport Layer Security (TLS) certificate to your load
-- balancer. TLS is just an updated, more secure version of Secure Socket
-- Layer (SSL).
--
-- Once you create and validate your certificate, you can attach it to your
-- load balancer. You can also use this API to rotate the certificates on
-- your account. Use the @AttachLoadBalancerTlsCertificate@ action with the
-- non-attached certificate, and it will replace the existing one and
-- become the attached certificate.
--
-- The @AttachLoadBalancerTlsCertificate@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.AttachLoadBalancerTlsCertificate
  ( -- * Creating a Request
    AttachLoadBalancerTlsCertificate (..),
    newAttachLoadBalancerTlsCertificate,

    -- * Request Lenses
    attachLoadBalancerTlsCertificate_loadBalancerName,
    attachLoadBalancerTlsCertificate_certificateName,

    -- * Destructuring the Response
    AttachLoadBalancerTlsCertificateResponse (..),
    newAttachLoadBalancerTlsCertificateResponse,

    -- * Response Lenses
    attachLoadBalancerTlsCertificateResponse_operations,
    attachLoadBalancerTlsCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachLoadBalancerTlsCertificate' smart constructor.
data AttachLoadBalancerTlsCertificate = AttachLoadBalancerTlsCertificate'
  { -- | The name of the load balancer to which you want to associate the
    -- SSL\/TLS certificate.
    loadBalancerName :: Prelude.Text,
    -- | The name of your SSL\/TLS certificate.
    certificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancerTlsCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'attachLoadBalancerTlsCertificate_loadBalancerName' - The name of the load balancer to which you want to associate the
-- SSL\/TLS certificate.
--
-- 'certificateName', 'attachLoadBalancerTlsCertificate_certificateName' - The name of your SSL\/TLS certificate.
newAttachLoadBalancerTlsCertificate ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'certificateName'
  Prelude.Text ->
  AttachLoadBalancerTlsCertificate
newAttachLoadBalancerTlsCertificate
  pLoadBalancerName_
  pCertificateName_ =
    AttachLoadBalancerTlsCertificate'
      { loadBalancerName =
          pLoadBalancerName_,
        certificateName = pCertificateName_
      }

-- | The name of the load balancer to which you want to associate the
-- SSL\/TLS certificate.
attachLoadBalancerTlsCertificate_loadBalancerName :: Lens.Lens' AttachLoadBalancerTlsCertificate Prelude.Text
attachLoadBalancerTlsCertificate_loadBalancerName = Lens.lens (\AttachLoadBalancerTlsCertificate' {loadBalancerName} -> loadBalancerName) (\s@AttachLoadBalancerTlsCertificate' {} a -> s {loadBalancerName = a} :: AttachLoadBalancerTlsCertificate)

-- | The name of your SSL\/TLS certificate.
attachLoadBalancerTlsCertificate_certificateName :: Lens.Lens' AttachLoadBalancerTlsCertificate Prelude.Text
attachLoadBalancerTlsCertificate_certificateName = Lens.lens (\AttachLoadBalancerTlsCertificate' {certificateName} -> certificateName) (\s@AttachLoadBalancerTlsCertificate' {} a -> s {certificateName = a} :: AttachLoadBalancerTlsCertificate)

instance
  Core.AWSRequest
    AttachLoadBalancerTlsCertificate
  where
  type
    AWSResponse AttachLoadBalancerTlsCertificate =
      AttachLoadBalancerTlsCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachLoadBalancerTlsCertificateResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AttachLoadBalancerTlsCertificate
  where
  hashWithSalt
    _salt
    AttachLoadBalancerTlsCertificate' {..} =
      _salt
        `Prelude.hashWithSalt` loadBalancerName
        `Prelude.hashWithSalt` certificateName

instance
  Prelude.NFData
    AttachLoadBalancerTlsCertificate
  where
  rnf AttachLoadBalancerTlsCertificate' {..} =
    Prelude.rnf loadBalancerName `Prelude.seq`
      Prelude.rnf certificateName

instance
  Data.ToHeaders
    AttachLoadBalancerTlsCertificate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.AttachLoadBalancerTlsCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AttachLoadBalancerTlsCertificate where
  toJSON AttachLoadBalancerTlsCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("loadBalancerName" Data..= loadBalancerName),
            Prelude.Just
              ("certificateName" Data..= certificateName)
          ]
      )

instance Data.ToPath AttachLoadBalancerTlsCertificate where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AttachLoadBalancerTlsCertificate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachLoadBalancerTlsCertificateResponse' smart constructor.
data AttachLoadBalancerTlsCertificateResponse = AttachLoadBalancerTlsCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    --
    -- These SSL\/TLS certificates are only usable by Lightsail load balancers.
    -- You can\'t get the certificate and use it for another purpose.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachLoadBalancerTlsCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'attachLoadBalancerTlsCertificateResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- These SSL\/TLS certificates are only usable by Lightsail load balancers.
-- You can\'t get the certificate and use it for another purpose.
--
-- 'httpStatus', 'attachLoadBalancerTlsCertificateResponse_httpStatus' - The response's http status code.
newAttachLoadBalancerTlsCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachLoadBalancerTlsCertificateResponse
newAttachLoadBalancerTlsCertificateResponse
  pHttpStatus_ =
    AttachLoadBalancerTlsCertificateResponse'
      { operations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- These SSL\/TLS certificates are only usable by Lightsail load balancers.
-- You can\'t get the certificate and use it for another purpose.
attachLoadBalancerTlsCertificateResponse_operations :: Lens.Lens' AttachLoadBalancerTlsCertificateResponse (Prelude.Maybe [Operation])
attachLoadBalancerTlsCertificateResponse_operations = Lens.lens (\AttachLoadBalancerTlsCertificateResponse' {operations} -> operations) (\s@AttachLoadBalancerTlsCertificateResponse' {} a -> s {operations = a} :: AttachLoadBalancerTlsCertificateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
attachLoadBalancerTlsCertificateResponse_httpStatus :: Lens.Lens' AttachLoadBalancerTlsCertificateResponse Prelude.Int
attachLoadBalancerTlsCertificateResponse_httpStatus = Lens.lens (\AttachLoadBalancerTlsCertificateResponse' {httpStatus} -> httpStatus) (\s@AttachLoadBalancerTlsCertificateResponse' {} a -> s {httpStatus = a} :: AttachLoadBalancerTlsCertificateResponse)

instance
  Prelude.NFData
    AttachLoadBalancerTlsCertificateResponse
  where
  rnf AttachLoadBalancerTlsCertificateResponse' {..} =
    Prelude.rnf operations `Prelude.seq`
      Prelude.rnf httpStatus
