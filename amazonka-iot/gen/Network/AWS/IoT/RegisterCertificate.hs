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
-- Module      : Network.AWS.IoT.RegisterCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device certificate with AWS IoT. If you have more than one
-- CA certificate that has the same subject field, you must specify the CA
-- certificate that was used to sign the device certificate being
-- registered.
module Network.AWS.IoT.RegisterCertificate
  ( -- * Creating a Request
    RegisterCertificate (..),
    newRegisterCertificate,

    -- * Request Lenses
    registerCertificate_caCertificatePem,
    registerCertificate_setAsActive,
    registerCertificate_status,
    registerCertificate_certificatePem,

    -- * Destructuring the Response
    RegisterCertificateResponse (..),
    newRegisterCertificateResponse,

    -- * Response Lenses
    registerCertificateResponse_certificateArn,
    registerCertificateResponse_certificateId,
    registerCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the RegisterCertificate operation.
--
-- /See:/ 'newRegisterCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { -- | The CA certificate used to sign the device certificate being registered.
    caCertificatePem :: Core.Maybe Core.Text,
    -- | A boolean value that specifies if the certificate is set to active.
    setAsActive :: Core.Maybe Core.Bool,
    -- | The status of the register certificate request.
    status :: Core.Maybe CertificateStatus,
    -- | The certificate data, in PEM format.
    certificatePem :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caCertificatePem', 'registerCertificate_caCertificatePem' - The CA certificate used to sign the device certificate being registered.
--
-- 'setAsActive', 'registerCertificate_setAsActive' - A boolean value that specifies if the certificate is set to active.
--
-- 'status', 'registerCertificate_status' - The status of the register certificate request.
--
-- 'certificatePem', 'registerCertificate_certificatePem' - The certificate data, in PEM format.
newRegisterCertificate ::
  -- | 'certificatePem'
  Core.Text ->
  RegisterCertificate
newRegisterCertificate pCertificatePem_ =
  RegisterCertificate'
    { caCertificatePem =
        Core.Nothing,
      setAsActive = Core.Nothing,
      status = Core.Nothing,
      certificatePem = pCertificatePem_
    }

-- | The CA certificate used to sign the device certificate being registered.
registerCertificate_caCertificatePem :: Lens.Lens' RegisterCertificate (Core.Maybe Core.Text)
registerCertificate_caCertificatePem = Lens.lens (\RegisterCertificate' {caCertificatePem} -> caCertificatePem) (\s@RegisterCertificate' {} a -> s {caCertificatePem = a} :: RegisterCertificate)

-- | A boolean value that specifies if the certificate is set to active.
registerCertificate_setAsActive :: Lens.Lens' RegisterCertificate (Core.Maybe Core.Bool)
registerCertificate_setAsActive = Lens.lens (\RegisterCertificate' {setAsActive} -> setAsActive) (\s@RegisterCertificate' {} a -> s {setAsActive = a} :: RegisterCertificate)

-- | The status of the register certificate request.
registerCertificate_status :: Lens.Lens' RegisterCertificate (Core.Maybe CertificateStatus)
registerCertificate_status = Lens.lens (\RegisterCertificate' {status} -> status) (\s@RegisterCertificate' {} a -> s {status = a} :: RegisterCertificate)

-- | The certificate data, in PEM format.
registerCertificate_certificatePem :: Lens.Lens' RegisterCertificate Core.Text
registerCertificate_certificatePem = Lens.lens (\RegisterCertificate' {certificatePem} -> certificatePem) (\s@RegisterCertificate' {} a -> s {certificatePem = a} :: RegisterCertificate)

instance Core.AWSRequest RegisterCertificate where
  type
    AWSResponse RegisterCertificate =
      RegisterCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCertificateResponse'
            Core.<$> (x Core..?> "certificateArn")
            Core.<*> (x Core..?> "certificateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterCertificate

instance Core.NFData RegisterCertificate

instance Core.ToHeaders RegisterCertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON RegisterCertificate where
  toJSON RegisterCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("caCertificatePem" Core..=)
              Core.<$> caCertificatePem,
            ("status" Core..=) Core.<$> status,
            Core.Just ("certificatePem" Core..= certificatePem)
          ]
      )

instance Core.ToPath RegisterCertificate where
  toPath = Core.const "/certificate/register"

instance Core.ToQuery RegisterCertificate where
  toQuery RegisterCertificate' {..} =
    Core.mconcat ["setAsActive" Core.=: setAsActive]

-- | The output from the RegisterCertificate operation.
--
-- /See:/ 'newRegisterCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { -- | The certificate ARN.
    certificateArn :: Core.Maybe Core.Text,
    -- | The certificate identifier.
    certificateId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'registerCertificateResponse_certificateArn' - The certificate ARN.
--
-- 'certificateId', 'registerCertificateResponse_certificateId' - The certificate identifier.
--
-- 'httpStatus', 'registerCertificateResponse_httpStatus' - The response's http status code.
newRegisterCertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterCertificateResponse
newRegisterCertificateResponse pHttpStatus_ =
  RegisterCertificateResponse'
    { certificateArn =
        Core.Nothing,
      certificateId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The certificate ARN.
registerCertificateResponse_certificateArn :: Lens.Lens' RegisterCertificateResponse (Core.Maybe Core.Text)
registerCertificateResponse_certificateArn = Lens.lens (\RegisterCertificateResponse' {certificateArn} -> certificateArn) (\s@RegisterCertificateResponse' {} a -> s {certificateArn = a} :: RegisterCertificateResponse)

-- | The certificate identifier.
registerCertificateResponse_certificateId :: Lens.Lens' RegisterCertificateResponse (Core.Maybe Core.Text)
registerCertificateResponse_certificateId = Lens.lens (\RegisterCertificateResponse' {certificateId} -> certificateId) (\s@RegisterCertificateResponse' {} a -> s {certificateId = a} :: RegisterCertificateResponse)

-- | The response's http status code.
registerCertificateResponse_httpStatus :: Lens.Lens' RegisterCertificateResponse Core.Int
registerCertificateResponse_httpStatus = Lens.lens (\RegisterCertificateResponse' {httpStatus} -> httpStatus) (\s@RegisterCertificateResponse' {} a -> s {httpStatus = a} :: RegisterCertificateResponse)

instance Core.NFData RegisterCertificateResponse
