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
-- Module      : Amazonka.IoT.RegisterCertificateWithoutCA
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Register a certificate that does not have a certificate authority (CA).
-- For supported certificates, consult
-- <https://docs.aws.amazon.com/iot/latest/developerguide/x509-client-certs.html#x509-cert-algorithms Certificate signing algorithms supported by IoT>.
module Amazonka.IoT.RegisterCertificateWithoutCA
  ( -- * Creating a Request
    RegisterCertificateWithoutCA (..),
    newRegisterCertificateWithoutCA,

    -- * Request Lenses
    registerCertificateWithoutCA_status,
    registerCertificateWithoutCA_certificatePem,

    -- * Destructuring the Response
    RegisterCertificateWithoutCAResponse (..),
    newRegisterCertificateWithoutCAResponse,

    -- * Response Lenses
    registerCertificateWithoutCAResponse_certificateArn,
    registerCertificateWithoutCAResponse_certificateId,
    registerCertificateWithoutCAResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegisterCertificateWithoutCA' smart constructor.
data RegisterCertificateWithoutCA = RegisterCertificateWithoutCA'
  { -- | The status of the register certificate request.
    status :: Prelude.Maybe CertificateStatus,
    -- | The certificate data, in PEM format.
    certificatePem :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCertificateWithoutCA' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'registerCertificateWithoutCA_status' - The status of the register certificate request.
--
-- 'certificatePem', 'registerCertificateWithoutCA_certificatePem' - The certificate data, in PEM format.
newRegisterCertificateWithoutCA ::
  -- | 'certificatePem'
  Prelude.Text ->
  RegisterCertificateWithoutCA
newRegisterCertificateWithoutCA pCertificatePem_ =
  RegisterCertificateWithoutCA'
    { status =
        Prelude.Nothing,
      certificatePem = pCertificatePem_
    }

-- | The status of the register certificate request.
registerCertificateWithoutCA_status :: Lens.Lens' RegisterCertificateWithoutCA (Prelude.Maybe CertificateStatus)
registerCertificateWithoutCA_status = Lens.lens (\RegisterCertificateWithoutCA' {status} -> status) (\s@RegisterCertificateWithoutCA' {} a -> s {status = a} :: RegisterCertificateWithoutCA)

-- | The certificate data, in PEM format.
registerCertificateWithoutCA_certificatePem :: Lens.Lens' RegisterCertificateWithoutCA Prelude.Text
registerCertificateWithoutCA_certificatePem = Lens.lens (\RegisterCertificateWithoutCA' {certificatePem} -> certificatePem) (\s@RegisterCertificateWithoutCA' {} a -> s {certificatePem = a} :: RegisterCertificateWithoutCA)

instance Core.AWSRequest RegisterCertificateWithoutCA where
  type
    AWSResponse RegisterCertificateWithoutCA =
      RegisterCertificateWithoutCAResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCertificateWithoutCAResponse'
            Prelude.<$> (x Core..?> "certificateArn")
            Prelude.<*> (x Core..?> "certificateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RegisterCertificateWithoutCA
  where
  hashWithSalt _salt RegisterCertificateWithoutCA' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` certificatePem

instance Prelude.NFData RegisterCertificateWithoutCA where
  rnf RegisterCertificateWithoutCA' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf certificatePem

instance Core.ToHeaders RegisterCertificateWithoutCA where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON RegisterCertificateWithoutCA where
  toJSON RegisterCertificateWithoutCA' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            Prelude.Just
              ("certificatePem" Core..= certificatePem)
          ]
      )

instance Core.ToPath RegisterCertificateWithoutCA where
  toPath = Prelude.const "/certificate/register-no-ca"

instance Core.ToQuery RegisterCertificateWithoutCA where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRegisterCertificateWithoutCAResponse' smart constructor.
data RegisterCertificateWithoutCAResponse = RegisterCertificateWithoutCAResponse'
  { -- | The Amazon Resource Name (ARN) of the registered certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the registered certificate. (The last part of the certificate
    -- ARN contains the certificate ID.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCertificateWithoutCAResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'registerCertificateWithoutCAResponse_certificateArn' - The Amazon Resource Name (ARN) of the registered certificate.
--
-- 'certificateId', 'registerCertificateWithoutCAResponse_certificateId' - The ID of the registered certificate. (The last part of the certificate
-- ARN contains the certificate ID.
--
-- 'httpStatus', 'registerCertificateWithoutCAResponse_httpStatus' - The response's http status code.
newRegisterCertificateWithoutCAResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterCertificateWithoutCAResponse
newRegisterCertificateWithoutCAResponse pHttpStatus_ =
  RegisterCertificateWithoutCAResponse'
    { certificateArn =
        Prelude.Nothing,
      certificateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the registered certificate.
registerCertificateWithoutCAResponse_certificateArn :: Lens.Lens' RegisterCertificateWithoutCAResponse (Prelude.Maybe Prelude.Text)
registerCertificateWithoutCAResponse_certificateArn = Lens.lens (\RegisterCertificateWithoutCAResponse' {certificateArn} -> certificateArn) (\s@RegisterCertificateWithoutCAResponse' {} a -> s {certificateArn = a} :: RegisterCertificateWithoutCAResponse)

-- | The ID of the registered certificate. (The last part of the certificate
-- ARN contains the certificate ID.
registerCertificateWithoutCAResponse_certificateId :: Lens.Lens' RegisterCertificateWithoutCAResponse (Prelude.Maybe Prelude.Text)
registerCertificateWithoutCAResponse_certificateId = Lens.lens (\RegisterCertificateWithoutCAResponse' {certificateId} -> certificateId) (\s@RegisterCertificateWithoutCAResponse' {} a -> s {certificateId = a} :: RegisterCertificateWithoutCAResponse)

-- | The response's http status code.
registerCertificateWithoutCAResponse_httpStatus :: Lens.Lens' RegisterCertificateWithoutCAResponse Prelude.Int
registerCertificateWithoutCAResponse_httpStatus = Lens.lens (\RegisterCertificateWithoutCAResponse' {httpStatus} -> httpStatus) (\s@RegisterCertificateWithoutCAResponse' {} a -> s {httpStatus = a} :: RegisterCertificateWithoutCAResponse)

instance
  Prelude.NFData
    RegisterCertificateWithoutCAResponse
  where
  rnf RegisterCertificateWithoutCAResponse' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf httpStatus
