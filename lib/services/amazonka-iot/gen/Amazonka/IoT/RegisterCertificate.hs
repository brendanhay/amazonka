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
-- Module      : Amazonka.IoT.RegisterCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a device certificate with IoT in the same
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CertificateDescription.html#iot-Type-CertificateDescription-certificateMode certificate mode>
-- as the signing CA. If you have more than one CA certificate that has the
-- same subject field, you must specify the CA certificate that was used to
-- sign the device certificate being registered.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RegisterCertificate>
-- action.
module Amazonka.IoT.RegisterCertificate
  ( -- * Creating a Request
    RegisterCertificate (..),
    newRegisterCertificate,

    -- * Request Lenses
    registerCertificate_caCertificatePem,
    registerCertificate_status,
    registerCertificate_setAsActive,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input to the RegisterCertificate operation.
--
-- /See:/ 'newRegisterCertificate' smart constructor.
data RegisterCertificate = RegisterCertificate'
  { -- | The CA certificate used to sign the device certificate being registered.
    caCertificatePem :: Prelude.Maybe Prelude.Text,
    -- | The status of the register certificate request. Valid values that you
    -- can use include @ACTIVE@, @INACTIVE@, and @REVOKED@.
    status :: Prelude.Maybe CertificateStatus,
    -- | A boolean value that specifies if the certificate is set to active.
    --
    -- Valid values: @ACTIVE | INACTIVE@
    setAsActive :: Prelude.Maybe Prelude.Bool,
    -- | The certificate data, in PEM format.
    certificatePem :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'status', 'registerCertificate_status' - The status of the register certificate request. Valid values that you
-- can use include @ACTIVE@, @INACTIVE@, and @REVOKED@.
--
-- 'setAsActive', 'registerCertificate_setAsActive' - A boolean value that specifies if the certificate is set to active.
--
-- Valid values: @ACTIVE | INACTIVE@
--
-- 'certificatePem', 'registerCertificate_certificatePem' - The certificate data, in PEM format.
newRegisterCertificate ::
  -- | 'certificatePem'
  Prelude.Text ->
  RegisterCertificate
newRegisterCertificate pCertificatePem_ =
  RegisterCertificate'
    { caCertificatePem =
        Prelude.Nothing,
      status = Prelude.Nothing,
      setAsActive = Prelude.Nothing,
      certificatePem = pCertificatePem_
    }

-- | The CA certificate used to sign the device certificate being registered.
registerCertificate_caCertificatePem :: Lens.Lens' RegisterCertificate (Prelude.Maybe Prelude.Text)
registerCertificate_caCertificatePem = Lens.lens (\RegisterCertificate' {caCertificatePem} -> caCertificatePem) (\s@RegisterCertificate' {} a -> s {caCertificatePem = a} :: RegisterCertificate)

-- | The status of the register certificate request. Valid values that you
-- can use include @ACTIVE@, @INACTIVE@, and @REVOKED@.
registerCertificate_status :: Lens.Lens' RegisterCertificate (Prelude.Maybe CertificateStatus)
registerCertificate_status = Lens.lens (\RegisterCertificate' {status} -> status) (\s@RegisterCertificate' {} a -> s {status = a} :: RegisterCertificate)

-- | A boolean value that specifies if the certificate is set to active.
--
-- Valid values: @ACTIVE | INACTIVE@
registerCertificate_setAsActive :: Lens.Lens' RegisterCertificate (Prelude.Maybe Prelude.Bool)
registerCertificate_setAsActive = Lens.lens (\RegisterCertificate' {setAsActive} -> setAsActive) (\s@RegisterCertificate' {} a -> s {setAsActive = a} :: RegisterCertificate)

-- | The certificate data, in PEM format.
registerCertificate_certificatePem :: Lens.Lens' RegisterCertificate Prelude.Text
registerCertificate_certificatePem = Lens.lens (\RegisterCertificate' {certificatePem} -> certificatePem) (\s@RegisterCertificate' {} a -> s {certificatePem = a} :: RegisterCertificate)

instance Core.AWSRequest RegisterCertificate where
  type
    AWSResponse RegisterCertificate =
      RegisterCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCertificateResponse'
            Prelude.<$> (x Core..?> "certificateArn")
            Prelude.<*> (x Core..?> "certificateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterCertificate where
  hashWithSalt _salt RegisterCertificate' {..} =
    _salt `Prelude.hashWithSalt` caCertificatePem
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` setAsActive
      `Prelude.hashWithSalt` certificatePem

instance Prelude.NFData RegisterCertificate where
  rnf RegisterCertificate' {..} =
    Prelude.rnf caCertificatePem
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf setAsActive
      `Prelude.seq` Prelude.rnf certificatePem

instance Core.ToHeaders RegisterCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON RegisterCertificate where
  toJSON RegisterCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("caCertificatePem" Core..=)
              Prelude.<$> caCertificatePem,
            ("status" Core..=) Prelude.<$> status,
            Prelude.Just
              ("certificatePem" Core..= certificatePem)
          ]
      )

instance Core.ToPath RegisterCertificate where
  toPath = Prelude.const "/certificate/register"

instance Core.ToQuery RegisterCertificate where
  toQuery RegisterCertificate' {..} =
    Prelude.mconcat ["setAsActive" Core.=: setAsActive]

-- | The output from the RegisterCertificate operation.
--
-- /See:/ 'newRegisterCertificateResponse' smart constructor.
data RegisterCertificateResponse = RegisterCertificateResponse'
  { -- | The certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The certificate identifier.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RegisterCertificateResponse
newRegisterCertificateResponse pHttpStatus_ =
  RegisterCertificateResponse'
    { certificateArn =
        Prelude.Nothing,
      certificateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The certificate ARN.
registerCertificateResponse_certificateArn :: Lens.Lens' RegisterCertificateResponse (Prelude.Maybe Prelude.Text)
registerCertificateResponse_certificateArn = Lens.lens (\RegisterCertificateResponse' {certificateArn} -> certificateArn) (\s@RegisterCertificateResponse' {} a -> s {certificateArn = a} :: RegisterCertificateResponse)

-- | The certificate identifier.
registerCertificateResponse_certificateId :: Lens.Lens' RegisterCertificateResponse (Prelude.Maybe Prelude.Text)
registerCertificateResponse_certificateId = Lens.lens (\RegisterCertificateResponse' {certificateId} -> certificateId) (\s@RegisterCertificateResponse' {} a -> s {certificateId = a} :: RegisterCertificateResponse)

-- | The response's http status code.
registerCertificateResponse_httpStatus :: Lens.Lens' RegisterCertificateResponse Prelude.Int
registerCertificateResponse_httpStatus = Lens.lens (\RegisterCertificateResponse' {httpStatus} -> httpStatus) (\s@RegisterCertificateResponse' {} a -> s {httpStatus = a} :: RegisterCertificateResponse)

instance Prelude.NFData RegisterCertificateResponse where
  rnf RegisterCertificateResponse' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf httpStatus
