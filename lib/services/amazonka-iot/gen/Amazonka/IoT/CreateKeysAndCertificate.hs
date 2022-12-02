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
-- Module      : Amazonka.IoT.CreateKeysAndCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a 2048-bit RSA key pair and issues an X.509 certificate using
-- the issued public key. You can also call @CreateKeysAndCertificate@ over
-- MQTT from a device, for more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-wo-cert.html#provision-mqtt-api Provisioning MQTT API>.
--
-- __Note__ This is the only time IoT issues the private key for this
-- certificate, so it is important to keep it in a secure location.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateKeysAndCertificate>
-- action.
module Amazonka.IoT.CreateKeysAndCertificate
  ( -- * Creating a Request
    CreateKeysAndCertificate (..),
    newCreateKeysAndCertificate,

    -- * Request Lenses
    createKeysAndCertificate_setAsActive,

    -- * Destructuring the Response
    CreateKeysAndCertificateResponse (..),
    newCreateKeysAndCertificateResponse,

    -- * Response Lenses
    createKeysAndCertificateResponse_keyPair,
    createKeysAndCertificateResponse_certificateArn,
    createKeysAndCertificateResponse_certificateId,
    createKeysAndCertificateResponse_certificatePem,
    createKeysAndCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the CreateKeysAndCertificate operation.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateKeysAndCertificateRequest>
-- action.
--
-- /See:/ 'newCreateKeysAndCertificate' smart constructor.
data CreateKeysAndCertificate = CreateKeysAndCertificate'
  { -- | Specifies whether the certificate is active.
    setAsActive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeysAndCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setAsActive', 'createKeysAndCertificate_setAsActive' - Specifies whether the certificate is active.
newCreateKeysAndCertificate ::
  CreateKeysAndCertificate
newCreateKeysAndCertificate =
  CreateKeysAndCertificate'
    { setAsActive =
        Prelude.Nothing
    }

-- | Specifies whether the certificate is active.
createKeysAndCertificate_setAsActive :: Lens.Lens' CreateKeysAndCertificate (Prelude.Maybe Prelude.Bool)
createKeysAndCertificate_setAsActive = Lens.lens (\CreateKeysAndCertificate' {setAsActive} -> setAsActive) (\s@CreateKeysAndCertificate' {} a -> s {setAsActive = a} :: CreateKeysAndCertificate)

instance Core.AWSRequest CreateKeysAndCertificate where
  type
    AWSResponse CreateKeysAndCertificate =
      CreateKeysAndCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeysAndCertificateResponse'
            Prelude.<$> (x Data..?> "keyPair")
            Prelude.<*> (x Data..?> "certificateArn")
            Prelude.<*> (x Data..?> "certificateId")
            Prelude.<*> (x Data..?> "certificatePem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKeysAndCertificate where
  hashWithSalt _salt CreateKeysAndCertificate' {..} =
    _salt `Prelude.hashWithSalt` setAsActive

instance Prelude.NFData CreateKeysAndCertificate where
  rnf CreateKeysAndCertificate' {..} =
    Prelude.rnf setAsActive

instance Data.ToHeaders CreateKeysAndCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateKeysAndCertificate where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CreateKeysAndCertificate where
  toPath = Prelude.const "/keys-and-certificate"

instance Data.ToQuery CreateKeysAndCertificate where
  toQuery CreateKeysAndCertificate' {..} =
    Prelude.mconcat ["setAsActive" Data.=: setAsActive]

-- | The output of the CreateKeysAndCertificate operation.
--
-- /See:/ 'newCreateKeysAndCertificateResponse' smart constructor.
data CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse'
  { -- | The generated key pair.
    keyPair :: Prelude.Maybe KeyPair,
    -- | The ARN of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate. IoT issues a default subject name for the
    -- certificate (for example, IoT Certificate).
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The certificate data, in PEM format.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeysAndCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyPair', 'createKeysAndCertificateResponse_keyPair' - The generated key pair.
--
-- 'certificateArn', 'createKeysAndCertificateResponse_certificateArn' - The ARN of the certificate.
--
-- 'certificateId', 'createKeysAndCertificateResponse_certificateId' - The ID of the certificate. IoT issues a default subject name for the
-- certificate (for example, IoT Certificate).
--
-- 'certificatePem', 'createKeysAndCertificateResponse_certificatePem' - The certificate data, in PEM format.
--
-- 'httpStatus', 'createKeysAndCertificateResponse_httpStatus' - The response's http status code.
newCreateKeysAndCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateKeysAndCertificateResponse
newCreateKeysAndCertificateResponse pHttpStatus_ =
  CreateKeysAndCertificateResponse'
    { keyPair =
        Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The generated key pair.
createKeysAndCertificateResponse_keyPair :: Lens.Lens' CreateKeysAndCertificateResponse (Prelude.Maybe KeyPair)
createKeysAndCertificateResponse_keyPair = Lens.lens (\CreateKeysAndCertificateResponse' {keyPair} -> keyPair) (\s@CreateKeysAndCertificateResponse' {} a -> s {keyPair = a} :: CreateKeysAndCertificateResponse)

-- | The ARN of the certificate.
createKeysAndCertificateResponse_certificateArn :: Lens.Lens' CreateKeysAndCertificateResponse (Prelude.Maybe Prelude.Text)
createKeysAndCertificateResponse_certificateArn = Lens.lens (\CreateKeysAndCertificateResponse' {certificateArn} -> certificateArn) (\s@CreateKeysAndCertificateResponse' {} a -> s {certificateArn = a} :: CreateKeysAndCertificateResponse)

-- | The ID of the certificate. IoT issues a default subject name for the
-- certificate (for example, IoT Certificate).
createKeysAndCertificateResponse_certificateId :: Lens.Lens' CreateKeysAndCertificateResponse (Prelude.Maybe Prelude.Text)
createKeysAndCertificateResponse_certificateId = Lens.lens (\CreateKeysAndCertificateResponse' {certificateId} -> certificateId) (\s@CreateKeysAndCertificateResponse' {} a -> s {certificateId = a} :: CreateKeysAndCertificateResponse)

-- | The certificate data, in PEM format.
createKeysAndCertificateResponse_certificatePem :: Lens.Lens' CreateKeysAndCertificateResponse (Prelude.Maybe Prelude.Text)
createKeysAndCertificateResponse_certificatePem = Lens.lens (\CreateKeysAndCertificateResponse' {certificatePem} -> certificatePem) (\s@CreateKeysAndCertificateResponse' {} a -> s {certificatePem = a} :: CreateKeysAndCertificateResponse)

-- | The response's http status code.
createKeysAndCertificateResponse_httpStatus :: Lens.Lens' CreateKeysAndCertificateResponse Prelude.Int
createKeysAndCertificateResponse_httpStatus = Lens.lens (\CreateKeysAndCertificateResponse' {httpStatus} -> httpStatus) (\s@CreateKeysAndCertificateResponse' {} a -> s {httpStatus = a} :: CreateKeysAndCertificateResponse)

instance
  Prelude.NFData
    CreateKeysAndCertificateResponse
  where
  rnf CreateKeysAndCertificateResponse' {..} =
    Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf certificatePem
      `Prelude.seq` Prelude.rnf httpStatus
