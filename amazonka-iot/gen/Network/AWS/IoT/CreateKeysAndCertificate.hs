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
-- Module      : Network.AWS.IoT.CreateKeysAndCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- __Note__ This is the only time AWS IoT issues the private key for this
-- certificate, so it is important to keep it in a secure location.
module Network.AWS.IoT.CreateKeysAndCertificate
  ( -- * Creating a Request
    CreateKeysAndCertificate (..),
    newCreateKeysAndCertificate,

    -- * Request Lenses
    createKeysAndCertificate_setAsActive,

    -- * Destructuring the Response
    CreateKeysAndCertificateResponse (..),
    newCreateKeysAndCertificateResponse,

    -- * Response Lenses
    createKeysAndCertificateResponse_certificateArn,
    createKeysAndCertificateResponse_keyPair,
    createKeysAndCertificateResponse_certificateId,
    createKeysAndCertificateResponse_certificatePem,
    createKeysAndCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateKeysAndCertificate operation.
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeysAndCertificateResponse'
            Prelude.<$> (x Core..?> "certificateArn")
            Prelude.<*> (x Core..?> "keyPair")
            Prelude.<*> (x Core..?> "certificateId")
            Prelude.<*> (x Core..?> "certificatePem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateKeysAndCertificate

instance Prelude.NFData CreateKeysAndCertificate

instance Core.ToHeaders CreateKeysAndCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateKeysAndCertificate where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath CreateKeysAndCertificate where
  toPath = Prelude.const "/keys-and-certificate"

instance Core.ToQuery CreateKeysAndCertificate where
  toQuery CreateKeysAndCertificate' {..} =
    Prelude.mconcat ["setAsActive" Core.=: setAsActive]

-- | The output of the CreateKeysAndCertificate operation.
--
-- /See:/ 'newCreateKeysAndCertificateResponse' smart constructor.
data CreateKeysAndCertificateResponse = CreateKeysAndCertificateResponse'
  { -- | The ARN of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The generated key pair.
    keyPair :: Prelude.Maybe KeyPair,
    -- | The ID of the certificate. AWS IoT issues a default subject name for the
    -- certificate (for example, AWS IoT Certificate).
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
-- 'certificateArn', 'createKeysAndCertificateResponse_certificateArn' - The ARN of the certificate.
--
-- 'keyPair', 'createKeysAndCertificateResponse_keyPair' - The generated key pair.
--
-- 'certificateId', 'createKeysAndCertificateResponse_certificateId' - The ID of the certificate. AWS IoT issues a default subject name for the
-- certificate (for example, AWS IoT Certificate).
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
    { certificateArn =
        Prelude.Nothing,
      keyPair = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the certificate.
createKeysAndCertificateResponse_certificateArn :: Lens.Lens' CreateKeysAndCertificateResponse (Prelude.Maybe Prelude.Text)
createKeysAndCertificateResponse_certificateArn = Lens.lens (\CreateKeysAndCertificateResponse' {certificateArn} -> certificateArn) (\s@CreateKeysAndCertificateResponse' {} a -> s {certificateArn = a} :: CreateKeysAndCertificateResponse)

-- | The generated key pair.
createKeysAndCertificateResponse_keyPair :: Lens.Lens' CreateKeysAndCertificateResponse (Prelude.Maybe KeyPair)
createKeysAndCertificateResponse_keyPair = Lens.lens (\CreateKeysAndCertificateResponse' {keyPair} -> keyPair) (\s@CreateKeysAndCertificateResponse' {} a -> s {keyPair = a} :: CreateKeysAndCertificateResponse)

-- | The ID of the certificate. AWS IoT issues a default subject name for the
-- certificate (for example, AWS IoT Certificate).
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
