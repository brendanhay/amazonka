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
-- Module      : Network.AWS.IoT.RegisterCACertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a CA certificate with AWS IoT. This CA certificate can then be
-- used to sign device certificates, which can be then registered with AWS
-- IoT. You can register up to 10 CA certificates per AWS account that have
-- the same subject field. This enables you to have up to 10 certificate
-- authorities sign your device certificates. If you have more than one CA
-- certificate registered, make sure you pass the CA certificate when you
-- register your device certificates with the RegisterCertificate API.
module Network.AWS.IoT.RegisterCACertificate
  ( -- * Creating a Request
    RegisterCACertificate (..),
    newRegisterCACertificate,

    -- * Request Lenses
    registerCACertificate_allowAutoRegistration,
    registerCACertificate_setAsActive,
    registerCACertificate_tags,
    registerCACertificate_registrationConfig,
    registerCACertificate_caCertificate,
    registerCACertificate_verificationCertificate,

    -- * Destructuring the Response
    RegisterCACertificateResponse (..),
    newRegisterCACertificateResponse,

    -- * Response Lenses
    registerCACertificateResponse_certificateArn,
    registerCACertificateResponse_certificateId,
    registerCACertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input to the RegisterCACertificate operation.
--
-- /See:/ 'newRegisterCACertificate' smart constructor.
data RegisterCACertificate = RegisterCACertificate'
  { -- | Allows this CA certificate to be used for auto registration of device
    -- certificates.
    allowAutoRegistration :: Core.Maybe Core.Bool,
    -- | A boolean value that specifies if the CA certificate is set to active.
    setAsActive :: Core.Maybe Core.Bool,
    -- | Metadata which can be used to manage the CA certificate.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Core.Maybe [Tag],
    -- | Information about the registration configuration.
    registrationConfig :: Core.Maybe RegistrationConfig,
    -- | The CA certificate.
    caCertificate :: Core.Text,
    -- | The private key verification certificate.
    verificationCertificate :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowAutoRegistration', 'registerCACertificate_allowAutoRegistration' - Allows this CA certificate to be used for auto registration of device
-- certificates.
--
-- 'setAsActive', 'registerCACertificate_setAsActive' - A boolean value that specifies if the CA certificate is set to active.
--
-- 'tags', 'registerCACertificate_tags' - Metadata which can be used to manage the CA certificate.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'registrationConfig', 'registerCACertificate_registrationConfig' - Information about the registration configuration.
--
-- 'caCertificate', 'registerCACertificate_caCertificate' - The CA certificate.
--
-- 'verificationCertificate', 'registerCACertificate_verificationCertificate' - The private key verification certificate.
newRegisterCACertificate ::
  -- | 'caCertificate'
  Core.Text ->
  -- | 'verificationCertificate'
  Core.Text ->
  RegisterCACertificate
newRegisterCACertificate
  pCaCertificate_
  pVerificationCertificate_ =
    RegisterCACertificate'
      { allowAutoRegistration =
          Core.Nothing,
        setAsActive = Core.Nothing,
        tags = Core.Nothing,
        registrationConfig = Core.Nothing,
        caCertificate = pCaCertificate_,
        verificationCertificate = pVerificationCertificate_
      }

-- | Allows this CA certificate to be used for auto registration of device
-- certificates.
registerCACertificate_allowAutoRegistration :: Lens.Lens' RegisterCACertificate (Core.Maybe Core.Bool)
registerCACertificate_allowAutoRegistration = Lens.lens (\RegisterCACertificate' {allowAutoRegistration} -> allowAutoRegistration) (\s@RegisterCACertificate' {} a -> s {allowAutoRegistration = a} :: RegisterCACertificate)

-- | A boolean value that specifies if the CA certificate is set to active.
registerCACertificate_setAsActive :: Lens.Lens' RegisterCACertificate (Core.Maybe Core.Bool)
registerCACertificate_setAsActive = Lens.lens (\RegisterCACertificate' {setAsActive} -> setAsActive) (\s@RegisterCACertificate' {} a -> s {setAsActive = a} :: RegisterCACertificate)

-- | Metadata which can be used to manage the CA certificate.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
registerCACertificate_tags :: Lens.Lens' RegisterCACertificate (Core.Maybe [Tag])
registerCACertificate_tags = Lens.lens (\RegisterCACertificate' {tags} -> tags) (\s@RegisterCACertificate' {} a -> s {tags = a} :: RegisterCACertificate) Core.. Lens.mapping Lens._Coerce

-- | Information about the registration configuration.
registerCACertificate_registrationConfig :: Lens.Lens' RegisterCACertificate (Core.Maybe RegistrationConfig)
registerCACertificate_registrationConfig = Lens.lens (\RegisterCACertificate' {registrationConfig} -> registrationConfig) (\s@RegisterCACertificate' {} a -> s {registrationConfig = a} :: RegisterCACertificate)

-- | The CA certificate.
registerCACertificate_caCertificate :: Lens.Lens' RegisterCACertificate Core.Text
registerCACertificate_caCertificate = Lens.lens (\RegisterCACertificate' {caCertificate} -> caCertificate) (\s@RegisterCACertificate' {} a -> s {caCertificate = a} :: RegisterCACertificate)

-- | The private key verification certificate.
registerCACertificate_verificationCertificate :: Lens.Lens' RegisterCACertificate Core.Text
registerCACertificate_verificationCertificate = Lens.lens (\RegisterCACertificate' {verificationCertificate} -> verificationCertificate) (\s@RegisterCACertificate' {} a -> s {verificationCertificate = a} :: RegisterCACertificate)

instance Core.AWSRequest RegisterCACertificate where
  type
    AWSResponse RegisterCACertificate =
      RegisterCACertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCACertificateResponse'
            Core.<$> (x Core..?> "certificateArn")
            Core.<*> (x Core..?> "certificateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable RegisterCACertificate

instance Core.NFData RegisterCACertificate

instance Core.ToHeaders RegisterCACertificate where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON RegisterCACertificate where
  toJSON RegisterCACertificate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("registrationConfig" Core..=)
              Core.<$> registrationConfig,
            Core.Just ("caCertificate" Core..= caCertificate),
            Core.Just
              ( "verificationCertificate"
                  Core..= verificationCertificate
              )
          ]
      )

instance Core.ToPath RegisterCACertificate where
  toPath = Core.const "/cacertificate"

instance Core.ToQuery RegisterCACertificate where
  toQuery RegisterCACertificate' {..} =
    Core.mconcat
      [ "allowAutoRegistration"
          Core.=: allowAutoRegistration,
        "setAsActive" Core.=: setAsActive
      ]

-- | The output from the RegisterCACertificateResponse operation.
--
-- /See:/ 'newRegisterCACertificateResponse' smart constructor.
data RegisterCACertificateResponse = RegisterCACertificateResponse'
  { -- | The CA certificate ARN.
    certificateArn :: Core.Maybe Core.Text,
    -- | The CA certificate identifier.
    certificateId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterCACertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'registerCACertificateResponse_certificateArn' - The CA certificate ARN.
--
-- 'certificateId', 'registerCACertificateResponse_certificateId' - The CA certificate identifier.
--
-- 'httpStatus', 'registerCACertificateResponse_httpStatus' - The response's http status code.
newRegisterCACertificateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterCACertificateResponse
newRegisterCACertificateResponse pHttpStatus_ =
  RegisterCACertificateResponse'
    { certificateArn =
        Core.Nothing,
      certificateId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CA certificate ARN.
registerCACertificateResponse_certificateArn :: Lens.Lens' RegisterCACertificateResponse (Core.Maybe Core.Text)
registerCACertificateResponse_certificateArn = Lens.lens (\RegisterCACertificateResponse' {certificateArn} -> certificateArn) (\s@RegisterCACertificateResponse' {} a -> s {certificateArn = a} :: RegisterCACertificateResponse)

-- | The CA certificate identifier.
registerCACertificateResponse_certificateId :: Lens.Lens' RegisterCACertificateResponse (Core.Maybe Core.Text)
registerCACertificateResponse_certificateId = Lens.lens (\RegisterCACertificateResponse' {certificateId} -> certificateId) (\s@RegisterCACertificateResponse' {} a -> s {certificateId = a} :: RegisterCACertificateResponse)

-- | The response's http status code.
registerCACertificateResponse_httpStatus :: Lens.Lens' RegisterCACertificateResponse Core.Int
registerCACertificateResponse_httpStatus = Lens.lens (\RegisterCACertificateResponse' {httpStatus} -> httpStatus) (\s@RegisterCACertificateResponse' {} a -> s {httpStatus = a} :: RegisterCACertificateResponse)

instance Core.NFData RegisterCACertificateResponse
