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
-- Module      : Amazonka.IoT.RegisterCACertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a CA certificate with Amazon Web Services IoT Core. There is
-- no limit to the number of CA certificates you can register in your
-- Amazon Web Services account. You can register up to 10 CA certificates
-- with the same @CA subject field@ per Amazon Web Services account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RegisterCACertificate>
-- action.
module Amazonka.IoT.RegisterCACertificate
  ( -- * Creating a Request
    RegisterCACertificate (..),
    newRegisterCACertificate,

    -- * Request Lenses
    registerCACertificate_tags,
    registerCACertificate_allowAutoRegistration,
    registerCACertificate_registrationConfig,
    registerCACertificate_verificationCertificate,
    registerCACertificate_setAsActive,
    registerCACertificate_certificateMode,
    registerCACertificate_caCertificate,

    -- * Destructuring the Response
    RegisterCACertificateResponse (..),
    newRegisterCACertificateResponse,

    -- * Response Lenses
    registerCACertificateResponse_certificateArn,
    registerCACertificateResponse_certificateId,
    registerCACertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input to the RegisterCACertificate operation.
--
-- /See:/ 'newRegisterCACertificate' smart constructor.
data RegisterCACertificate = RegisterCACertificate'
  { -- | Metadata which can be used to manage the CA certificate.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe [Tag],
    -- | Allows this CA certificate to be used for auto registration of device
    -- certificates.
    allowAutoRegistration :: Prelude.Maybe Prelude.Bool,
    -- | Information about the registration configuration.
    registrationConfig :: Prelude.Maybe RegistrationConfig,
    -- | The private key verification certificate. If @certificateMode@ is
    -- @SNI_ONLY@, the @verificationCertificate@ field must be empty. If
    -- @certificateMode@ is @DEFAULT@ or not provided, the
    -- @verificationCertificate@ field must not be empty.
    verificationCertificate :: Prelude.Maybe Prelude.Text,
    -- | A boolean value that specifies if the CA certificate is set to active.
    --
    -- Valid values: @ACTIVE | INACTIVE@
    setAsActive :: Prelude.Maybe Prelude.Bool,
    -- | Describes the certificate mode in which the Certificate Authority (CA)
    -- will be registered. If the @verificationCertificate@ field is not
    -- provided, set @certificateMode@ to be @SNI_ONLY@. If the
    -- @verificationCertificate@ field is provided, set @certificateMode@ to be
    -- @DEFAULT@. When @certificateMode@ is not provided, it defaults to
    -- @DEFAULT@. All the device certificates that are registered using this CA
    -- will be registered in the same certificate mode as the CA. For more
    -- information about certificate mode for device certificates, see
    -- <https://docs.aws.amazon.com/iot/latest/apireference/API_CertificateDescription.html#iot-Type-CertificateDescription-certificateMode certificate mode>.
    certificateMode :: Prelude.Maybe CertificateMode,
    -- | The CA certificate.
    caCertificate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'allowAutoRegistration', 'registerCACertificate_allowAutoRegistration' - Allows this CA certificate to be used for auto registration of device
-- certificates.
--
-- 'registrationConfig', 'registerCACertificate_registrationConfig' - Information about the registration configuration.
--
-- 'verificationCertificate', 'registerCACertificate_verificationCertificate' - The private key verification certificate. If @certificateMode@ is
-- @SNI_ONLY@, the @verificationCertificate@ field must be empty. If
-- @certificateMode@ is @DEFAULT@ or not provided, the
-- @verificationCertificate@ field must not be empty.
--
-- 'setAsActive', 'registerCACertificate_setAsActive' - A boolean value that specifies if the CA certificate is set to active.
--
-- Valid values: @ACTIVE | INACTIVE@
--
-- 'certificateMode', 'registerCACertificate_certificateMode' - Describes the certificate mode in which the Certificate Authority (CA)
-- will be registered. If the @verificationCertificate@ field is not
-- provided, set @certificateMode@ to be @SNI_ONLY@. If the
-- @verificationCertificate@ field is provided, set @certificateMode@ to be
-- @DEFAULT@. When @certificateMode@ is not provided, it defaults to
-- @DEFAULT@. All the device certificates that are registered using this CA
-- will be registered in the same certificate mode as the CA. For more
-- information about certificate mode for device certificates, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CertificateDescription.html#iot-Type-CertificateDescription-certificateMode certificate mode>.
--
-- 'caCertificate', 'registerCACertificate_caCertificate' - The CA certificate.
newRegisterCACertificate ::
  -- | 'caCertificate'
  Prelude.Text ->
  RegisterCACertificate
newRegisterCACertificate pCaCertificate_ =
  RegisterCACertificate'
    { tags = Prelude.Nothing,
      allowAutoRegistration = Prelude.Nothing,
      registrationConfig = Prelude.Nothing,
      verificationCertificate = Prelude.Nothing,
      setAsActive = Prelude.Nothing,
      certificateMode = Prelude.Nothing,
      caCertificate = pCaCertificate_
    }

-- | Metadata which can be used to manage the CA certificate.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
registerCACertificate_tags :: Lens.Lens' RegisterCACertificate (Prelude.Maybe [Tag])
registerCACertificate_tags = Lens.lens (\RegisterCACertificate' {tags} -> tags) (\s@RegisterCACertificate' {} a -> s {tags = a} :: RegisterCACertificate) Prelude.. Lens.mapping Lens.coerced

-- | Allows this CA certificate to be used for auto registration of device
-- certificates.
registerCACertificate_allowAutoRegistration :: Lens.Lens' RegisterCACertificate (Prelude.Maybe Prelude.Bool)
registerCACertificate_allowAutoRegistration = Lens.lens (\RegisterCACertificate' {allowAutoRegistration} -> allowAutoRegistration) (\s@RegisterCACertificate' {} a -> s {allowAutoRegistration = a} :: RegisterCACertificate)

-- | Information about the registration configuration.
registerCACertificate_registrationConfig :: Lens.Lens' RegisterCACertificate (Prelude.Maybe RegistrationConfig)
registerCACertificate_registrationConfig = Lens.lens (\RegisterCACertificate' {registrationConfig} -> registrationConfig) (\s@RegisterCACertificate' {} a -> s {registrationConfig = a} :: RegisterCACertificate)

-- | The private key verification certificate. If @certificateMode@ is
-- @SNI_ONLY@, the @verificationCertificate@ field must be empty. If
-- @certificateMode@ is @DEFAULT@ or not provided, the
-- @verificationCertificate@ field must not be empty.
registerCACertificate_verificationCertificate :: Lens.Lens' RegisterCACertificate (Prelude.Maybe Prelude.Text)
registerCACertificate_verificationCertificate = Lens.lens (\RegisterCACertificate' {verificationCertificate} -> verificationCertificate) (\s@RegisterCACertificate' {} a -> s {verificationCertificate = a} :: RegisterCACertificate)

-- | A boolean value that specifies if the CA certificate is set to active.
--
-- Valid values: @ACTIVE | INACTIVE@
registerCACertificate_setAsActive :: Lens.Lens' RegisterCACertificate (Prelude.Maybe Prelude.Bool)
registerCACertificate_setAsActive = Lens.lens (\RegisterCACertificate' {setAsActive} -> setAsActive) (\s@RegisterCACertificate' {} a -> s {setAsActive = a} :: RegisterCACertificate)

-- | Describes the certificate mode in which the Certificate Authority (CA)
-- will be registered. If the @verificationCertificate@ field is not
-- provided, set @certificateMode@ to be @SNI_ONLY@. If the
-- @verificationCertificate@ field is provided, set @certificateMode@ to be
-- @DEFAULT@. When @certificateMode@ is not provided, it defaults to
-- @DEFAULT@. All the device certificates that are registered using this CA
-- will be registered in the same certificate mode as the CA. For more
-- information about certificate mode for device certificates, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CertificateDescription.html#iot-Type-CertificateDescription-certificateMode certificate mode>.
registerCACertificate_certificateMode :: Lens.Lens' RegisterCACertificate (Prelude.Maybe CertificateMode)
registerCACertificate_certificateMode = Lens.lens (\RegisterCACertificate' {certificateMode} -> certificateMode) (\s@RegisterCACertificate' {} a -> s {certificateMode = a} :: RegisterCACertificate)

-- | The CA certificate.
registerCACertificate_caCertificate :: Lens.Lens' RegisterCACertificate Prelude.Text
registerCACertificate_caCertificate = Lens.lens (\RegisterCACertificate' {caCertificate} -> caCertificate) (\s@RegisterCACertificate' {} a -> s {caCertificate = a} :: RegisterCACertificate)

instance Core.AWSRequest RegisterCACertificate where
  type
    AWSResponse RegisterCACertificate =
      RegisterCACertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegisterCACertificateResponse'
            Prelude.<$> (x Core..?> "certificateArn")
            Prelude.<*> (x Core..?> "certificateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterCACertificate where
  hashWithSalt _salt RegisterCACertificate' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` allowAutoRegistration
      `Prelude.hashWithSalt` registrationConfig
      `Prelude.hashWithSalt` verificationCertificate
      `Prelude.hashWithSalt` setAsActive
      `Prelude.hashWithSalt` certificateMode
      `Prelude.hashWithSalt` caCertificate

instance Prelude.NFData RegisterCACertificate where
  rnf RegisterCACertificate' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf allowAutoRegistration
      `Prelude.seq` Prelude.rnf registrationConfig
      `Prelude.seq` Prelude.rnf verificationCertificate
      `Prelude.seq` Prelude.rnf setAsActive
      `Prelude.seq` Prelude.rnf certificateMode
      `Prelude.seq` Prelude.rnf caCertificate

instance Core.ToHeaders RegisterCACertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON RegisterCACertificate where
  toJSON RegisterCACertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("registrationConfig" Core..=)
              Prelude.<$> registrationConfig,
            ("verificationCertificate" Core..=)
              Prelude.<$> verificationCertificate,
            ("certificateMode" Core..=)
              Prelude.<$> certificateMode,
            Prelude.Just
              ("caCertificate" Core..= caCertificate)
          ]
      )

instance Core.ToPath RegisterCACertificate where
  toPath = Prelude.const "/cacertificate"

instance Core.ToQuery RegisterCACertificate where
  toQuery RegisterCACertificate' {..} =
    Prelude.mconcat
      [ "allowAutoRegistration"
          Core.=: allowAutoRegistration,
        "setAsActive" Core.=: setAsActive
      ]

-- | The output from the RegisterCACertificateResponse operation.
--
-- /See:/ 'newRegisterCACertificateResponse' smart constructor.
data RegisterCACertificateResponse = RegisterCACertificateResponse'
  { -- | The CA certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The CA certificate identifier.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  RegisterCACertificateResponse
newRegisterCACertificateResponse pHttpStatus_ =
  RegisterCACertificateResponse'
    { certificateArn =
        Prelude.Nothing,
      certificateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The CA certificate ARN.
registerCACertificateResponse_certificateArn :: Lens.Lens' RegisterCACertificateResponse (Prelude.Maybe Prelude.Text)
registerCACertificateResponse_certificateArn = Lens.lens (\RegisterCACertificateResponse' {certificateArn} -> certificateArn) (\s@RegisterCACertificateResponse' {} a -> s {certificateArn = a} :: RegisterCACertificateResponse)

-- | The CA certificate identifier.
registerCACertificateResponse_certificateId :: Lens.Lens' RegisterCACertificateResponse (Prelude.Maybe Prelude.Text)
registerCACertificateResponse_certificateId = Lens.lens (\RegisterCACertificateResponse' {certificateId} -> certificateId) (\s@RegisterCACertificateResponse' {} a -> s {certificateId = a} :: RegisterCACertificateResponse)

-- | The response's http status code.
registerCACertificateResponse_httpStatus :: Lens.Lens' RegisterCACertificateResponse Prelude.Int
registerCACertificateResponse_httpStatus = Lens.lens (\RegisterCACertificateResponse' {httpStatus} -> httpStatus) (\s@RegisterCACertificateResponse' {} a -> s {httpStatus = a} :: RegisterCACertificateResponse)

instance Prelude.NFData RegisterCACertificateResponse where
  rnf RegisterCACertificateResponse' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf httpStatus
