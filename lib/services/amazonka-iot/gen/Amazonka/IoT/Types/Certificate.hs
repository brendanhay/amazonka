{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.IoT.Types.Certificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Certificate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.CertificateMode
import Amazonka.IoT.Types.CertificateStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about a certificate.
--
-- /See:/ 'newCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The ARN of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The mode of the certificate.
    --
    -- @DEFAULT@: A certificate in @DEFAULT@ mode is either generated by Amazon
    -- Web Services IoT Core or registered with an issuer certificate authority
    -- (CA) in @DEFAULT@ mode. Devices with certificates in @DEFAULT@ mode
    -- aren\'t required to send the Server Name Indication (SNI) extension when
    -- connecting to Amazon Web Services IoT Core. However, to use features
    -- such as custom domains and VPC endpoints, we recommend that you use the
    -- SNI extension when connecting to Amazon Web Services IoT Core.
    --
    -- @SNI_ONLY@: A certificate in @SNI_ONLY@ mode is registered without an
    -- issuer CA. Devices with certificates in @SNI_ONLY@ mode must send the
    -- SNI extension when connecting to Amazon Web Services IoT Core.
    certificateMode :: Prelude.Maybe CertificateMode,
    -- | The date and time the certificate was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The status of the certificate.
    --
    -- The status value REGISTER_INACTIVE is deprecated and should not be used.
    status :: Prelude.Maybe CertificateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Certificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'certificate_certificateArn' - The ARN of the certificate.
--
-- 'certificateId', 'certificate_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
--
-- 'certificateMode', 'certificate_certificateMode' - The mode of the certificate.
--
-- @DEFAULT@: A certificate in @DEFAULT@ mode is either generated by Amazon
-- Web Services IoT Core or registered with an issuer certificate authority
-- (CA) in @DEFAULT@ mode. Devices with certificates in @DEFAULT@ mode
-- aren\'t required to send the Server Name Indication (SNI) extension when
-- connecting to Amazon Web Services IoT Core. However, to use features
-- such as custom domains and VPC endpoints, we recommend that you use the
-- SNI extension when connecting to Amazon Web Services IoT Core.
--
-- @SNI_ONLY@: A certificate in @SNI_ONLY@ mode is registered without an
-- issuer CA. Devices with certificates in @SNI_ONLY@ mode must send the
-- SNI extension when connecting to Amazon Web Services IoT Core.
--
-- 'creationDate', 'certificate_creationDate' - The date and time the certificate was created.
--
-- 'status', 'certificate_status' - The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
newCertificate ::
  Certificate
newCertificate =
  Certificate'
    { certificateArn = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificateMode = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The ARN of the certificate.
certificate_certificateArn :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateArn = Lens.lens (\Certificate' {certificateArn} -> certificateArn) (\s@Certificate' {} a -> s {certificateArn = a} :: Certificate)

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
certificate_certificateId :: Lens.Lens' Certificate (Prelude.Maybe Prelude.Text)
certificate_certificateId = Lens.lens (\Certificate' {certificateId} -> certificateId) (\s@Certificate' {} a -> s {certificateId = a} :: Certificate)

-- | The mode of the certificate.
--
-- @DEFAULT@: A certificate in @DEFAULT@ mode is either generated by Amazon
-- Web Services IoT Core or registered with an issuer certificate authority
-- (CA) in @DEFAULT@ mode. Devices with certificates in @DEFAULT@ mode
-- aren\'t required to send the Server Name Indication (SNI) extension when
-- connecting to Amazon Web Services IoT Core. However, to use features
-- such as custom domains and VPC endpoints, we recommend that you use the
-- SNI extension when connecting to Amazon Web Services IoT Core.
--
-- @SNI_ONLY@: A certificate in @SNI_ONLY@ mode is registered without an
-- issuer CA. Devices with certificates in @SNI_ONLY@ mode must send the
-- SNI extension when connecting to Amazon Web Services IoT Core.
certificate_certificateMode :: Lens.Lens' Certificate (Prelude.Maybe CertificateMode)
certificate_certificateMode = Lens.lens (\Certificate' {certificateMode} -> certificateMode) (\s@Certificate' {} a -> s {certificateMode = a} :: Certificate)

-- | The date and time the certificate was created.
certificate_creationDate :: Lens.Lens' Certificate (Prelude.Maybe Prelude.UTCTime)
certificate_creationDate = Lens.lens (\Certificate' {creationDate} -> creationDate) (\s@Certificate' {} a -> s {creationDate = a} :: Certificate) Prelude.. Lens.mapping Data._Time

-- | The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
certificate_status :: Lens.Lens' Certificate (Prelude.Maybe CertificateStatus)
certificate_status = Lens.lens (\Certificate' {status} -> status) (\s@Certificate' {} a -> s {status = a} :: Certificate)

instance Data.FromJSON Certificate where
  parseJSON =
    Data.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Prelude.<$> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "certificateId")
            Prelude.<*> (x Data..:? "certificateMode")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable Certificate where
  hashWithSalt _salt Certificate' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` certificateMode
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status

instance Prelude.NFData Certificate where
  rnf Certificate' {..} =
    Prelude.rnf certificateArn `Prelude.seq`
      Prelude.rnf certificateId `Prelude.seq`
        Prelude.rnf certificateMode `Prelude.seq`
          Prelude.rnf creationDate `Prelude.seq`
            Prelude.rnf status
