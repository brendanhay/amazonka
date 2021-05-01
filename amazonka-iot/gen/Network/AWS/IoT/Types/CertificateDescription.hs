{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.CertificateDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateDescription where

import Network.AWS.IoT.Types.CertificateMode
import Network.AWS.IoT.Types.CertificateStatus
import Network.AWS.IoT.Types.CertificateValidity
import Network.AWS.IoT.Types.TransferData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a certificate.
--
-- /See:/ 'newCertificateDescription' smart constructor.
data CertificateDescription = CertificateDescription'
  { -- | The date and time the certificate was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the certificate.
    status :: Prelude.Maybe CertificateStatus,
    -- | The mode of the certificate.
    certificateMode :: Prelude.Maybe CertificateMode,
    -- | The ARN of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS account of the previous owner of the certificate.
    previousOwnedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time the certificate was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the AWS account that owns the certificate.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The customer version of the certificate.
    customerVersion :: Prelude.Maybe Prelude.Natural,
    -- | The generation ID of the certificate.
    generationId :: Prelude.Maybe Prelude.Text,
    -- | The transfer data.
    transferData :: Prelude.Maybe TransferData,
    -- | The ID of the certificate.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The certificate data, in PEM format.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | When the certificate is valid.
    validity :: Prelude.Maybe CertificateValidity,
    -- | The certificate ID of the CA certificate used to sign this certificate.
    caCertificateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CertificateDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'certificateDescription_lastModifiedDate' - The date and time the certificate was last modified.
--
-- 'status', 'certificateDescription_status' - The status of the certificate.
--
-- 'certificateMode', 'certificateDescription_certificateMode' - The mode of the certificate.
--
-- 'certificateArn', 'certificateDescription_certificateArn' - The ARN of the certificate.
--
-- 'previousOwnedBy', 'certificateDescription_previousOwnedBy' - The ID of the AWS account of the previous owner of the certificate.
--
-- 'creationDate', 'certificateDescription_creationDate' - The date and time the certificate was created.
--
-- 'ownedBy', 'certificateDescription_ownedBy' - The ID of the AWS account that owns the certificate.
--
-- 'customerVersion', 'certificateDescription_customerVersion' - The customer version of the certificate.
--
-- 'generationId', 'certificateDescription_generationId' - The generation ID of the certificate.
--
-- 'transferData', 'certificateDescription_transferData' - The transfer data.
--
-- 'certificateId', 'certificateDescription_certificateId' - The ID of the certificate.
--
-- 'certificatePem', 'certificateDescription_certificatePem' - The certificate data, in PEM format.
--
-- 'validity', 'certificateDescription_validity' - When the certificate is valid.
--
-- 'caCertificateId', 'certificateDescription_caCertificateId' - The certificate ID of the CA certificate used to sign this certificate.
newCertificateDescription ::
  CertificateDescription
newCertificateDescription =
  CertificateDescription'
    { lastModifiedDate =
        Prelude.Nothing,
      status = Prelude.Nothing,
      certificateMode = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      previousOwnedBy = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      customerVersion = Prelude.Nothing,
      generationId = Prelude.Nothing,
      transferData = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      validity = Prelude.Nothing,
      caCertificateId = Prelude.Nothing
    }

-- | The date and time the certificate was last modified.
certificateDescription_lastModifiedDate :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.UTCTime)
certificateDescription_lastModifiedDate = Lens.lens (\CertificateDescription' {lastModifiedDate} -> lastModifiedDate) (\s@CertificateDescription' {} a -> s {lastModifiedDate = a} :: CertificateDescription) Prelude.. Lens.mapping Prelude._Time

-- | The status of the certificate.
certificateDescription_status :: Lens.Lens' CertificateDescription (Prelude.Maybe CertificateStatus)
certificateDescription_status = Lens.lens (\CertificateDescription' {status} -> status) (\s@CertificateDescription' {} a -> s {status = a} :: CertificateDescription)

-- | The mode of the certificate.
certificateDescription_certificateMode :: Lens.Lens' CertificateDescription (Prelude.Maybe CertificateMode)
certificateDescription_certificateMode = Lens.lens (\CertificateDescription' {certificateMode} -> certificateMode) (\s@CertificateDescription' {} a -> s {certificateMode = a} :: CertificateDescription)

-- | The ARN of the certificate.
certificateDescription_certificateArn :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_certificateArn = Lens.lens (\CertificateDescription' {certificateArn} -> certificateArn) (\s@CertificateDescription' {} a -> s {certificateArn = a} :: CertificateDescription)

-- | The ID of the AWS account of the previous owner of the certificate.
certificateDescription_previousOwnedBy :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_previousOwnedBy = Lens.lens (\CertificateDescription' {previousOwnedBy} -> previousOwnedBy) (\s@CertificateDescription' {} a -> s {previousOwnedBy = a} :: CertificateDescription)

-- | The date and time the certificate was created.
certificateDescription_creationDate :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.UTCTime)
certificateDescription_creationDate = Lens.lens (\CertificateDescription' {creationDate} -> creationDate) (\s@CertificateDescription' {} a -> s {creationDate = a} :: CertificateDescription) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the AWS account that owns the certificate.
certificateDescription_ownedBy :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_ownedBy = Lens.lens (\CertificateDescription' {ownedBy} -> ownedBy) (\s@CertificateDescription' {} a -> s {ownedBy = a} :: CertificateDescription)

-- | The customer version of the certificate.
certificateDescription_customerVersion :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Natural)
certificateDescription_customerVersion = Lens.lens (\CertificateDescription' {customerVersion} -> customerVersion) (\s@CertificateDescription' {} a -> s {customerVersion = a} :: CertificateDescription)

-- | The generation ID of the certificate.
certificateDescription_generationId :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_generationId = Lens.lens (\CertificateDescription' {generationId} -> generationId) (\s@CertificateDescription' {} a -> s {generationId = a} :: CertificateDescription)

-- | The transfer data.
certificateDescription_transferData :: Lens.Lens' CertificateDescription (Prelude.Maybe TransferData)
certificateDescription_transferData = Lens.lens (\CertificateDescription' {transferData} -> transferData) (\s@CertificateDescription' {} a -> s {transferData = a} :: CertificateDescription)

-- | The ID of the certificate.
certificateDescription_certificateId :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_certificateId = Lens.lens (\CertificateDescription' {certificateId} -> certificateId) (\s@CertificateDescription' {} a -> s {certificateId = a} :: CertificateDescription)

-- | The certificate data, in PEM format.
certificateDescription_certificatePem :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_certificatePem = Lens.lens (\CertificateDescription' {certificatePem} -> certificatePem) (\s@CertificateDescription' {} a -> s {certificatePem = a} :: CertificateDescription)

-- | When the certificate is valid.
certificateDescription_validity :: Lens.Lens' CertificateDescription (Prelude.Maybe CertificateValidity)
certificateDescription_validity = Lens.lens (\CertificateDescription' {validity} -> validity) (\s@CertificateDescription' {} a -> s {validity = a} :: CertificateDescription)

-- | The certificate ID of the CA certificate used to sign this certificate.
certificateDescription_caCertificateId :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_caCertificateId = Lens.lens (\CertificateDescription' {caCertificateId} -> caCertificateId) (\s@CertificateDescription' {} a -> s {caCertificateId = a} :: CertificateDescription)

instance Prelude.FromJSON CertificateDescription where
  parseJSON =
    Prelude.withObject
      "CertificateDescription"
      ( \x ->
          CertificateDescription'
            Prelude.<$> (x Prelude..:? "lastModifiedDate")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "certificateMode")
            Prelude.<*> (x Prelude..:? "certificateArn")
            Prelude.<*> (x Prelude..:? "previousOwnedBy")
            Prelude.<*> (x Prelude..:? "creationDate")
            Prelude.<*> (x Prelude..:? "ownedBy")
            Prelude.<*> (x Prelude..:? "customerVersion")
            Prelude.<*> (x Prelude..:? "generationId")
            Prelude.<*> (x Prelude..:? "transferData")
            Prelude.<*> (x Prelude..:? "certificateId")
            Prelude.<*> (x Prelude..:? "certificatePem")
            Prelude.<*> (x Prelude..:? "validity")
            Prelude.<*> (x Prelude..:? "caCertificateId")
      )

instance Prelude.Hashable CertificateDescription

instance Prelude.NFData CertificateDescription
