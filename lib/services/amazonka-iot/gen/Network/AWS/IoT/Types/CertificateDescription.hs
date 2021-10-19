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

import qualified Network.AWS.Core as Core
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
  { -- | The status of the certificate.
    status :: Prelude.Maybe CertificateStatus,
    -- | The ID of the Amazon Web Services account that owns the certificate.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time the certificate was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The certificate ID of the CA certificate used to sign this certificate.
    caCertificateId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account of the previous owner of the
    -- certificate.
    previousOwnedBy :: Prelude.Maybe Prelude.Text,
    -- | The certificate data, in PEM format.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The mode of the certificate.
    certificateMode :: Prelude.Maybe CertificateMode,
    -- | When the certificate is valid.
    validity :: Prelude.Maybe CertificateValidity,
    -- | The date and time the certificate was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The generation ID of the certificate.
    generationId :: Prelude.Maybe Prelude.Text,
    -- | The transfer data.
    transferData :: Prelude.Maybe TransferData,
    -- | The customer version of the certificate.
    customerVersion :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'certificateDescription_status' - The status of the certificate.
--
-- 'ownedBy', 'certificateDescription_ownedBy' - The ID of the Amazon Web Services account that owns the certificate.
--
-- 'lastModifiedDate', 'certificateDescription_lastModifiedDate' - The date and time the certificate was last modified.
--
-- 'caCertificateId', 'certificateDescription_caCertificateId' - The certificate ID of the CA certificate used to sign this certificate.
--
-- 'previousOwnedBy', 'certificateDescription_previousOwnedBy' - The ID of the Amazon Web Services account of the previous owner of the
-- certificate.
--
-- 'certificatePem', 'certificateDescription_certificatePem' - The certificate data, in PEM format.
--
-- 'certificateArn', 'certificateDescription_certificateArn' - The ARN of the certificate.
--
-- 'certificateId', 'certificateDescription_certificateId' - The ID of the certificate.
--
-- 'certificateMode', 'certificateDescription_certificateMode' - The mode of the certificate.
--
-- 'validity', 'certificateDescription_validity' - When the certificate is valid.
--
-- 'creationDate', 'certificateDescription_creationDate' - The date and time the certificate was created.
--
-- 'generationId', 'certificateDescription_generationId' - The generation ID of the certificate.
--
-- 'transferData', 'certificateDescription_transferData' - The transfer data.
--
-- 'customerVersion', 'certificateDescription_customerVersion' - The customer version of the certificate.
newCertificateDescription ::
  CertificateDescription
newCertificateDescription =
  CertificateDescription'
    { status = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      caCertificateId = Prelude.Nothing,
      previousOwnedBy = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificateMode = Prelude.Nothing,
      validity = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      generationId = Prelude.Nothing,
      transferData = Prelude.Nothing,
      customerVersion = Prelude.Nothing
    }

-- | The status of the certificate.
certificateDescription_status :: Lens.Lens' CertificateDescription (Prelude.Maybe CertificateStatus)
certificateDescription_status = Lens.lens (\CertificateDescription' {status} -> status) (\s@CertificateDescription' {} a -> s {status = a} :: CertificateDescription)

-- | The ID of the Amazon Web Services account that owns the certificate.
certificateDescription_ownedBy :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_ownedBy = Lens.lens (\CertificateDescription' {ownedBy} -> ownedBy) (\s@CertificateDescription' {} a -> s {ownedBy = a} :: CertificateDescription)

-- | The date and time the certificate was last modified.
certificateDescription_lastModifiedDate :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.UTCTime)
certificateDescription_lastModifiedDate = Lens.lens (\CertificateDescription' {lastModifiedDate} -> lastModifiedDate) (\s@CertificateDescription' {} a -> s {lastModifiedDate = a} :: CertificateDescription) Prelude.. Lens.mapping Core._Time

-- | The certificate ID of the CA certificate used to sign this certificate.
certificateDescription_caCertificateId :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_caCertificateId = Lens.lens (\CertificateDescription' {caCertificateId} -> caCertificateId) (\s@CertificateDescription' {} a -> s {caCertificateId = a} :: CertificateDescription)

-- | The ID of the Amazon Web Services account of the previous owner of the
-- certificate.
certificateDescription_previousOwnedBy :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_previousOwnedBy = Lens.lens (\CertificateDescription' {previousOwnedBy} -> previousOwnedBy) (\s@CertificateDescription' {} a -> s {previousOwnedBy = a} :: CertificateDescription)

-- | The certificate data, in PEM format.
certificateDescription_certificatePem :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_certificatePem = Lens.lens (\CertificateDescription' {certificatePem} -> certificatePem) (\s@CertificateDescription' {} a -> s {certificatePem = a} :: CertificateDescription)

-- | The ARN of the certificate.
certificateDescription_certificateArn :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_certificateArn = Lens.lens (\CertificateDescription' {certificateArn} -> certificateArn) (\s@CertificateDescription' {} a -> s {certificateArn = a} :: CertificateDescription)

-- | The ID of the certificate.
certificateDescription_certificateId :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_certificateId = Lens.lens (\CertificateDescription' {certificateId} -> certificateId) (\s@CertificateDescription' {} a -> s {certificateId = a} :: CertificateDescription)

-- | The mode of the certificate.
certificateDescription_certificateMode :: Lens.Lens' CertificateDescription (Prelude.Maybe CertificateMode)
certificateDescription_certificateMode = Lens.lens (\CertificateDescription' {certificateMode} -> certificateMode) (\s@CertificateDescription' {} a -> s {certificateMode = a} :: CertificateDescription)

-- | When the certificate is valid.
certificateDescription_validity :: Lens.Lens' CertificateDescription (Prelude.Maybe CertificateValidity)
certificateDescription_validity = Lens.lens (\CertificateDescription' {validity} -> validity) (\s@CertificateDescription' {} a -> s {validity = a} :: CertificateDescription)

-- | The date and time the certificate was created.
certificateDescription_creationDate :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.UTCTime)
certificateDescription_creationDate = Lens.lens (\CertificateDescription' {creationDate} -> creationDate) (\s@CertificateDescription' {} a -> s {creationDate = a} :: CertificateDescription) Prelude.. Lens.mapping Core._Time

-- | The generation ID of the certificate.
certificateDescription_generationId :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Text)
certificateDescription_generationId = Lens.lens (\CertificateDescription' {generationId} -> generationId) (\s@CertificateDescription' {} a -> s {generationId = a} :: CertificateDescription)

-- | The transfer data.
certificateDescription_transferData :: Lens.Lens' CertificateDescription (Prelude.Maybe TransferData)
certificateDescription_transferData = Lens.lens (\CertificateDescription' {transferData} -> transferData) (\s@CertificateDescription' {} a -> s {transferData = a} :: CertificateDescription)

-- | The customer version of the certificate.
certificateDescription_customerVersion :: Lens.Lens' CertificateDescription (Prelude.Maybe Prelude.Natural)
certificateDescription_customerVersion = Lens.lens (\CertificateDescription' {customerVersion} -> customerVersion) (\s@CertificateDescription' {} a -> s {customerVersion = a} :: CertificateDescription)

instance Core.FromJSON CertificateDescription where
  parseJSON =
    Core.withObject
      "CertificateDescription"
      ( \x ->
          CertificateDescription'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "ownedBy")
            Prelude.<*> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "caCertificateId")
            Prelude.<*> (x Core..:? "previousOwnedBy")
            Prelude.<*> (x Core..:? "certificatePem")
            Prelude.<*> (x Core..:? "certificateArn")
            Prelude.<*> (x Core..:? "certificateId")
            Prelude.<*> (x Core..:? "certificateMode")
            Prelude.<*> (x Core..:? "validity")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "generationId")
            Prelude.<*> (x Core..:? "transferData")
            Prelude.<*> (x Core..:? "customerVersion")
      )

instance Prelude.Hashable CertificateDescription

instance Prelude.NFData CertificateDescription
