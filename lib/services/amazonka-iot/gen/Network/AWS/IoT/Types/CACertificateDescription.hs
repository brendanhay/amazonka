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
-- Module      : Network.AWS.IoT.Types.CACertificateDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CACertificateDescription where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.AutoRegistrationStatus
import Network.AWS.IoT.Types.CACertificateStatus
import Network.AWS.IoT.Types.CertificateValidity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a CA certificate.
--
-- /See:/ 'newCACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
  { -- | The date the CA certificate was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The status of a CA certificate.
    status :: Prelude.Maybe CACertificateStatus,
    -- | The CA certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The date the CA certificate was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The owner of the CA certificate.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The customer version of the CA certificate.
    customerVersion :: Prelude.Maybe Prelude.Natural,
    -- | The generation ID of the CA certificate.
    generationId :: Prelude.Maybe Prelude.Text,
    -- | The CA certificate ID.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The CA certificate data, in PEM format.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | When the CA certificate is valid.
    validity :: Prelude.Maybe CertificateValidity,
    -- | Whether the CA certificate configured for auto registration of device
    -- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
    autoRegistrationStatus :: Prelude.Maybe AutoRegistrationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CACertificateDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'cACertificateDescription_lastModifiedDate' - The date the CA certificate was last modified.
--
-- 'status', 'cACertificateDescription_status' - The status of a CA certificate.
--
-- 'certificateArn', 'cACertificateDescription_certificateArn' - The CA certificate ARN.
--
-- 'creationDate', 'cACertificateDescription_creationDate' - The date the CA certificate was created.
--
-- 'ownedBy', 'cACertificateDescription_ownedBy' - The owner of the CA certificate.
--
-- 'customerVersion', 'cACertificateDescription_customerVersion' - The customer version of the CA certificate.
--
-- 'generationId', 'cACertificateDescription_generationId' - The generation ID of the CA certificate.
--
-- 'certificateId', 'cACertificateDescription_certificateId' - The CA certificate ID.
--
-- 'certificatePem', 'cACertificateDescription_certificatePem' - The CA certificate data, in PEM format.
--
-- 'validity', 'cACertificateDescription_validity' - When the CA certificate is valid.
--
-- 'autoRegistrationStatus', 'cACertificateDescription_autoRegistrationStatus' - Whether the CA certificate configured for auto registration of device
-- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
newCACertificateDescription ::
  CACertificateDescription
newCACertificateDescription =
  CACertificateDescription'
    { lastModifiedDate =
        Prelude.Nothing,
      status = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      customerVersion = Prelude.Nothing,
      generationId = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      validity = Prelude.Nothing,
      autoRegistrationStatus = Prelude.Nothing
    }

-- | The date the CA certificate was last modified.
cACertificateDescription_lastModifiedDate :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.UTCTime)
cACertificateDescription_lastModifiedDate = Lens.lens (\CACertificateDescription' {lastModifiedDate} -> lastModifiedDate) (\s@CACertificateDescription' {} a -> s {lastModifiedDate = a} :: CACertificateDescription) Prelude.. Lens.mapping Core._Time

-- | The status of a CA certificate.
cACertificateDescription_status :: Lens.Lens' CACertificateDescription (Prelude.Maybe CACertificateStatus)
cACertificateDescription_status = Lens.lens (\CACertificateDescription' {status} -> status) (\s@CACertificateDescription' {} a -> s {status = a} :: CACertificateDescription)

-- | The CA certificate ARN.
cACertificateDescription_certificateArn :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_certificateArn = Lens.lens (\CACertificateDescription' {certificateArn} -> certificateArn) (\s@CACertificateDescription' {} a -> s {certificateArn = a} :: CACertificateDescription)

-- | The date the CA certificate was created.
cACertificateDescription_creationDate :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.UTCTime)
cACertificateDescription_creationDate = Lens.lens (\CACertificateDescription' {creationDate} -> creationDate) (\s@CACertificateDescription' {} a -> s {creationDate = a} :: CACertificateDescription) Prelude.. Lens.mapping Core._Time

-- | The owner of the CA certificate.
cACertificateDescription_ownedBy :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_ownedBy = Lens.lens (\CACertificateDescription' {ownedBy} -> ownedBy) (\s@CACertificateDescription' {} a -> s {ownedBy = a} :: CACertificateDescription)

-- | The customer version of the CA certificate.
cACertificateDescription_customerVersion :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Natural)
cACertificateDescription_customerVersion = Lens.lens (\CACertificateDescription' {customerVersion} -> customerVersion) (\s@CACertificateDescription' {} a -> s {customerVersion = a} :: CACertificateDescription)

-- | The generation ID of the CA certificate.
cACertificateDescription_generationId :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_generationId = Lens.lens (\CACertificateDescription' {generationId} -> generationId) (\s@CACertificateDescription' {} a -> s {generationId = a} :: CACertificateDescription)

-- | The CA certificate ID.
cACertificateDescription_certificateId :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_certificateId = Lens.lens (\CACertificateDescription' {certificateId} -> certificateId) (\s@CACertificateDescription' {} a -> s {certificateId = a} :: CACertificateDescription)

-- | The CA certificate data, in PEM format.
cACertificateDescription_certificatePem :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_certificatePem = Lens.lens (\CACertificateDescription' {certificatePem} -> certificatePem) (\s@CACertificateDescription' {} a -> s {certificatePem = a} :: CACertificateDescription)

-- | When the CA certificate is valid.
cACertificateDescription_validity :: Lens.Lens' CACertificateDescription (Prelude.Maybe CertificateValidity)
cACertificateDescription_validity = Lens.lens (\CACertificateDescription' {validity} -> validity) (\s@CACertificateDescription' {} a -> s {validity = a} :: CACertificateDescription)

-- | Whether the CA certificate configured for auto registration of device
-- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
cACertificateDescription_autoRegistrationStatus :: Lens.Lens' CACertificateDescription (Prelude.Maybe AutoRegistrationStatus)
cACertificateDescription_autoRegistrationStatus = Lens.lens (\CACertificateDescription' {autoRegistrationStatus} -> autoRegistrationStatus) (\s@CACertificateDescription' {} a -> s {autoRegistrationStatus = a} :: CACertificateDescription)

instance Core.FromJSON CACertificateDescription where
  parseJSON =
    Core.withObject
      "CACertificateDescription"
      ( \x ->
          CACertificateDescription'
            Prelude.<$> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "certificateArn")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "ownedBy")
            Prelude.<*> (x Core..:? "customerVersion")
            Prelude.<*> (x Core..:? "generationId")
            Prelude.<*> (x Core..:? "certificateId")
            Prelude.<*> (x Core..:? "certificatePem")
            Prelude.<*> (x Core..:? "validity")
            Prelude.<*> (x Core..:? "autoRegistrationStatus")
      )

instance Prelude.Hashable CACertificateDescription

instance Prelude.NFData CACertificateDescription
