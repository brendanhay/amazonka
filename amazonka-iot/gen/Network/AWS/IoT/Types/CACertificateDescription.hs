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

-- | Describes a CA certificate.
--
-- /See:/ 'newCACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
  { -- | The date the CA certificate was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The status of a CA certificate.
    status :: Core.Maybe CACertificateStatus,
    -- | The CA certificate ARN.
    certificateArn :: Core.Maybe Core.Text,
    -- | The date the CA certificate was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The owner of the CA certificate.
    ownedBy :: Core.Maybe Core.Text,
    -- | The customer version of the CA certificate.
    customerVersion :: Core.Maybe Core.Natural,
    -- | The generation ID of the CA certificate.
    generationId :: Core.Maybe Core.Text,
    -- | The CA certificate ID.
    certificateId :: Core.Maybe Core.Text,
    -- | The CA certificate data, in PEM format.
    certificatePem :: Core.Maybe Core.Text,
    -- | When the CA certificate is valid.
    validity :: Core.Maybe CertificateValidity,
    -- | Whether the CA certificate configured for auto registration of device
    -- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
    autoRegistrationStatus :: Core.Maybe AutoRegistrationStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      status = Core.Nothing,
      certificateArn = Core.Nothing,
      creationDate = Core.Nothing,
      ownedBy = Core.Nothing,
      customerVersion = Core.Nothing,
      generationId = Core.Nothing,
      certificateId = Core.Nothing,
      certificatePem = Core.Nothing,
      validity = Core.Nothing,
      autoRegistrationStatus = Core.Nothing
    }

-- | The date the CA certificate was last modified.
cACertificateDescription_lastModifiedDate :: Lens.Lens' CACertificateDescription (Core.Maybe Core.UTCTime)
cACertificateDescription_lastModifiedDate = Lens.lens (\CACertificateDescription' {lastModifiedDate} -> lastModifiedDate) (\s@CACertificateDescription' {} a -> s {lastModifiedDate = a} :: CACertificateDescription) Core.. Lens.mapping Core._Time

-- | The status of a CA certificate.
cACertificateDescription_status :: Lens.Lens' CACertificateDescription (Core.Maybe CACertificateStatus)
cACertificateDescription_status = Lens.lens (\CACertificateDescription' {status} -> status) (\s@CACertificateDescription' {} a -> s {status = a} :: CACertificateDescription)

-- | The CA certificate ARN.
cACertificateDescription_certificateArn :: Lens.Lens' CACertificateDescription (Core.Maybe Core.Text)
cACertificateDescription_certificateArn = Lens.lens (\CACertificateDescription' {certificateArn} -> certificateArn) (\s@CACertificateDescription' {} a -> s {certificateArn = a} :: CACertificateDescription)

-- | The date the CA certificate was created.
cACertificateDescription_creationDate :: Lens.Lens' CACertificateDescription (Core.Maybe Core.UTCTime)
cACertificateDescription_creationDate = Lens.lens (\CACertificateDescription' {creationDate} -> creationDate) (\s@CACertificateDescription' {} a -> s {creationDate = a} :: CACertificateDescription) Core.. Lens.mapping Core._Time

-- | The owner of the CA certificate.
cACertificateDescription_ownedBy :: Lens.Lens' CACertificateDescription (Core.Maybe Core.Text)
cACertificateDescription_ownedBy = Lens.lens (\CACertificateDescription' {ownedBy} -> ownedBy) (\s@CACertificateDescription' {} a -> s {ownedBy = a} :: CACertificateDescription)

-- | The customer version of the CA certificate.
cACertificateDescription_customerVersion :: Lens.Lens' CACertificateDescription (Core.Maybe Core.Natural)
cACertificateDescription_customerVersion = Lens.lens (\CACertificateDescription' {customerVersion} -> customerVersion) (\s@CACertificateDescription' {} a -> s {customerVersion = a} :: CACertificateDescription)

-- | The generation ID of the CA certificate.
cACertificateDescription_generationId :: Lens.Lens' CACertificateDescription (Core.Maybe Core.Text)
cACertificateDescription_generationId = Lens.lens (\CACertificateDescription' {generationId} -> generationId) (\s@CACertificateDescription' {} a -> s {generationId = a} :: CACertificateDescription)

-- | The CA certificate ID.
cACertificateDescription_certificateId :: Lens.Lens' CACertificateDescription (Core.Maybe Core.Text)
cACertificateDescription_certificateId = Lens.lens (\CACertificateDescription' {certificateId} -> certificateId) (\s@CACertificateDescription' {} a -> s {certificateId = a} :: CACertificateDescription)

-- | The CA certificate data, in PEM format.
cACertificateDescription_certificatePem :: Lens.Lens' CACertificateDescription (Core.Maybe Core.Text)
cACertificateDescription_certificatePem = Lens.lens (\CACertificateDescription' {certificatePem} -> certificatePem) (\s@CACertificateDescription' {} a -> s {certificatePem = a} :: CACertificateDescription)

-- | When the CA certificate is valid.
cACertificateDescription_validity :: Lens.Lens' CACertificateDescription (Core.Maybe CertificateValidity)
cACertificateDescription_validity = Lens.lens (\CACertificateDescription' {validity} -> validity) (\s@CACertificateDescription' {} a -> s {validity = a} :: CACertificateDescription)

-- | Whether the CA certificate configured for auto registration of device
-- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
cACertificateDescription_autoRegistrationStatus :: Lens.Lens' CACertificateDescription (Core.Maybe AutoRegistrationStatus)
cACertificateDescription_autoRegistrationStatus = Lens.lens (\CACertificateDescription' {autoRegistrationStatus} -> autoRegistrationStatus) (\s@CACertificateDescription' {} a -> s {autoRegistrationStatus = a} :: CACertificateDescription)

instance Core.FromJSON CACertificateDescription where
  parseJSON =
    Core.withObject
      "CACertificateDescription"
      ( \x ->
          CACertificateDescription'
            Core.<$> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "certificateArn")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "ownedBy")
            Core.<*> (x Core..:? "customerVersion")
            Core.<*> (x Core..:? "generationId")
            Core.<*> (x Core..:? "certificateId")
            Core.<*> (x Core..:? "certificatePem")
            Core.<*> (x Core..:? "validity")
            Core.<*> (x Core..:? "autoRegistrationStatus")
      )

instance Core.Hashable CACertificateDescription

instance Core.NFData CACertificateDescription
