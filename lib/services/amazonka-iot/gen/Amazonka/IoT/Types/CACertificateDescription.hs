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
-- Module      : Amazonka.IoT.Types.CACertificateDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.CACertificateDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AutoRegistrationStatus
import Amazonka.IoT.Types.CACertificateStatus
import Amazonka.IoT.Types.CertificateMode
import Amazonka.IoT.Types.CertificateValidity
import qualified Amazonka.Prelude as Prelude

-- | Describes a CA certificate.
--
-- /See:/ 'newCACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
  { -- | The customer version of the CA certificate.
    customerVersion :: Prelude.Maybe Prelude.Natural,
    -- | The date the CA certificate was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The date the CA certificate was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The status of a CA certificate.
    status :: Prelude.Maybe CACertificateStatus,
    -- | The CA certificate ARN.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The CA certificate ID.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The CA certificate data, in PEM format.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | Whether the CA certificate configured for auto registration of device
    -- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
    autoRegistrationStatus :: Prelude.Maybe AutoRegistrationStatus,
    -- | The generation ID of the CA certificate.
    generationId :: Prelude.Maybe Prelude.Text,
    -- | The mode of the CA.
    --
    -- All the device certificates that are registered using this CA will be
    -- registered in the same mode as the CA. For more information about
    -- certificate mode for device certificates, see
    -- <https://docs.aws.amazon.com/iot/latest/apireference/API_CertificateDescription.html#iot-Type-CertificateDescription-certificateMode certificate mode>.
    certificateMode :: Prelude.Maybe CertificateMode,
    -- | The owner of the CA certificate.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | When the CA certificate is valid.
    validity :: Prelude.Maybe CertificateValidity
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
-- 'customerVersion', 'cACertificateDescription_customerVersion' - The customer version of the CA certificate.
--
-- 'lastModifiedDate', 'cACertificateDescription_lastModifiedDate' - The date the CA certificate was last modified.
--
-- 'creationDate', 'cACertificateDescription_creationDate' - The date the CA certificate was created.
--
-- 'status', 'cACertificateDescription_status' - The status of a CA certificate.
--
-- 'certificateArn', 'cACertificateDescription_certificateArn' - The CA certificate ARN.
--
-- 'certificateId', 'cACertificateDescription_certificateId' - The CA certificate ID.
--
-- 'certificatePem', 'cACertificateDescription_certificatePem' - The CA certificate data, in PEM format.
--
-- 'autoRegistrationStatus', 'cACertificateDescription_autoRegistrationStatus' - Whether the CA certificate configured for auto registration of device
-- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
--
-- 'generationId', 'cACertificateDescription_generationId' - The generation ID of the CA certificate.
--
-- 'certificateMode', 'cACertificateDescription_certificateMode' - The mode of the CA.
--
-- All the device certificates that are registered using this CA will be
-- registered in the same mode as the CA. For more information about
-- certificate mode for device certificates, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CertificateDescription.html#iot-Type-CertificateDescription-certificateMode certificate mode>.
--
-- 'ownedBy', 'cACertificateDescription_ownedBy' - The owner of the CA certificate.
--
-- 'validity', 'cACertificateDescription_validity' - When the CA certificate is valid.
newCACertificateDescription ::
  CACertificateDescription
newCACertificateDescription =
  CACertificateDescription'
    { customerVersion =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      status = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      autoRegistrationStatus = Prelude.Nothing,
      generationId = Prelude.Nothing,
      certificateMode = Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      validity = Prelude.Nothing
    }

-- | The customer version of the CA certificate.
cACertificateDescription_customerVersion :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Natural)
cACertificateDescription_customerVersion = Lens.lens (\CACertificateDescription' {customerVersion} -> customerVersion) (\s@CACertificateDescription' {} a -> s {customerVersion = a} :: CACertificateDescription)

-- | The date the CA certificate was last modified.
cACertificateDescription_lastModifiedDate :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.UTCTime)
cACertificateDescription_lastModifiedDate = Lens.lens (\CACertificateDescription' {lastModifiedDate} -> lastModifiedDate) (\s@CACertificateDescription' {} a -> s {lastModifiedDate = a} :: CACertificateDescription) Prelude.. Lens.mapping Data._Time

-- | The date the CA certificate was created.
cACertificateDescription_creationDate :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.UTCTime)
cACertificateDescription_creationDate = Lens.lens (\CACertificateDescription' {creationDate} -> creationDate) (\s@CACertificateDescription' {} a -> s {creationDate = a} :: CACertificateDescription) Prelude.. Lens.mapping Data._Time

-- | The status of a CA certificate.
cACertificateDescription_status :: Lens.Lens' CACertificateDescription (Prelude.Maybe CACertificateStatus)
cACertificateDescription_status = Lens.lens (\CACertificateDescription' {status} -> status) (\s@CACertificateDescription' {} a -> s {status = a} :: CACertificateDescription)

-- | The CA certificate ARN.
cACertificateDescription_certificateArn :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_certificateArn = Lens.lens (\CACertificateDescription' {certificateArn} -> certificateArn) (\s@CACertificateDescription' {} a -> s {certificateArn = a} :: CACertificateDescription)

-- | The CA certificate ID.
cACertificateDescription_certificateId :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_certificateId = Lens.lens (\CACertificateDescription' {certificateId} -> certificateId) (\s@CACertificateDescription' {} a -> s {certificateId = a} :: CACertificateDescription)

-- | The CA certificate data, in PEM format.
cACertificateDescription_certificatePem :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_certificatePem = Lens.lens (\CACertificateDescription' {certificatePem} -> certificatePem) (\s@CACertificateDescription' {} a -> s {certificatePem = a} :: CACertificateDescription)

-- | Whether the CA certificate configured for auto registration of device
-- certificates. Valid values are \"ENABLE\" and \"DISABLE\"
cACertificateDescription_autoRegistrationStatus :: Lens.Lens' CACertificateDescription (Prelude.Maybe AutoRegistrationStatus)
cACertificateDescription_autoRegistrationStatus = Lens.lens (\CACertificateDescription' {autoRegistrationStatus} -> autoRegistrationStatus) (\s@CACertificateDescription' {} a -> s {autoRegistrationStatus = a} :: CACertificateDescription)

-- | The generation ID of the CA certificate.
cACertificateDescription_generationId :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_generationId = Lens.lens (\CACertificateDescription' {generationId} -> generationId) (\s@CACertificateDescription' {} a -> s {generationId = a} :: CACertificateDescription)

-- | The mode of the CA.
--
-- All the device certificates that are registered using this CA will be
-- registered in the same mode as the CA. For more information about
-- certificate mode for device certificates, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CertificateDescription.html#iot-Type-CertificateDescription-certificateMode certificate mode>.
cACertificateDescription_certificateMode :: Lens.Lens' CACertificateDescription (Prelude.Maybe CertificateMode)
cACertificateDescription_certificateMode = Lens.lens (\CACertificateDescription' {certificateMode} -> certificateMode) (\s@CACertificateDescription' {} a -> s {certificateMode = a} :: CACertificateDescription)

-- | The owner of the CA certificate.
cACertificateDescription_ownedBy :: Lens.Lens' CACertificateDescription (Prelude.Maybe Prelude.Text)
cACertificateDescription_ownedBy = Lens.lens (\CACertificateDescription' {ownedBy} -> ownedBy) (\s@CACertificateDescription' {} a -> s {ownedBy = a} :: CACertificateDescription)

-- | When the CA certificate is valid.
cACertificateDescription_validity :: Lens.Lens' CACertificateDescription (Prelude.Maybe CertificateValidity)
cACertificateDescription_validity = Lens.lens (\CACertificateDescription' {validity} -> validity) (\s@CACertificateDescription' {} a -> s {validity = a} :: CACertificateDescription)

instance Data.FromJSON CACertificateDescription where
  parseJSON =
    Data.withObject
      "CACertificateDescription"
      ( \x ->
          CACertificateDescription'
            Prelude.<$> (x Data..:? "customerVersion")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "certificateArn")
            Prelude.<*> (x Data..:? "certificateId")
            Prelude.<*> (x Data..:? "certificatePem")
            Prelude.<*> (x Data..:? "autoRegistrationStatus")
            Prelude.<*> (x Data..:? "generationId")
            Prelude.<*> (x Data..:? "certificateMode")
            Prelude.<*> (x Data..:? "ownedBy")
            Prelude.<*> (x Data..:? "validity")
      )

instance Prelude.Hashable CACertificateDescription where
  hashWithSalt _salt CACertificateDescription' {..} =
    _salt `Prelude.hashWithSalt` customerVersion
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` certificatePem
      `Prelude.hashWithSalt` autoRegistrationStatus
      `Prelude.hashWithSalt` generationId
      `Prelude.hashWithSalt` certificateMode
      `Prelude.hashWithSalt` ownedBy
      `Prelude.hashWithSalt` validity

instance Prelude.NFData CACertificateDescription where
  rnf CACertificateDescription' {..} =
    Prelude.rnf customerVersion
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf certificatePem
      `Prelude.seq` Prelude.rnf autoRegistrationStatus
      `Prelude.seq` Prelude.rnf generationId
      `Prelude.seq` Prelude.rnf certificateMode
      `Prelude.seq` Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf validity
