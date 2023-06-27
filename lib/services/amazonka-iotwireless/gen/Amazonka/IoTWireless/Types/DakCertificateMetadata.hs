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
-- Module      : Amazonka.IoTWireless.Types.DakCertificateMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.DakCertificateMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The device attestation key (DAK) information.
--
-- /See:/ 'newDakCertificateMetadata' smart constructor.
data DakCertificateMetadata = DakCertificateMetadata'
  { -- | The advertised product ID (APID) that\'s used for pre-production and
    -- production applications.
    apId :: Prelude.Maybe Prelude.Text,
    -- | The device type ID that\'s used for prototyping applications.
    deviceTypeId :: Prelude.Maybe Prelude.Text,
    -- | Whether factory support has been enabled.
    factorySupport :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of signatures that the DAK can sign. A value of @-1@
    -- indicates that there\'s no device limit.
    maxAllowedSignature :: Prelude.Maybe Prelude.Int,
    -- | The certificate ID for the DAK.
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DakCertificateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apId', 'dakCertificateMetadata_apId' - The advertised product ID (APID) that\'s used for pre-production and
-- production applications.
--
-- 'deviceTypeId', 'dakCertificateMetadata_deviceTypeId' - The device type ID that\'s used for prototyping applications.
--
-- 'factorySupport', 'dakCertificateMetadata_factorySupport' - Whether factory support has been enabled.
--
-- 'maxAllowedSignature', 'dakCertificateMetadata_maxAllowedSignature' - The maximum number of signatures that the DAK can sign. A value of @-1@
-- indicates that there\'s no device limit.
--
-- 'certificateId', 'dakCertificateMetadata_certificateId' - The certificate ID for the DAK.
newDakCertificateMetadata ::
  -- | 'certificateId'
  Prelude.Text ->
  DakCertificateMetadata
newDakCertificateMetadata pCertificateId_ =
  DakCertificateMetadata'
    { apId = Prelude.Nothing,
      deviceTypeId = Prelude.Nothing,
      factorySupport = Prelude.Nothing,
      maxAllowedSignature = Prelude.Nothing,
      certificateId = pCertificateId_
    }

-- | The advertised product ID (APID) that\'s used for pre-production and
-- production applications.
dakCertificateMetadata_apId :: Lens.Lens' DakCertificateMetadata (Prelude.Maybe Prelude.Text)
dakCertificateMetadata_apId = Lens.lens (\DakCertificateMetadata' {apId} -> apId) (\s@DakCertificateMetadata' {} a -> s {apId = a} :: DakCertificateMetadata)

-- | The device type ID that\'s used for prototyping applications.
dakCertificateMetadata_deviceTypeId :: Lens.Lens' DakCertificateMetadata (Prelude.Maybe Prelude.Text)
dakCertificateMetadata_deviceTypeId = Lens.lens (\DakCertificateMetadata' {deviceTypeId} -> deviceTypeId) (\s@DakCertificateMetadata' {} a -> s {deviceTypeId = a} :: DakCertificateMetadata)

-- | Whether factory support has been enabled.
dakCertificateMetadata_factorySupport :: Lens.Lens' DakCertificateMetadata (Prelude.Maybe Prelude.Bool)
dakCertificateMetadata_factorySupport = Lens.lens (\DakCertificateMetadata' {factorySupport} -> factorySupport) (\s@DakCertificateMetadata' {} a -> s {factorySupport = a} :: DakCertificateMetadata)

-- | The maximum number of signatures that the DAK can sign. A value of @-1@
-- indicates that there\'s no device limit.
dakCertificateMetadata_maxAllowedSignature :: Lens.Lens' DakCertificateMetadata (Prelude.Maybe Prelude.Int)
dakCertificateMetadata_maxAllowedSignature = Lens.lens (\DakCertificateMetadata' {maxAllowedSignature} -> maxAllowedSignature) (\s@DakCertificateMetadata' {} a -> s {maxAllowedSignature = a} :: DakCertificateMetadata)

-- | The certificate ID for the DAK.
dakCertificateMetadata_certificateId :: Lens.Lens' DakCertificateMetadata Prelude.Text
dakCertificateMetadata_certificateId = Lens.lens (\DakCertificateMetadata' {certificateId} -> certificateId) (\s@DakCertificateMetadata' {} a -> s {certificateId = a} :: DakCertificateMetadata)

instance Data.FromJSON DakCertificateMetadata where
  parseJSON =
    Data.withObject
      "DakCertificateMetadata"
      ( \x ->
          DakCertificateMetadata'
            Prelude.<$> (x Data..:? "ApId")
            Prelude.<*> (x Data..:? "DeviceTypeId")
            Prelude.<*> (x Data..:? "FactorySupport")
            Prelude.<*> (x Data..:? "MaxAllowedSignature")
            Prelude.<*> (x Data..: "CertificateId")
      )

instance Prelude.Hashable DakCertificateMetadata where
  hashWithSalt _salt DakCertificateMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` apId
      `Prelude.hashWithSalt` deviceTypeId
      `Prelude.hashWithSalt` factorySupport
      `Prelude.hashWithSalt` maxAllowedSignature
      `Prelude.hashWithSalt` certificateId

instance Prelude.NFData DakCertificateMetadata where
  rnf DakCertificateMetadata' {..} =
    Prelude.rnf apId
      `Prelude.seq` Prelude.rnf deviceTypeId
      `Prelude.seq` Prelude.rnf factorySupport
      `Prelude.seq` Prelude.rnf maxAllowedSignature
      `Prelude.seq` Prelude.rnf certificateId
