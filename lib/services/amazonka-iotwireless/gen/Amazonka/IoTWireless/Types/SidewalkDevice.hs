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
-- Module      : Amazonka.IoTWireless.Types.SidewalkDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.CertificateList
import Amazonka.IoTWireless.Types.WirelessDeviceSidewalkStatus
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk device object.
--
-- /See:/ 'newSidewalkDevice' smart constructor.
data SidewalkDevice = SidewalkDevice'
  { amazonId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Sidewalk device profile.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The sidewalk device certificates for Ed25519 and P256r1.
    deviceCertificates :: Prelude.Maybe [CertificateList],
    -- | The ID of the Sidewalk device profile.
    deviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk device private keys that will be used for onboarding the
    -- device.
    privateKeys :: Prelude.Maybe [CertificateList],
    -- | The sidewalk device identification.
    sidewalkId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk manufacturing series number.
    sidewalkManufacturingSn :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk device status, such as provisioned or registered.
    status :: Prelude.Maybe WirelessDeviceSidewalkStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonId', 'sidewalkDevice_amazonId' - Undocumented member.
--
-- 'certificateId', 'sidewalkDevice_certificateId' - The ID of the Sidewalk device profile.
--
-- 'deviceCertificates', 'sidewalkDevice_deviceCertificates' - The sidewalk device certificates for Ed25519 and P256r1.
--
-- 'deviceProfileId', 'sidewalkDevice_deviceProfileId' - The ID of the Sidewalk device profile.
--
-- 'privateKeys', 'sidewalkDevice_privateKeys' - The Sidewalk device private keys that will be used for onboarding the
-- device.
--
-- 'sidewalkId', 'sidewalkDevice_sidewalkId' - The sidewalk device identification.
--
-- 'sidewalkManufacturingSn', 'sidewalkDevice_sidewalkManufacturingSn' - The Sidewalk manufacturing series number.
--
-- 'status', 'sidewalkDevice_status' - The Sidewalk device status, such as provisioned or registered.
newSidewalkDevice ::
  SidewalkDevice
newSidewalkDevice =
  SidewalkDevice'
    { amazonId = Prelude.Nothing,
      certificateId = Prelude.Nothing,
      deviceCertificates = Prelude.Nothing,
      deviceProfileId = Prelude.Nothing,
      privateKeys = Prelude.Nothing,
      sidewalkId = Prelude.Nothing,
      sidewalkManufacturingSn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Undocumented member.
sidewalkDevice_amazonId :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_amazonId = Lens.lens (\SidewalkDevice' {amazonId} -> amazonId) (\s@SidewalkDevice' {} a -> s {amazonId = a} :: SidewalkDevice)

-- | The ID of the Sidewalk device profile.
sidewalkDevice_certificateId :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_certificateId = Lens.lens (\SidewalkDevice' {certificateId} -> certificateId) (\s@SidewalkDevice' {} a -> s {certificateId = a} :: SidewalkDevice)

-- | The sidewalk device certificates for Ed25519 and P256r1.
sidewalkDevice_deviceCertificates :: Lens.Lens' SidewalkDevice (Prelude.Maybe [CertificateList])
sidewalkDevice_deviceCertificates = Lens.lens (\SidewalkDevice' {deviceCertificates} -> deviceCertificates) (\s@SidewalkDevice' {} a -> s {deviceCertificates = a} :: SidewalkDevice) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Sidewalk device profile.
sidewalkDevice_deviceProfileId :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_deviceProfileId = Lens.lens (\SidewalkDevice' {deviceProfileId} -> deviceProfileId) (\s@SidewalkDevice' {} a -> s {deviceProfileId = a} :: SidewalkDevice)

-- | The Sidewalk device private keys that will be used for onboarding the
-- device.
sidewalkDevice_privateKeys :: Lens.Lens' SidewalkDevice (Prelude.Maybe [CertificateList])
sidewalkDevice_privateKeys = Lens.lens (\SidewalkDevice' {privateKeys} -> privateKeys) (\s@SidewalkDevice' {} a -> s {privateKeys = a} :: SidewalkDevice) Prelude.. Lens.mapping Lens.coerced

-- | The sidewalk device identification.
sidewalkDevice_sidewalkId :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_sidewalkId = Lens.lens (\SidewalkDevice' {sidewalkId} -> sidewalkId) (\s@SidewalkDevice' {} a -> s {sidewalkId = a} :: SidewalkDevice)

-- | The Sidewalk manufacturing series number.
sidewalkDevice_sidewalkManufacturingSn :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_sidewalkManufacturingSn = Lens.lens (\SidewalkDevice' {sidewalkManufacturingSn} -> sidewalkManufacturingSn) (\s@SidewalkDevice' {} a -> s {sidewalkManufacturingSn = a} :: SidewalkDevice)

-- | The Sidewalk device status, such as provisioned or registered.
sidewalkDevice_status :: Lens.Lens' SidewalkDevice (Prelude.Maybe WirelessDeviceSidewalkStatus)
sidewalkDevice_status = Lens.lens (\SidewalkDevice' {status} -> status) (\s@SidewalkDevice' {} a -> s {status = a} :: SidewalkDevice)

instance Data.FromJSON SidewalkDevice where
  parseJSON =
    Data.withObject
      "SidewalkDevice"
      ( \x ->
          SidewalkDevice'
            Prelude.<$> (x Data..:? "AmazonId")
            Prelude.<*> (x Data..:? "CertificateId")
            Prelude.<*> ( x
                            Data..:? "DeviceCertificates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DeviceProfileId")
            Prelude.<*> (x Data..:? "PrivateKeys" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SidewalkId")
            Prelude.<*> (x Data..:? "SidewalkManufacturingSn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable SidewalkDevice where
  hashWithSalt _salt SidewalkDevice' {..} =
    _salt
      `Prelude.hashWithSalt` amazonId
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` deviceCertificates
      `Prelude.hashWithSalt` deviceProfileId
      `Prelude.hashWithSalt` privateKeys
      `Prelude.hashWithSalt` sidewalkId
      `Prelude.hashWithSalt` sidewalkManufacturingSn
      `Prelude.hashWithSalt` status

instance Prelude.NFData SidewalkDevice where
  rnf SidewalkDevice' {..} =
    Prelude.rnf amazonId
      `Prelude.seq` Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf deviceCertificates
      `Prelude.seq` Prelude.rnf deviceProfileId
      `Prelude.seq` Prelude.rnf privateKeys
      `Prelude.seq` Prelude.rnf sidewalkId
      `Prelude.seq` Prelude.rnf sidewalkManufacturingSn
      `Prelude.seq` Prelude.rnf status
