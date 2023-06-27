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
-- Module      : Amazonka.IoTWireless.Types.SidewalkListDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkListDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.CertificateList
import Amazonka.IoTWireless.Types.WirelessDeviceSidewalkStatus
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk object used by list functions.
--
-- /See:/ 'newSidewalkListDevice' smart constructor.
data SidewalkListDevice = SidewalkListDevice'
  { -- | The Sidewalk Amazon ID.
    amazonId :: Prelude.Maybe Prelude.Text,
    -- | The sidewalk device certificates for Ed25519 and P256r1.
    deviceCertificates :: Prelude.Maybe [CertificateList],
    -- | Sidewalk object used by list functions.
    deviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | The sidewalk device identification.
    sidewalkId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk manufacturing series number.
    sidewalkManufacturingSn :: Prelude.Maybe Prelude.Text,
    -- | The status of the Sidewalk devices, such as provisioned or registered.
    status :: Prelude.Maybe WirelessDeviceSidewalkStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkListDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amazonId', 'sidewalkListDevice_amazonId' - The Sidewalk Amazon ID.
--
-- 'deviceCertificates', 'sidewalkListDevice_deviceCertificates' - The sidewalk device certificates for Ed25519 and P256r1.
--
-- 'deviceProfileId', 'sidewalkListDevice_deviceProfileId' - Sidewalk object used by list functions.
--
-- 'sidewalkId', 'sidewalkListDevice_sidewalkId' - The sidewalk device identification.
--
-- 'sidewalkManufacturingSn', 'sidewalkListDevice_sidewalkManufacturingSn' - The Sidewalk manufacturing series number.
--
-- 'status', 'sidewalkListDevice_status' - The status of the Sidewalk devices, such as provisioned or registered.
newSidewalkListDevice ::
  SidewalkListDevice
newSidewalkListDevice =
  SidewalkListDevice'
    { amazonId = Prelude.Nothing,
      deviceCertificates = Prelude.Nothing,
      deviceProfileId = Prelude.Nothing,
      sidewalkId = Prelude.Nothing,
      sidewalkManufacturingSn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Sidewalk Amazon ID.
sidewalkListDevice_amazonId :: Lens.Lens' SidewalkListDevice (Prelude.Maybe Prelude.Text)
sidewalkListDevice_amazonId = Lens.lens (\SidewalkListDevice' {amazonId} -> amazonId) (\s@SidewalkListDevice' {} a -> s {amazonId = a} :: SidewalkListDevice)

-- | The sidewalk device certificates for Ed25519 and P256r1.
sidewalkListDevice_deviceCertificates :: Lens.Lens' SidewalkListDevice (Prelude.Maybe [CertificateList])
sidewalkListDevice_deviceCertificates = Lens.lens (\SidewalkListDevice' {deviceCertificates} -> deviceCertificates) (\s@SidewalkListDevice' {} a -> s {deviceCertificates = a} :: SidewalkListDevice) Prelude.. Lens.mapping Lens.coerced

-- | Sidewalk object used by list functions.
sidewalkListDevice_deviceProfileId :: Lens.Lens' SidewalkListDevice (Prelude.Maybe Prelude.Text)
sidewalkListDevice_deviceProfileId = Lens.lens (\SidewalkListDevice' {deviceProfileId} -> deviceProfileId) (\s@SidewalkListDevice' {} a -> s {deviceProfileId = a} :: SidewalkListDevice)

-- | The sidewalk device identification.
sidewalkListDevice_sidewalkId :: Lens.Lens' SidewalkListDevice (Prelude.Maybe Prelude.Text)
sidewalkListDevice_sidewalkId = Lens.lens (\SidewalkListDevice' {sidewalkId} -> sidewalkId) (\s@SidewalkListDevice' {} a -> s {sidewalkId = a} :: SidewalkListDevice)

-- | The Sidewalk manufacturing series number.
sidewalkListDevice_sidewalkManufacturingSn :: Lens.Lens' SidewalkListDevice (Prelude.Maybe Prelude.Text)
sidewalkListDevice_sidewalkManufacturingSn = Lens.lens (\SidewalkListDevice' {sidewalkManufacturingSn} -> sidewalkManufacturingSn) (\s@SidewalkListDevice' {} a -> s {sidewalkManufacturingSn = a} :: SidewalkListDevice)

-- | The status of the Sidewalk devices, such as provisioned or registered.
sidewalkListDevice_status :: Lens.Lens' SidewalkListDevice (Prelude.Maybe WirelessDeviceSidewalkStatus)
sidewalkListDevice_status = Lens.lens (\SidewalkListDevice' {status} -> status) (\s@SidewalkListDevice' {} a -> s {status = a} :: SidewalkListDevice)

instance Data.FromJSON SidewalkListDevice where
  parseJSON =
    Data.withObject
      "SidewalkListDevice"
      ( \x ->
          SidewalkListDevice'
            Prelude.<$> (x Data..:? "AmazonId")
            Prelude.<*> ( x
                            Data..:? "DeviceCertificates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DeviceProfileId")
            Prelude.<*> (x Data..:? "SidewalkId")
            Prelude.<*> (x Data..:? "SidewalkManufacturingSn")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable SidewalkListDevice where
  hashWithSalt _salt SidewalkListDevice' {..} =
    _salt
      `Prelude.hashWithSalt` amazonId
      `Prelude.hashWithSalt` deviceCertificates
      `Prelude.hashWithSalt` deviceProfileId
      `Prelude.hashWithSalt` sidewalkId
      `Prelude.hashWithSalt` sidewalkManufacturingSn
      `Prelude.hashWithSalt` status

instance Prelude.NFData SidewalkListDevice where
  rnf SidewalkListDevice' {..} =
    Prelude.rnf amazonId
      `Prelude.seq` Prelude.rnf deviceCertificates
      `Prelude.seq` Prelude.rnf deviceProfileId
      `Prelude.seq` Prelude.rnf sidewalkId
      `Prelude.seq` Prelude.rnf sidewalkManufacturingSn
      `Prelude.seq` Prelude.rnf status
