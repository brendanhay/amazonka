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
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.CertificateList
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk device object.
--
-- /See:/ 'newSidewalkDevice' smart constructor.
data SidewalkDevice = SidewalkDevice'
  { amazonId :: Prelude.Maybe Prelude.Text,
    -- | The sidewalk device certificates for Ed25519 and P256r1.
    deviceCertificates :: Prelude.Maybe [CertificateList],
    -- | The sidewalk device identification.
    sidewalkId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk manufacturing series number.
    sidewalkManufacturingSn :: Prelude.Maybe Prelude.Text
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
-- 'deviceCertificates', 'sidewalkDevice_deviceCertificates' - The sidewalk device certificates for Ed25519 and P256r1.
--
-- 'sidewalkId', 'sidewalkDevice_sidewalkId' - The sidewalk device identification.
--
-- 'sidewalkManufacturingSn', 'sidewalkDevice_sidewalkManufacturingSn' - The Sidewalk manufacturing series number.
newSidewalkDevice ::
  SidewalkDevice
newSidewalkDevice =
  SidewalkDevice'
    { amazonId = Prelude.Nothing,
      deviceCertificates = Prelude.Nothing,
      sidewalkId = Prelude.Nothing,
      sidewalkManufacturingSn = Prelude.Nothing
    }

-- | Undocumented member.
sidewalkDevice_amazonId :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_amazonId = Lens.lens (\SidewalkDevice' {amazonId} -> amazonId) (\s@SidewalkDevice' {} a -> s {amazonId = a} :: SidewalkDevice)

-- | The sidewalk device certificates for Ed25519 and P256r1.
sidewalkDevice_deviceCertificates :: Lens.Lens' SidewalkDevice (Prelude.Maybe [CertificateList])
sidewalkDevice_deviceCertificates = Lens.lens (\SidewalkDevice' {deviceCertificates} -> deviceCertificates) (\s@SidewalkDevice' {} a -> s {deviceCertificates = a} :: SidewalkDevice) Prelude.. Lens.mapping Lens.coerced

-- | The sidewalk device identification.
sidewalkDevice_sidewalkId :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_sidewalkId = Lens.lens (\SidewalkDevice' {sidewalkId} -> sidewalkId) (\s@SidewalkDevice' {} a -> s {sidewalkId = a} :: SidewalkDevice)

-- | The Sidewalk manufacturing series number.
sidewalkDevice_sidewalkManufacturingSn :: Lens.Lens' SidewalkDevice (Prelude.Maybe Prelude.Text)
sidewalkDevice_sidewalkManufacturingSn = Lens.lens (\SidewalkDevice' {sidewalkManufacturingSn} -> sidewalkManufacturingSn) (\s@SidewalkDevice' {} a -> s {sidewalkManufacturingSn = a} :: SidewalkDevice)

instance Data.FromJSON SidewalkDevice where
  parseJSON =
    Data.withObject
      "SidewalkDevice"
      ( \x ->
          SidewalkDevice'
            Prelude.<$> (x Data..:? "AmazonId")
            Prelude.<*> ( x
                            Data..:? "DeviceCertificates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SidewalkId")
            Prelude.<*> (x Data..:? "SidewalkManufacturingSn")
      )

instance Prelude.Hashable SidewalkDevice where
  hashWithSalt _salt SidewalkDevice' {..} =
    _salt
      `Prelude.hashWithSalt` amazonId
      `Prelude.hashWithSalt` deviceCertificates
      `Prelude.hashWithSalt` sidewalkId
      `Prelude.hashWithSalt` sidewalkManufacturingSn

instance Prelude.NFData SidewalkDevice where
  rnf SidewalkDevice' {..} =
    Prelude.rnf amazonId
      `Prelude.seq` Prelude.rnf deviceCertificates
      `Prelude.seq` Prelude.rnf sidewalkId
      `Prelude.seq` Prelude.rnf sidewalkManufacturingSn
