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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkListDevice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types.CertificateList
import qualified Amazonka.Prelude as Prelude

-- | Sidewalk object used by list functions.
--
-- /See:/ 'newSidewalkListDevice' smart constructor.
data SidewalkListDevice = SidewalkListDevice'
  { -- | The sidewalk device certificates for Ed25519 and P256r1.
    deviceCertificates :: Prelude.Maybe [CertificateList],
    -- | The sidewalk device identification.
    sidewalkId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk Amazon ID.
    amazonId :: Prelude.Maybe Prelude.Text,
    -- | The Sidewalk manufacturing series number.
    sidewalkManufacturingSn :: Prelude.Maybe Prelude.Text
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
-- 'deviceCertificates', 'sidewalkListDevice_deviceCertificates' - The sidewalk device certificates for Ed25519 and P256r1.
--
-- 'sidewalkId', 'sidewalkListDevice_sidewalkId' - The sidewalk device identification.
--
-- 'amazonId', 'sidewalkListDevice_amazonId' - The Sidewalk Amazon ID.
--
-- 'sidewalkManufacturingSn', 'sidewalkListDevice_sidewalkManufacturingSn' - The Sidewalk manufacturing series number.
newSidewalkListDevice ::
  SidewalkListDevice
newSidewalkListDevice =
  SidewalkListDevice'
    { deviceCertificates =
        Prelude.Nothing,
      sidewalkId = Prelude.Nothing,
      amazonId = Prelude.Nothing,
      sidewalkManufacturingSn = Prelude.Nothing
    }

-- | The sidewalk device certificates for Ed25519 and P256r1.
sidewalkListDevice_deviceCertificates :: Lens.Lens' SidewalkListDevice (Prelude.Maybe [CertificateList])
sidewalkListDevice_deviceCertificates = Lens.lens (\SidewalkListDevice' {deviceCertificates} -> deviceCertificates) (\s@SidewalkListDevice' {} a -> s {deviceCertificates = a} :: SidewalkListDevice) Prelude.. Lens.mapping Lens.coerced

-- | The sidewalk device identification.
sidewalkListDevice_sidewalkId :: Lens.Lens' SidewalkListDevice (Prelude.Maybe Prelude.Text)
sidewalkListDevice_sidewalkId = Lens.lens (\SidewalkListDevice' {sidewalkId} -> sidewalkId) (\s@SidewalkListDevice' {} a -> s {sidewalkId = a} :: SidewalkListDevice)

-- | The Sidewalk Amazon ID.
sidewalkListDevice_amazonId :: Lens.Lens' SidewalkListDevice (Prelude.Maybe Prelude.Text)
sidewalkListDevice_amazonId = Lens.lens (\SidewalkListDevice' {amazonId} -> amazonId) (\s@SidewalkListDevice' {} a -> s {amazonId = a} :: SidewalkListDevice)

-- | The Sidewalk manufacturing series number.
sidewalkListDevice_sidewalkManufacturingSn :: Lens.Lens' SidewalkListDevice (Prelude.Maybe Prelude.Text)
sidewalkListDevice_sidewalkManufacturingSn = Lens.lens (\SidewalkListDevice' {sidewalkManufacturingSn} -> sidewalkManufacturingSn) (\s@SidewalkListDevice' {} a -> s {sidewalkManufacturingSn = a} :: SidewalkListDevice)

instance Core.FromJSON SidewalkListDevice where
  parseJSON =
    Core.withObject
      "SidewalkListDevice"
      ( \x ->
          SidewalkListDevice'
            Prelude.<$> ( x Core..:? "DeviceCertificates"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "SidewalkId")
            Prelude.<*> (x Core..:? "AmazonId")
            Prelude.<*> (x Core..:? "SidewalkManufacturingSn")
      )

instance Prelude.Hashable SidewalkListDevice where
  hashWithSalt _salt SidewalkListDevice' {..} =
    _salt `Prelude.hashWithSalt` deviceCertificates
      `Prelude.hashWithSalt` sidewalkId
      `Prelude.hashWithSalt` amazonId
      `Prelude.hashWithSalt` sidewalkManufacturingSn

instance Prelude.NFData SidewalkListDevice where
  rnf SidewalkListDevice' {..} =
    Prelude.rnf deviceCertificates
      `Prelude.seq` Prelude.rnf sidewalkId
      `Prelude.seq` Prelude.rnf amazonId
      `Prelude.seq` Prelude.rnf sidewalkManufacturingSn
