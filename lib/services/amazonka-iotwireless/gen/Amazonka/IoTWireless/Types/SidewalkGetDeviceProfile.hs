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
-- Module      : Amazonka.IoTWireless.Types.SidewalkGetDeviceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.SidewalkGetDeviceProfile where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types.DakCertificateMetadata
import qualified Amazonka.Prelude as Prelude

-- | Gets information about a Sidewalk device profile.
--
-- /See:/ 'newSidewalkGetDeviceProfile' smart constructor.
data SidewalkGetDeviceProfile = SidewalkGetDeviceProfile'
  { -- | The Sidewalk application server public key.
    applicationServerPublicKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The DAK certificate information of the Sidewalk device profile.
    dakCertificateMetadata :: Prelude.Maybe [DakCertificateMetadata],
    -- | Gets information about the certification status of a Sidewalk device
    -- profile.
    qualificationStatus :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SidewalkGetDeviceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationServerPublicKey', 'sidewalkGetDeviceProfile_applicationServerPublicKey' - The Sidewalk application server public key.
--
-- 'dakCertificateMetadata', 'sidewalkGetDeviceProfile_dakCertificateMetadata' - The DAK certificate information of the Sidewalk device profile.
--
-- 'qualificationStatus', 'sidewalkGetDeviceProfile_qualificationStatus' - Gets information about the certification status of a Sidewalk device
-- profile.
newSidewalkGetDeviceProfile ::
  SidewalkGetDeviceProfile
newSidewalkGetDeviceProfile =
  SidewalkGetDeviceProfile'
    { applicationServerPublicKey =
        Prelude.Nothing,
      dakCertificateMetadata = Prelude.Nothing,
      qualificationStatus = Prelude.Nothing
    }

-- | The Sidewalk application server public key.
sidewalkGetDeviceProfile_applicationServerPublicKey :: Lens.Lens' SidewalkGetDeviceProfile (Prelude.Maybe Prelude.Text)
sidewalkGetDeviceProfile_applicationServerPublicKey = Lens.lens (\SidewalkGetDeviceProfile' {applicationServerPublicKey} -> applicationServerPublicKey) (\s@SidewalkGetDeviceProfile' {} a -> s {applicationServerPublicKey = a} :: SidewalkGetDeviceProfile) Prelude.. Lens.mapping Data._Sensitive

-- | The DAK certificate information of the Sidewalk device profile.
sidewalkGetDeviceProfile_dakCertificateMetadata :: Lens.Lens' SidewalkGetDeviceProfile (Prelude.Maybe [DakCertificateMetadata])
sidewalkGetDeviceProfile_dakCertificateMetadata = Lens.lens (\SidewalkGetDeviceProfile' {dakCertificateMetadata} -> dakCertificateMetadata) (\s@SidewalkGetDeviceProfile' {} a -> s {dakCertificateMetadata = a} :: SidewalkGetDeviceProfile) Prelude.. Lens.mapping Lens.coerced

-- | Gets information about the certification status of a Sidewalk device
-- profile.
sidewalkGetDeviceProfile_qualificationStatus :: Lens.Lens' SidewalkGetDeviceProfile (Prelude.Maybe Prelude.Bool)
sidewalkGetDeviceProfile_qualificationStatus = Lens.lens (\SidewalkGetDeviceProfile' {qualificationStatus} -> qualificationStatus) (\s@SidewalkGetDeviceProfile' {} a -> s {qualificationStatus = a} :: SidewalkGetDeviceProfile)

instance Data.FromJSON SidewalkGetDeviceProfile where
  parseJSON =
    Data.withObject
      "SidewalkGetDeviceProfile"
      ( \x ->
          SidewalkGetDeviceProfile'
            Prelude.<$> (x Data..:? "ApplicationServerPublicKey")
            Prelude.<*> ( x
                            Data..:? "DakCertificateMetadata"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "QualificationStatus")
      )

instance Prelude.Hashable SidewalkGetDeviceProfile where
  hashWithSalt _salt SidewalkGetDeviceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` applicationServerPublicKey
      `Prelude.hashWithSalt` dakCertificateMetadata
      `Prelude.hashWithSalt` qualificationStatus

instance Prelude.NFData SidewalkGetDeviceProfile where
  rnf SidewalkGetDeviceProfile' {..} =
    Prelude.rnf applicationServerPublicKey
      `Prelude.seq` Prelude.rnf dakCertificateMetadata
      `Prelude.seq` Prelude.rnf qualificationStatus
