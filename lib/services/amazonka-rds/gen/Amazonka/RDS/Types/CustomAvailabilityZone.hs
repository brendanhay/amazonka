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
-- Module      : Amazonka.RDS.Types.CustomAvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.CustomAvailabilityZone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.VpnDetails

-- | A custom Availability Zone (AZ) is an on-premises AZ that is integrated
-- with a VMware vSphere cluster.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
--
-- /See:/ 'newCustomAvailabilityZone' smart constructor.
data CustomAvailabilityZone = CustomAvailabilityZone'
  { -- | Information about the virtual private network (VPN) between the VMware
    -- vSphere cluster and the Amazon Web Services website.
    vpnDetails :: Prelude.Maybe VpnDetails,
    -- | The name of the custom AZ.
    customAvailabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the custom AZ.
    --
    -- Amazon RDS generates a unique identifier when a custom AZ is created.
    customAvailabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The status of the custom AZ.
    customAvailabilityZoneStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomAvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpnDetails', 'customAvailabilityZone_vpnDetails' - Information about the virtual private network (VPN) between the VMware
-- vSphere cluster and the Amazon Web Services website.
--
-- 'customAvailabilityZoneName', 'customAvailabilityZone_customAvailabilityZoneName' - The name of the custom AZ.
--
-- 'customAvailabilityZoneId', 'customAvailabilityZone_customAvailabilityZoneId' - The identifier of the custom AZ.
--
-- Amazon RDS generates a unique identifier when a custom AZ is created.
--
-- 'customAvailabilityZoneStatus', 'customAvailabilityZone_customAvailabilityZoneStatus' - The status of the custom AZ.
newCustomAvailabilityZone ::
  CustomAvailabilityZone
newCustomAvailabilityZone =
  CustomAvailabilityZone'
    { vpnDetails =
        Prelude.Nothing,
      customAvailabilityZoneName = Prelude.Nothing,
      customAvailabilityZoneId = Prelude.Nothing,
      customAvailabilityZoneStatus = Prelude.Nothing
    }

-- | Information about the virtual private network (VPN) between the VMware
-- vSphere cluster and the Amazon Web Services website.
customAvailabilityZone_vpnDetails :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe VpnDetails)
customAvailabilityZone_vpnDetails = Lens.lens (\CustomAvailabilityZone' {vpnDetails} -> vpnDetails) (\s@CustomAvailabilityZone' {} a -> s {vpnDetails = a} :: CustomAvailabilityZone)

-- | The name of the custom AZ.
customAvailabilityZone_customAvailabilityZoneName :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe Prelude.Text)
customAvailabilityZone_customAvailabilityZoneName = Lens.lens (\CustomAvailabilityZone' {customAvailabilityZoneName} -> customAvailabilityZoneName) (\s@CustomAvailabilityZone' {} a -> s {customAvailabilityZoneName = a} :: CustomAvailabilityZone)

-- | The identifier of the custom AZ.
--
-- Amazon RDS generates a unique identifier when a custom AZ is created.
customAvailabilityZone_customAvailabilityZoneId :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe Prelude.Text)
customAvailabilityZone_customAvailabilityZoneId = Lens.lens (\CustomAvailabilityZone' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@CustomAvailabilityZone' {} a -> s {customAvailabilityZoneId = a} :: CustomAvailabilityZone)

-- | The status of the custom AZ.
customAvailabilityZone_customAvailabilityZoneStatus :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe Prelude.Text)
customAvailabilityZone_customAvailabilityZoneStatus = Lens.lens (\CustomAvailabilityZone' {customAvailabilityZoneStatus} -> customAvailabilityZoneStatus) (\s@CustomAvailabilityZone' {} a -> s {customAvailabilityZoneStatus = a} :: CustomAvailabilityZone)

instance Core.FromXML CustomAvailabilityZone where
  parseXML x =
    CustomAvailabilityZone'
      Prelude.<$> (x Core..@? "VpnDetails")
      Prelude.<*> (x Core..@? "CustomAvailabilityZoneName")
      Prelude.<*> (x Core..@? "CustomAvailabilityZoneId")
      Prelude.<*> (x Core..@? "CustomAvailabilityZoneStatus")

instance Prelude.Hashable CustomAvailabilityZone where
  hashWithSalt _salt CustomAvailabilityZone' {..} =
    _salt `Prelude.hashWithSalt` vpnDetails
      `Prelude.hashWithSalt` customAvailabilityZoneName
      `Prelude.hashWithSalt` customAvailabilityZoneId
      `Prelude.hashWithSalt` customAvailabilityZoneStatus

instance Prelude.NFData CustomAvailabilityZone where
  rnf CustomAvailabilityZone' {..} =
    Prelude.rnf vpnDetails
      `Prelude.seq` Prelude.rnf customAvailabilityZoneName
      `Prelude.seq` Prelude.rnf customAvailabilityZoneId
      `Prelude.seq` Prelude.rnf customAvailabilityZoneStatus
