{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.RDS.Types.CustomAvailabilityZone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.CustomAvailabilityZone where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.VpnDetails

-- | A custom Availability Zone (AZ) is an on-premises AZ that is integrated
-- with a VMware vSphere cluster.
--
-- For more information about RDS on VMware, see the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/RDSonVMwareUserGuide/rds-on-vmware.html RDS on VMware User Guide.>
--
-- /See:/ 'newCustomAvailabilityZone' smart constructor.
data CustomAvailabilityZone = CustomAvailabilityZone'
  { -- | The identifier of the custom AZ.
    --
    -- Amazon RDS generates a unique identifier when a custom AZ is created.
    customAvailabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom AZ.
    customAvailabilityZoneName :: Prelude.Maybe Prelude.Text,
    -- | Information about the virtual private network (VPN) between the VMware
    -- vSphere cluster and the AWS website.
    vpnDetails :: Prelude.Maybe VpnDetails,
    -- | The status of the custom AZ.
    customAvailabilityZoneStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomAvailabilityZone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customAvailabilityZoneId', 'customAvailabilityZone_customAvailabilityZoneId' - The identifier of the custom AZ.
--
-- Amazon RDS generates a unique identifier when a custom AZ is created.
--
-- 'customAvailabilityZoneName', 'customAvailabilityZone_customAvailabilityZoneName' - The name of the custom AZ.
--
-- 'vpnDetails', 'customAvailabilityZone_vpnDetails' - Information about the virtual private network (VPN) between the VMware
-- vSphere cluster and the AWS website.
--
-- 'customAvailabilityZoneStatus', 'customAvailabilityZone_customAvailabilityZoneStatus' - The status of the custom AZ.
newCustomAvailabilityZone ::
  CustomAvailabilityZone
newCustomAvailabilityZone =
  CustomAvailabilityZone'
    { customAvailabilityZoneId =
        Prelude.Nothing,
      customAvailabilityZoneName = Prelude.Nothing,
      vpnDetails = Prelude.Nothing,
      customAvailabilityZoneStatus = Prelude.Nothing
    }

-- | The identifier of the custom AZ.
--
-- Amazon RDS generates a unique identifier when a custom AZ is created.
customAvailabilityZone_customAvailabilityZoneId :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe Prelude.Text)
customAvailabilityZone_customAvailabilityZoneId = Lens.lens (\CustomAvailabilityZone' {customAvailabilityZoneId} -> customAvailabilityZoneId) (\s@CustomAvailabilityZone' {} a -> s {customAvailabilityZoneId = a} :: CustomAvailabilityZone)

-- | The name of the custom AZ.
customAvailabilityZone_customAvailabilityZoneName :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe Prelude.Text)
customAvailabilityZone_customAvailabilityZoneName = Lens.lens (\CustomAvailabilityZone' {customAvailabilityZoneName} -> customAvailabilityZoneName) (\s@CustomAvailabilityZone' {} a -> s {customAvailabilityZoneName = a} :: CustomAvailabilityZone)

-- | Information about the virtual private network (VPN) between the VMware
-- vSphere cluster and the AWS website.
customAvailabilityZone_vpnDetails :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe VpnDetails)
customAvailabilityZone_vpnDetails = Lens.lens (\CustomAvailabilityZone' {vpnDetails} -> vpnDetails) (\s@CustomAvailabilityZone' {} a -> s {vpnDetails = a} :: CustomAvailabilityZone)

-- | The status of the custom AZ.
customAvailabilityZone_customAvailabilityZoneStatus :: Lens.Lens' CustomAvailabilityZone (Prelude.Maybe Prelude.Text)
customAvailabilityZone_customAvailabilityZoneStatus = Lens.lens (\CustomAvailabilityZone' {customAvailabilityZoneStatus} -> customAvailabilityZoneStatus) (\s@CustomAvailabilityZone' {} a -> s {customAvailabilityZoneStatus = a} :: CustomAvailabilityZone)

instance Prelude.FromXML CustomAvailabilityZone where
  parseXML x =
    CustomAvailabilityZone'
      Prelude.<$> (x Prelude..@? "CustomAvailabilityZoneId")
      Prelude.<*> (x Prelude..@? "CustomAvailabilityZoneName")
      Prelude.<*> (x Prelude..@? "VpnDetails")
      Prelude.<*> (x Prelude..@? "CustomAvailabilityZoneStatus")

instance Prelude.Hashable CustomAvailabilityZone

instance Prelude.NFData CustomAvailabilityZone
