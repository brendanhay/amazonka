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
-- Module      : Amazonka.FMS.Types.FirewallSubnetMissingVPCEndpointViolation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.FirewallSubnetMissingVPCEndpointViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The violation details for a firewall subnet\'s VPC endpoint that\'s
-- deleted or missing.
--
-- /See:/ 'newFirewallSubnetMissingVPCEndpointViolation' smart constructor.
data FirewallSubnetMissingVPCEndpointViolation = FirewallSubnetMissingVPCEndpointViolation'
  { -- | The ID of the firewall that this VPC endpoint is associated with.
    firewallSubnetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Availability Zone of the deleted VPC subnet.
    subnetAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Availability Zone of the deleted VPC subnet.
    subnetAvailabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The resource ID of the VPC associated with the deleted VPC subnet.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallSubnetMissingVPCEndpointViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallSubnetId', 'firewallSubnetMissingVPCEndpointViolation_firewallSubnetId' - The ID of the firewall that this VPC endpoint is associated with.
--
-- 'subnetAvailabilityZone', 'firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZone' - The name of the Availability Zone of the deleted VPC subnet.
--
-- 'subnetAvailabilityZoneId', 'firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZoneId' - The ID of the Availability Zone of the deleted VPC subnet.
--
-- 'vpcId', 'firewallSubnetMissingVPCEndpointViolation_vpcId' - The resource ID of the VPC associated with the deleted VPC subnet.
newFirewallSubnetMissingVPCEndpointViolation ::
  FirewallSubnetMissingVPCEndpointViolation
newFirewallSubnetMissingVPCEndpointViolation =
  FirewallSubnetMissingVPCEndpointViolation'
    { firewallSubnetId =
        Prelude.Nothing,
      subnetAvailabilityZone =
        Prelude.Nothing,
      subnetAvailabilityZoneId =
        Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The ID of the firewall that this VPC endpoint is associated with.
firewallSubnetMissingVPCEndpointViolation_firewallSubnetId :: Lens.Lens' FirewallSubnetMissingVPCEndpointViolation (Prelude.Maybe Prelude.Text)
firewallSubnetMissingVPCEndpointViolation_firewallSubnetId = Lens.lens (\FirewallSubnetMissingVPCEndpointViolation' {firewallSubnetId} -> firewallSubnetId) (\s@FirewallSubnetMissingVPCEndpointViolation' {} a -> s {firewallSubnetId = a} :: FirewallSubnetMissingVPCEndpointViolation)

-- | The name of the Availability Zone of the deleted VPC subnet.
firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZone :: Lens.Lens' FirewallSubnetMissingVPCEndpointViolation (Prelude.Maybe Prelude.Text)
firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZone = Lens.lens (\FirewallSubnetMissingVPCEndpointViolation' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@FirewallSubnetMissingVPCEndpointViolation' {} a -> s {subnetAvailabilityZone = a} :: FirewallSubnetMissingVPCEndpointViolation)

-- | The ID of the Availability Zone of the deleted VPC subnet.
firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZoneId :: Lens.Lens' FirewallSubnetMissingVPCEndpointViolation (Prelude.Maybe Prelude.Text)
firewallSubnetMissingVPCEndpointViolation_subnetAvailabilityZoneId = Lens.lens (\FirewallSubnetMissingVPCEndpointViolation' {subnetAvailabilityZoneId} -> subnetAvailabilityZoneId) (\s@FirewallSubnetMissingVPCEndpointViolation' {} a -> s {subnetAvailabilityZoneId = a} :: FirewallSubnetMissingVPCEndpointViolation)

-- | The resource ID of the VPC associated with the deleted VPC subnet.
firewallSubnetMissingVPCEndpointViolation_vpcId :: Lens.Lens' FirewallSubnetMissingVPCEndpointViolation (Prelude.Maybe Prelude.Text)
firewallSubnetMissingVPCEndpointViolation_vpcId = Lens.lens (\FirewallSubnetMissingVPCEndpointViolation' {vpcId} -> vpcId) (\s@FirewallSubnetMissingVPCEndpointViolation' {} a -> s {vpcId = a} :: FirewallSubnetMissingVPCEndpointViolation)

instance
  Data.FromJSON
    FirewallSubnetMissingVPCEndpointViolation
  where
  parseJSON =
    Data.withObject
      "FirewallSubnetMissingVPCEndpointViolation"
      ( \x ->
          FirewallSubnetMissingVPCEndpointViolation'
            Prelude.<$> (x Data..:? "FirewallSubnetId")
            Prelude.<*> (x Data..:? "SubnetAvailabilityZone")
            Prelude.<*> (x Data..:? "SubnetAvailabilityZoneId")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    FirewallSubnetMissingVPCEndpointViolation
  where
  hashWithSalt
    _salt
    FirewallSubnetMissingVPCEndpointViolation' {..} =
      _salt
        `Prelude.hashWithSalt` firewallSubnetId
        `Prelude.hashWithSalt` subnetAvailabilityZone
        `Prelude.hashWithSalt` subnetAvailabilityZoneId
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    FirewallSubnetMissingVPCEndpointViolation
  where
  rnf FirewallSubnetMissingVPCEndpointViolation' {..} =
    Prelude.rnf firewallSubnetId `Prelude.seq`
      Prelude.rnf subnetAvailabilityZone `Prelude.seq`
        Prelude.rnf subnetAvailabilityZoneId `Prelude.seq`
          Prelude.rnf vpcId
