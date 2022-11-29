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
-- Module      : Amazonka.MediaLive.Types.VpcOutputSettingsDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.VpcOutputSettingsDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties for a private VPC Output
--
-- /See:/ 'newVpcOutputSettingsDescription' smart constructor.
data VpcOutputSettingsDescription = VpcOutputSettingsDescription'
  { -- | A list of up EC2 VPC security group IDs attached to the Output VPC
    -- network interfaces.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The Availability Zones where the vpc subnets are located. The first
    -- Availability Zone applies to the first subnet in the list of subnets.
    -- The second Availability Zone applies to the second subnet.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | A list of Elastic Network Interfaces created by MediaLive in the
    -- customer\'s VPC
    networkInterfaceIds :: Prelude.Maybe [Prelude.Text],
    -- | A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
    -- IDs must be mapped to two unique availability zones (AZ).
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcOutputSettingsDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'vpcOutputSettingsDescription_securityGroupIds' - A list of up EC2 VPC security group IDs attached to the Output VPC
-- network interfaces.
--
-- 'availabilityZones', 'vpcOutputSettingsDescription_availabilityZones' - The Availability Zones where the vpc subnets are located. The first
-- Availability Zone applies to the first subnet in the list of subnets.
-- The second Availability Zone applies to the second subnet.
--
-- 'networkInterfaceIds', 'vpcOutputSettingsDescription_networkInterfaceIds' - A list of Elastic Network Interfaces created by MediaLive in the
-- customer\'s VPC
--
-- 'subnetIds', 'vpcOutputSettingsDescription_subnetIds' - A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
-- IDs must be mapped to two unique availability zones (AZ).
newVpcOutputSettingsDescription ::
  VpcOutputSettingsDescription
newVpcOutputSettingsDescription =
  VpcOutputSettingsDescription'
    { securityGroupIds =
        Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      networkInterfaceIds = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | A list of up EC2 VPC security group IDs attached to the Output VPC
-- network interfaces.
vpcOutputSettingsDescription_securityGroupIds :: Lens.Lens' VpcOutputSettingsDescription (Prelude.Maybe [Prelude.Text])
vpcOutputSettingsDescription_securityGroupIds = Lens.lens (\VpcOutputSettingsDescription' {securityGroupIds} -> securityGroupIds) (\s@VpcOutputSettingsDescription' {} a -> s {securityGroupIds = a} :: VpcOutputSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zones where the vpc subnets are located. The first
-- Availability Zone applies to the first subnet in the list of subnets.
-- The second Availability Zone applies to the second subnet.
vpcOutputSettingsDescription_availabilityZones :: Lens.Lens' VpcOutputSettingsDescription (Prelude.Maybe [Prelude.Text])
vpcOutputSettingsDescription_availabilityZones = Lens.lens (\VpcOutputSettingsDescription' {availabilityZones} -> availabilityZones) (\s@VpcOutputSettingsDescription' {} a -> s {availabilityZones = a} :: VpcOutputSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | A list of Elastic Network Interfaces created by MediaLive in the
-- customer\'s VPC
vpcOutputSettingsDescription_networkInterfaceIds :: Lens.Lens' VpcOutputSettingsDescription (Prelude.Maybe [Prelude.Text])
vpcOutputSettingsDescription_networkInterfaceIds = Lens.lens (\VpcOutputSettingsDescription' {networkInterfaceIds} -> networkInterfaceIds) (\s@VpcOutputSettingsDescription' {} a -> s {networkInterfaceIds = a} :: VpcOutputSettingsDescription) Prelude.. Lens.mapping Lens.coerced

-- | A list of VPC subnet IDs from the same VPC. If STANDARD channel, subnet
-- IDs must be mapped to two unique availability zones (AZ).
vpcOutputSettingsDescription_subnetIds :: Lens.Lens' VpcOutputSettingsDescription (Prelude.Maybe [Prelude.Text])
vpcOutputSettingsDescription_subnetIds = Lens.lens (\VpcOutputSettingsDescription' {subnetIds} -> subnetIds) (\s@VpcOutputSettingsDescription' {} a -> s {subnetIds = a} :: VpcOutputSettingsDescription) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON VpcOutputSettingsDescription where
  parseJSON =
    Core.withObject
      "VpcOutputSettingsDescription"
      ( \x ->
          VpcOutputSettingsDescription'
            Prelude.<$> ( x Core..:? "securityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "availabilityZones"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "networkInterfaceIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "subnetIds" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    VpcOutputSettingsDescription
  where
  hashWithSalt _salt VpcOutputSettingsDescription' {..} =
    _salt `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` networkInterfaceIds
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData VpcOutputSettingsDescription where
  rnf VpcOutputSettingsDescription' {..} =
    Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf networkInterfaceIds
      `Prelude.seq` Prelude.rnf subnetIds
