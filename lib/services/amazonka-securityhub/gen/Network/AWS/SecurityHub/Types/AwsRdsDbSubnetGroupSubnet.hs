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
-- Module      : Network.AWS.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SecurityHub.Types.AwsRdsDbSubnetGroupSubnetAvailabilityZone

-- | Information about a subnet in a subnet group.
--
-- /See:/ 'newAwsRdsDbSubnetGroupSubnet' smart constructor.
data AwsRdsDbSubnetGroupSubnet = AwsRdsDbSubnetGroupSubnet'
  { -- | The status of a subnet in the subnet group.
    subnetStatus :: Prelude.Maybe Prelude.Text,
    -- | The identifier of a subnet in the subnet group.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Information about the Availability Zone for a subnet in the subnet
    -- group.
    subnetAvailabilityZone :: Prelude.Maybe AwsRdsDbSubnetGroupSubnetAvailabilityZone
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRdsDbSubnetGroupSubnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetStatus', 'awsRdsDbSubnetGroupSubnet_subnetStatus' - The status of a subnet in the subnet group.
--
-- 'subnetIdentifier', 'awsRdsDbSubnetGroupSubnet_subnetIdentifier' - The identifier of a subnet in the subnet group.
--
-- 'subnetAvailabilityZone', 'awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone' - Information about the Availability Zone for a subnet in the subnet
-- group.
newAwsRdsDbSubnetGroupSubnet ::
  AwsRdsDbSubnetGroupSubnet
newAwsRdsDbSubnetGroupSubnet =
  AwsRdsDbSubnetGroupSubnet'
    { subnetStatus =
        Prelude.Nothing,
      subnetIdentifier = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing
    }

-- | The status of a subnet in the subnet group.
awsRdsDbSubnetGroupSubnet_subnetStatus :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroupSubnet_subnetStatus = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetStatus} -> subnetStatus) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetStatus = a} :: AwsRdsDbSubnetGroupSubnet)

-- | The identifier of a subnet in the subnet group.
awsRdsDbSubnetGroupSubnet_subnetIdentifier :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroupSubnet_subnetIdentifier = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetIdentifier} -> subnetIdentifier) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetIdentifier = a} :: AwsRdsDbSubnetGroupSubnet)

-- | Information about the Availability Zone for a subnet in the subnet
-- group.
awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe AwsRdsDbSubnetGroupSubnetAvailabilityZone)
awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetAvailabilityZone = a} :: AwsRdsDbSubnetGroupSubnet)

instance Core.FromJSON AwsRdsDbSubnetGroupSubnet where
  parseJSON =
    Core.withObject
      "AwsRdsDbSubnetGroupSubnet"
      ( \x ->
          AwsRdsDbSubnetGroupSubnet'
            Prelude.<$> (x Core..:? "SubnetStatus")
            Prelude.<*> (x Core..:? "SubnetIdentifier")
            Prelude.<*> (x Core..:? "SubnetAvailabilityZone")
      )

instance Prelude.Hashable AwsRdsDbSubnetGroupSubnet

instance Prelude.NFData AwsRdsDbSubnetGroupSubnet

instance Core.ToJSON AwsRdsDbSubnetGroupSubnet where
  toJSON AwsRdsDbSubnetGroupSubnet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SubnetStatus" Core..=) Prelude.<$> subnetStatus,
            ("SubnetIdentifier" Core..=)
              Prelude.<$> subnetIdentifier,
            ("SubnetAvailabilityZone" Core..=)
              Prelude.<$> subnetAvailabilityZone
          ]
      )
