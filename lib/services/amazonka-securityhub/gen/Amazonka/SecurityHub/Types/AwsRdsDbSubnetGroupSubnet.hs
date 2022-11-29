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
-- Module      : Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnetAvailabilityZone

-- | Information about a subnet in a subnet group.
--
-- /See:/ 'newAwsRdsDbSubnetGroupSubnet' smart constructor.
data AwsRdsDbSubnetGroupSubnet = AwsRdsDbSubnetGroupSubnet'
  { -- | The identifier of a subnet in the subnet group.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The status of a subnet in the subnet group.
    subnetStatus :: Prelude.Maybe Prelude.Text,
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
-- 'subnetIdentifier', 'awsRdsDbSubnetGroupSubnet_subnetIdentifier' - The identifier of a subnet in the subnet group.
--
-- 'subnetStatus', 'awsRdsDbSubnetGroupSubnet_subnetStatus' - The status of a subnet in the subnet group.
--
-- 'subnetAvailabilityZone', 'awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone' - Information about the Availability Zone for a subnet in the subnet
-- group.
newAwsRdsDbSubnetGroupSubnet ::
  AwsRdsDbSubnetGroupSubnet
newAwsRdsDbSubnetGroupSubnet =
  AwsRdsDbSubnetGroupSubnet'
    { subnetIdentifier =
        Prelude.Nothing,
      subnetStatus = Prelude.Nothing,
      subnetAvailabilityZone = Prelude.Nothing
    }

-- | The identifier of a subnet in the subnet group.
awsRdsDbSubnetGroupSubnet_subnetIdentifier :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroupSubnet_subnetIdentifier = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetIdentifier} -> subnetIdentifier) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetIdentifier = a} :: AwsRdsDbSubnetGroupSubnet)

-- | The status of a subnet in the subnet group.
awsRdsDbSubnetGroupSubnet_subnetStatus :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroupSubnet_subnetStatus = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetStatus} -> subnetStatus) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetStatus = a} :: AwsRdsDbSubnetGroupSubnet)

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
            Prelude.<$> (x Core..:? "SubnetIdentifier")
            Prelude.<*> (x Core..:? "SubnetStatus")
            Prelude.<*> (x Core..:? "SubnetAvailabilityZone")
      )

instance Prelude.Hashable AwsRdsDbSubnetGroupSubnet where
  hashWithSalt _salt AwsRdsDbSubnetGroupSubnet' {..} =
    _salt `Prelude.hashWithSalt` subnetIdentifier
      `Prelude.hashWithSalt` subnetStatus
      `Prelude.hashWithSalt` subnetAvailabilityZone

instance Prelude.NFData AwsRdsDbSubnetGroupSubnet where
  rnf AwsRdsDbSubnetGroupSubnet' {..} =
    Prelude.rnf subnetIdentifier
      `Prelude.seq` Prelude.rnf subnetStatus
      `Prelude.seq` Prelude.rnf subnetAvailabilityZone

instance Core.ToJSON AwsRdsDbSubnetGroupSubnet where
  toJSON AwsRdsDbSubnetGroupSubnet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SubnetIdentifier" Core..=)
              Prelude.<$> subnetIdentifier,
            ("SubnetStatus" Core..=) Prelude.<$> subnetStatus,
            ("SubnetAvailabilityZone" Core..=)
              Prelude.<$> subnetAvailabilityZone
          ]
      )
