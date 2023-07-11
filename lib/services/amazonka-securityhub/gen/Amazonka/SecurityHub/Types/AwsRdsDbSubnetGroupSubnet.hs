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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRdsDbSubnetGroupSubnetAvailabilityZone

-- | Information about a subnet in a subnet group.
--
-- /See:/ 'newAwsRdsDbSubnetGroupSubnet' smart constructor.
data AwsRdsDbSubnetGroupSubnet = AwsRdsDbSubnetGroupSubnet'
  { -- | Information about the Availability Zone for a subnet in the subnet
    -- group.
    subnetAvailabilityZone :: Prelude.Maybe AwsRdsDbSubnetGroupSubnetAvailabilityZone,
    -- | The identifier of a subnet in the subnet group.
    subnetIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The status of a subnet in the subnet group.
    subnetStatus :: Prelude.Maybe Prelude.Text
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
-- 'subnetAvailabilityZone', 'awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone' - Information about the Availability Zone for a subnet in the subnet
-- group.
--
-- 'subnetIdentifier', 'awsRdsDbSubnetGroupSubnet_subnetIdentifier' - The identifier of a subnet in the subnet group.
--
-- 'subnetStatus', 'awsRdsDbSubnetGroupSubnet_subnetStatus' - The status of a subnet in the subnet group.
newAwsRdsDbSubnetGroupSubnet ::
  AwsRdsDbSubnetGroupSubnet
newAwsRdsDbSubnetGroupSubnet =
  AwsRdsDbSubnetGroupSubnet'
    { subnetAvailabilityZone =
        Prelude.Nothing,
      subnetIdentifier = Prelude.Nothing,
      subnetStatus = Prelude.Nothing
    }

-- | Information about the Availability Zone for a subnet in the subnet
-- group.
awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe AwsRdsDbSubnetGroupSubnetAvailabilityZone)
awsRdsDbSubnetGroupSubnet_subnetAvailabilityZone = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetAvailabilityZone} -> subnetAvailabilityZone) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetAvailabilityZone = a} :: AwsRdsDbSubnetGroupSubnet)

-- | The identifier of a subnet in the subnet group.
awsRdsDbSubnetGroupSubnet_subnetIdentifier :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroupSubnet_subnetIdentifier = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetIdentifier} -> subnetIdentifier) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetIdentifier = a} :: AwsRdsDbSubnetGroupSubnet)

-- | The status of a subnet in the subnet group.
awsRdsDbSubnetGroupSubnet_subnetStatus :: Lens.Lens' AwsRdsDbSubnetGroupSubnet (Prelude.Maybe Prelude.Text)
awsRdsDbSubnetGroupSubnet_subnetStatus = Lens.lens (\AwsRdsDbSubnetGroupSubnet' {subnetStatus} -> subnetStatus) (\s@AwsRdsDbSubnetGroupSubnet' {} a -> s {subnetStatus = a} :: AwsRdsDbSubnetGroupSubnet)

instance Data.FromJSON AwsRdsDbSubnetGroupSubnet where
  parseJSON =
    Data.withObject
      "AwsRdsDbSubnetGroupSubnet"
      ( \x ->
          AwsRdsDbSubnetGroupSubnet'
            Prelude.<$> (x Data..:? "SubnetAvailabilityZone")
            Prelude.<*> (x Data..:? "SubnetIdentifier")
            Prelude.<*> (x Data..:? "SubnetStatus")
      )

instance Prelude.Hashable AwsRdsDbSubnetGroupSubnet where
  hashWithSalt _salt AwsRdsDbSubnetGroupSubnet' {..} =
    _salt
      `Prelude.hashWithSalt` subnetAvailabilityZone
      `Prelude.hashWithSalt` subnetIdentifier
      `Prelude.hashWithSalt` subnetStatus

instance Prelude.NFData AwsRdsDbSubnetGroupSubnet where
  rnf AwsRdsDbSubnetGroupSubnet' {..} =
    Prelude.rnf subnetAvailabilityZone
      `Prelude.seq` Prelude.rnf subnetIdentifier
      `Prelude.seq` Prelude.rnf subnetStatus

instance Data.ToJSON AwsRdsDbSubnetGroupSubnet where
  toJSON AwsRdsDbSubnetGroupSubnet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SubnetAvailabilityZone" Data..=)
              Prelude.<$> subnetAvailabilityZone,
            ("SubnetIdentifier" Data..=)
              Prelude.<$> subnetIdentifier,
            ("SubnetStatus" Data..=) Prelude.<$> subnetStatus
          ]
      )
