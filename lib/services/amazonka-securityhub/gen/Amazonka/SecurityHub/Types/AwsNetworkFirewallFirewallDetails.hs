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
-- Module      : Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallSubnetMappingsDetails

-- | Details about an Network Firewall firewall.
--
-- /See:/ 'newAwsNetworkFirewallFirewallDetails' smart constructor.
data AwsNetworkFirewallFirewallDetails = AwsNetworkFirewallFirewallDetails'
  { -- | Whether the firewall is protected from deletion. If set to @true@, then
    -- the firewall cannot be deleted.
    deleteProtection :: Prelude.Maybe Prelude.Bool,
    -- | Whether the firewall is protected from a change to the subnet
    -- associations. If set to @true@, you cannot map different subnets to the
    -- firewall.
    subnetChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the firewall.
    firewallId :: Prelude.Maybe Prelude.Text,
    -- | A description of the firewall.
    description :: Prelude.Maybe Prelude.Text,
    -- | Whether the firewall is protected from a change to the firewall policy.
    -- If set to @true@, you cannot associate a different policy with the
    -- firewall.
    firewallPolicyChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The public subnets that Network Firewall uses for the firewall. Each
    -- subnet must belong to a different Availability Zone.
    subnetMappings :: Prelude.Maybe [AwsNetworkFirewallFirewallSubnetMappingsDetails],
    -- | A descriptive name of the firewall.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC where the firewall is used.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the firewall policy.
    firewallPolicyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsNetworkFirewallFirewallDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteProtection', 'awsNetworkFirewallFirewallDetails_deleteProtection' - Whether the firewall is protected from deletion. If set to @true@, then
-- the firewall cannot be deleted.
--
-- 'subnetChangeProtection', 'awsNetworkFirewallFirewallDetails_subnetChangeProtection' - Whether the firewall is protected from a change to the subnet
-- associations. If set to @true@, you cannot map different subnets to the
-- firewall.
--
-- 'firewallId', 'awsNetworkFirewallFirewallDetails_firewallId' - The identifier of the firewall.
--
-- 'description', 'awsNetworkFirewallFirewallDetails_description' - A description of the firewall.
--
-- 'firewallPolicyChangeProtection', 'awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection' - Whether the firewall is protected from a change to the firewall policy.
-- If set to @true@, you cannot associate a different policy with the
-- firewall.
--
-- 'firewallArn', 'awsNetworkFirewallFirewallDetails_firewallArn' - The ARN of the firewall.
--
-- 'subnetMappings', 'awsNetworkFirewallFirewallDetails_subnetMappings' - The public subnets that Network Firewall uses for the firewall. Each
-- subnet must belong to a different Availability Zone.
--
-- 'firewallName', 'awsNetworkFirewallFirewallDetails_firewallName' - A descriptive name of the firewall.
--
-- 'vpcId', 'awsNetworkFirewallFirewallDetails_vpcId' - The identifier of the VPC where the firewall is used.
--
-- 'firewallPolicyArn', 'awsNetworkFirewallFirewallDetails_firewallPolicyArn' - The ARN of the firewall policy.
newAwsNetworkFirewallFirewallDetails ::
  AwsNetworkFirewallFirewallDetails
newAwsNetworkFirewallFirewallDetails =
  AwsNetworkFirewallFirewallDetails'
    { deleteProtection =
        Prelude.Nothing,
      subnetChangeProtection = Prelude.Nothing,
      firewallId = Prelude.Nothing,
      description = Prelude.Nothing,
      firewallPolicyChangeProtection =
        Prelude.Nothing,
      firewallArn = Prelude.Nothing,
      subnetMappings = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      firewallPolicyArn = Prelude.Nothing
    }

-- | Whether the firewall is protected from deletion. If set to @true@, then
-- the firewall cannot be deleted.
awsNetworkFirewallFirewallDetails_deleteProtection :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Bool)
awsNetworkFirewallFirewallDetails_deleteProtection = Lens.lens (\AwsNetworkFirewallFirewallDetails' {deleteProtection} -> deleteProtection) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {deleteProtection = a} :: AwsNetworkFirewallFirewallDetails)

-- | Whether the firewall is protected from a change to the subnet
-- associations. If set to @true@, you cannot map different subnets to the
-- firewall.
awsNetworkFirewallFirewallDetails_subnetChangeProtection :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Bool)
awsNetworkFirewallFirewallDetails_subnetChangeProtection = Lens.lens (\AwsNetworkFirewallFirewallDetails' {subnetChangeProtection} -> subnetChangeProtection) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {subnetChangeProtection = a} :: AwsNetworkFirewallFirewallDetails)

-- | The identifier of the firewall.
awsNetworkFirewallFirewallDetails_firewallId :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallId = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallId} -> firewallId) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallId = a} :: AwsNetworkFirewallFirewallDetails)

-- | A description of the firewall.
awsNetworkFirewallFirewallDetails_description :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_description = Lens.lens (\AwsNetworkFirewallFirewallDetails' {description} -> description) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {description = a} :: AwsNetworkFirewallFirewallDetails)

-- | Whether the firewall is protected from a change to the firewall policy.
-- If set to @true@, you cannot associate a different policy with the
-- firewall.
awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Bool)
awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallPolicyChangeProtection} -> firewallPolicyChangeProtection) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallPolicyChangeProtection = a} :: AwsNetworkFirewallFirewallDetails)

-- | The ARN of the firewall.
awsNetworkFirewallFirewallDetails_firewallArn :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallArn = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallArn} -> firewallArn) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallArn = a} :: AwsNetworkFirewallFirewallDetails)

-- | The public subnets that Network Firewall uses for the firewall. Each
-- subnet must belong to a different Availability Zone.
awsNetworkFirewallFirewallDetails_subnetMappings :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe [AwsNetworkFirewallFirewallSubnetMappingsDetails])
awsNetworkFirewallFirewallDetails_subnetMappings = Lens.lens (\AwsNetworkFirewallFirewallDetails' {subnetMappings} -> subnetMappings) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {subnetMappings = a} :: AwsNetworkFirewallFirewallDetails) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive name of the firewall.
awsNetworkFirewallFirewallDetails_firewallName :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallName = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallName} -> firewallName) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallName = a} :: AwsNetworkFirewallFirewallDetails)

-- | The identifier of the VPC where the firewall is used.
awsNetworkFirewallFirewallDetails_vpcId :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_vpcId = Lens.lens (\AwsNetworkFirewallFirewallDetails' {vpcId} -> vpcId) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {vpcId = a} :: AwsNetworkFirewallFirewallDetails)

-- | The ARN of the firewall policy.
awsNetworkFirewallFirewallDetails_firewallPolicyArn :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallPolicyArn = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallPolicyArn} -> firewallPolicyArn) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallPolicyArn = a} :: AwsNetworkFirewallFirewallDetails)

instance
  Data.FromJSON
    AwsNetworkFirewallFirewallDetails
  where
  parseJSON =
    Data.withObject
      "AwsNetworkFirewallFirewallDetails"
      ( \x ->
          AwsNetworkFirewallFirewallDetails'
            Prelude.<$> (x Data..:? "DeleteProtection")
            Prelude.<*> (x Data..:? "SubnetChangeProtection")
            Prelude.<*> (x Data..:? "FirewallId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FirewallPolicyChangeProtection")
            Prelude.<*> (x Data..:? "FirewallArn")
            Prelude.<*> (x Data..:? "SubnetMappings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FirewallName")
            Prelude.<*> (x Data..:? "VpcId")
            Prelude.<*> (x Data..:? "FirewallPolicyArn")
      )

instance
  Prelude.Hashable
    AwsNetworkFirewallFirewallDetails
  where
  hashWithSalt
    _salt
    AwsNetworkFirewallFirewallDetails' {..} =
      _salt `Prelude.hashWithSalt` deleteProtection
        `Prelude.hashWithSalt` subnetChangeProtection
        `Prelude.hashWithSalt` firewallId
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` firewallPolicyChangeProtection
        `Prelude.hashWithSalt` firewallArn
        `Prelude.hashWithSalt` subnetMappings
        `Prelude.hashWithSalt` firewallName
        `Prelude.hashWithSalt` vpcId
        `Prelude.hashWithSalt` firewallPolicyArn

instance
  Prelude.NFData
    AwsNetworkFirewallFirewallDetails
  where
  rnf AwsNetworkFirewallFirewallDetails' {..} =
    Prelude.rnf deleteProtection
      `Prelude.seq` Prelude.rnf subnetChangeProtection
      `Prelude.seq` Prelude.rnf firewallId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf firewallPolicyChangeProtection
      `Prelude.seq` Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf subnetMappings
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf firewallPolicyArn

instance
  Data.ToJSON
    AwsNetworkFirewallFirewallDetails
  where
  toJSON AwsNetworkFirewallFirewallDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteProtection" Data..=)
              Prelude.<$> deleteProtection,
            ("SubnetChangeProtection" Data..=)
              Prelude.<$> subnetChangeProtection,
            ("FirewallId" Data..=) Prelude.<$> firewallId,
            ("Description" Data..=) Prelude.<$> description,
            ("FirewallPolicyChangeProtection" Data..=)
              Prelude.<$> firewallPolicyChangeProtection,
            ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("SubnetMappings" Data..=)
              Prelude.<$> subnetMappings,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            ("VpcId" Data..=) Prelude.<$> vpcId,
            ("FirewallPolicyArn" Data..=)
              Prelude.<$> firewallPolicyArn
          ]
      )
