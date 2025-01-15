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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    -- | A description of the firewall.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the firewall.
    firewallId :: Prelude.Maybe Prelude.Text,
    -- | A descriptive name of the firewall.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the firewall policy.
    firewallPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | Whether the firewall is protected from a change to the firewall policy.
    -- If set to @true@, you cannot associate a different policy with the
    -- firewall.
    firewallPolicyChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | Whether the firewall is protected from a change to the subnet
    -- associations. If set to @true@, you cannot map different subnets to the
    -- firewall.
    subnetChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | The public subnets that Network Firewall uses for the firewall. Each
    -- subnet must belong to a different Availability Zone.
    subnetMappings :: Prelude.Maybe [AwsNetworkFirewallFirewallSubnetMappingsDetails],
    -- | The identifier of the VPC where the firewall is used.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'description', 'awsNetworkFirewallFirewallDetails_description' - A description of the firewall.
--
-- 'firewallArn', 'awsNetworkFirewallFirewallDetails_firewallArn' - The ARN of the firewall.
--
-- 'firewallId', 'awsNetworkFirewallFirewallDetails_firewallId' - The identifier of the firewall.
--
-- 'firewallName', 'awsNetworkFirewallFirewallDetails_firewallName' - A descriptive name of the firewall.
--
-- 'firewallPolicyArn', 'awsNetworkFirewallFirewallDetails_firewallPolicyArn' - The ARN of the firewall policy.
--
-- 'firewallPolicyChangeProtection', 'awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection' - Whether the firewall is protected from a change to the firewall policy.
-- If set to @true@, you cannot associate a different policy with the
-- firewall.
--
-- 'subnetChangeProtection', 'awsNetworkFirewallFirewallDetails_subnetChangeProtection' - Whether the firewall is protected from a change to the subnet
-- associations. If set to @true@, you cannot map different subnets to the
-- firewall.
--
-- 'subnetMappings', 'awsNetworkFirewallFirewallDetails_subnetMappings' - The public subnets that Network Firewall uses for the firewall. Each
-- subnet must belong to a different Availability Zone.
--
-- 'vpcId', 'awsNetworkFirewallFirewallDetails_vpcId' - The identifier of the VPC where the firewall is used.
newAwsNetworkFirewallFirewallDetails ::
  AwsNetworkFirewallFirewallDetails
newAwsNetworkFirewallFirewallDetails =
  AwsNetworkFirewallFirewallDetails'
    { deleteProtection =
        Prelude.Nothing,
      description = Prelude.Nothing,
      firewallArn = Prelude.Nothing,
      firewallId = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      firewallPolicyArn = Prelude.Nothing,
      firewallPolicyChangeProtection =
        Prelude.Nothing,
      subnetChangeProtection = Prelude.Nothing,
      subnetMappings = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Whether the firewall is protected from deletion. If set to @true@, then
-- the firewall cannot be deleted.
awsNetworkFirewallFirewallDetails_deleteProtection :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Bool)
awsNetworkFirewallFirewallDetails_deleteProtection = Lens.lens (\AwsNetworkFirewallFirewallDetails' {deleteProtection} -> deleteProtection) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {deleteProtection = a} :: AwsNetworkFirewallFirewallDetails)

-- | A description of the firewall.
awsNetworkFirewallFirewallDetails_description :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_description = Lens.lens (\AwsNetworkFirewallFirewallDetails' {description} -> description) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {description = a} :: AwsNetworkFirewallFirewallDetails)

-- | The ARN of the firewall.
awsNetworkFirewallFirewallDetails_firewallArn :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallArn = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallArn} -> firewallArn) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallArn = a} :: AwsNetworkFirewallFirewallDetails)

-- | The identifier of the firewall.
awsNetworkFirewallFirewallDetails_firewallId :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallId = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallId} -> firewallId) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallId = a} :: AwsNetworkFirewallFirewallDetails)

-- | A descriptive name of the firewall.
awsNetworkFirewallFirewallDetails_firewallName :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallName = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallName} -> firewallName) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallName = a} :: AwsNetworkFirewallFirewallDetails)

-- | The ARN of the firewall policy.
awsNetworkFirewallFirewallDetails_firewallPolicyArn :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_firewallPolicyArn = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallPolicyArn} -> firewallPolicyArn) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallPolicyArn = a} :: AwsNetworkFirewallFirewallDetails)

-- | Whether the firewall is protected from a change to the firewall policy.
-- If set to @true@, you cannot associate a different policy with the
-- firewall.
awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Bool)
awsNetworkFirewallFirewallDetails_firewallPolicyChangeProtection = Lens.lens (\AwsNetworkFirewallFirewallDetails' {firewallPolicyChangeProtection} -> firewallPolicyChangeProtection) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {firewallPolicyChangeProtection = a} :: AwsNetworkFirewallFirewallDetails)

-- | Whether the firewall is protected from a change to the subnet
-- associations. If set to @true@, you cannot map different subnets to the
-- firewall.
awsNetworkFirewallFirewallDetails_subnetChangeProtection :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Bool)
awsNetworkFirewallFirewallDetails_subnetChangeProtection = Lens.lens (\AwsNetworkFirewallFirewallDetails' {subnetChangeProtection} -> subnetChangeProtection) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {subnetChangeProtection = a} :: AwsNetworkFirewallFirewallDetails)

-- | The public subnets that Network Firewall uses for the firewall. Each
-- subnet must belong to a different Availability Zone.
awsNetworkFirewallFirewallDetails_subnetMappings :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe [AwsNetworkFirewallFirewallSubnetMappingsDetails])
awsNetworkFirewallFirewallDetails_subnetMappings = Lens.lens (\AwsNetworkFirewallFirewallDetails' {subnetMappings} -> subnetMappings) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {subnetMappings = a} :: AwsNetworkFirewallFirewallDetails) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the VPC where the firewall is used.
awsNetworkFirewallFirewallDetails_vpcId :: Lens.Lens' AwsNetworkFirewallFirewallDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallDetails_vpcId = Lens.lens (\AwsNetworkFirewallFirewallDetails' {vpcId} -> vpcId) (\s@AwsNetworkFirewallFirewallDetails' {} a -> s {vpcId = a} :: AwsNetworkFirewallFirewallDetails)

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
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FirewallArn")
            Prelude.<*> (x Data..:? "FirewallId")
            Prelude.<*> (x Data..:? "FirewallName")
            Prelude.<*> (x Data..:? "FirewallPolicyArn")
            Prelude.<*> (x Data..:? "FirewallPolicyChangeProtection")
            Prelude.<*> (x Data..:? "SubnetChangeProtection")
            Prelude.<*> (x Data..:? "SubnetMappings" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    AwsNetworkFirewallFirewallDetails
  where
  hashWithSalt
    _salt
    AwsNetworkFirewallFirewallDetails' {..} =
      _salt
        `Prelude.hashWithSalt` deleteProtection
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` firewallArn
        `Prelude.hashWithSalt` firewallId
        `Prelude.hashWithSalt` firewallName
        `Prelude.hashWithSalt` firewallPolicyArn
        `Prelude.hashWithSalt` firewallPolicyChangeProtection
        `Prelude.hashWithSalt` subnetChangeProtection
        `Prelude.hashWithSalt` subnetMappings
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    AwsNetworkFirewallFirewallDetails
  where
  rnf AwsNetworkFirewallFirewallDetails' {..} =
    Prelude.rnf deleteProtection `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf firewallArn `Prelude.seq`
          Prelude.rnf firewallId `Prelude.seq`
            Prelude.rnf firewallName `Prelude.seq`
              Prelude.rnf firewallPolicyArn `Prelude.seq`
                Prelude.rnf firewallPolicyChangeProtection `Prelude.seq`
                  Prelude.rnf subnetChangeProtection `Prelude.seq`
                    Prelude.rnf subnetMappings `Prelude.seq`
                      Prelude.rnf vpcId

instance
  Data.ToJSON
    AwsNetworkFirewallFirewallDetails
  where
  toJSON AwsNetworkFirewallFirewallDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeleteProtection" Data..=)
              Prelude.<$> deleteProtection,
            ("Description" Data..=) Prelude.<$> description,
            ("FirewallArn" Data..=) Prelude.<$> firewallArn,
            ("FirewallId" Data..=) Prelude.<$> firewallId,
            ("FirewallName" Data..=) Prelude.<$> firewallName,
            ("FirewallPolicyArn" Data..=)
              Prelude.<$> firewallPolicyArn,
            ("FirewallPolicyChangeProtection" Data..=)
              Prelude.<$> firewallPolicyChangeProtection,
            ("SubnetChangeProtection" Data..=)
              Prelude.<$> subnetChangeProtection,
            ("SubnetMappings" Data..=)
              Prelude.<$> subnetMappings,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
