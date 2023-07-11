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
-- Module      : Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallPolicyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallPolicyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.FirewallPolicyDetails

-- | Details about a firewall policy. A firewall policy defines the behavior
-- of a network firewall.
--
-- /See:/ 'newAwsNetworkFirewallFirewallPolicyDetails' smart constructor.
data AwsNetworkFirewallFirewallPolicyDetails = AwsNetworkFirewallFirewallPolicyDetails'
  { -- | A description of the firewall policy.
    description :: Prelude.Maybe Prelude.Text,
    -- | The firewall policy configuration.
    firewallPolicy :: Prelude.Maybe FirewallPolicyDetails,
    -- | The ARN of the firewall policy.
    firewallPolicyArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the firewall policy.
    firewallPolicyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the firewall policy.
    firewallPolicyName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsNetworkFirewallFirewallPolicyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'awsNetworkFirewallFirewallPolicyDetails_description' - A description of the firewall policy.
--
-- 'firewallPolicy', 'awsNetworkFirewallFirewallPolicyDetails_firewallPolicy' - The firewall policy configuration.
--
-- 'firewallPolicyArn', 'awsNetworkFirewallFirewallPolicyDetails_firewallPolicyArn' - The ARN of the firewall policy.
--
-- 'firewallPolicyId', 'awsNetworkFirewallFirewallPolicyDetails_firewallPolicyId' - The identifier of the firewall policy.
--
-- 'firewallPolicyName', 'awsNetworkFirewallFirewallPolicyDetails_firewallPolicyName' - The name of the firewall policy.
newAwsNetworkFirewallFirewallPolicyDetails ::
  AwsNetworkFirewallFirewallPolicyDetails
newAwsNetworkFirewallFirewallPolicyDetails =
  AwsNetworkFirewallFirewallPolicyDetails'
    { description =
        Prelude.Nothing,
      firewallPolicy = Prelude.Nothing,
      firewallPolicyArn =
        Prelude.Nothing,
      firewallPolicyId = Prelude.Nothing,
      firewallPolicyName =
        Prelude.Nothing
    }

-- | A description of the firewall policy.
awsNetworkFirewallFirewallPolicyDetails_description :: Lens.Lens' AwsNetworkFirewallFirewallPolicyDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallPolicyDetails_description = Lens.lens (\AwsNetworkFirewallFirewallPolicyDetails' {description} -> description) (\s@AwsNetworkFirewallFirewallPolicyDetails' {} a -> s {description = a} :: AwsNetworkFirewallFirewallPolicyDetails)

-- | The firewall policy configuration.
awsNetworkFirewallFirewallPolicyDetails_firewallPolicy :: Lens.Lens' AwsNetworkFirewallFirewallPolicyDetails (Prelude.Maybe FirewallPolicyDetails)
awsNetworkFirewallFirewallPolicyDetails_firewallPolicy = Lens.lens (\AwsNetworkFirewallFirewallPolicyDetails' {firewallPolicy} -> firewallPolicy) (\s@AwsNetworkFirewallFirewallPolicyDetails' {} a -> s {firewallPolicy = a} :: AwsNetworkFirewallFirewallPolicyDetails)

-- | The ARN of the firewall policy.
awsNetworkFirewallFirewallPolicyDetails_firewallPolicyArn :: Lens.Lens' AwsNetworkFirewallFirewallPolicyDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallPolicyDetails_firewallPolicyArn = Lens.lens (\AwsNetworkFirewallFirewallPolicyDetails' {firewallPolicyArn} -> firewallPolicyArn) (\s@AwsNetworkFirewallFirewallPolicyDetails' {} a -> s {firewallPolicyArn = a} :: AwsNetworkFirewallFirewallPolicyDetails)

-- | The identifier of the firewall policy.
awsNetworkFirewallFirewallPolicyDetails_firewallPolicyId :: Lens.Lens' AwsNetworkFirewallFirewallPolicyDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallPolicyDetails_firewallPolicyId = Lens.lens (\AwsNetworkFirewallFirewallPolicyDetails' {firewallPolicyId} -> firewallPolicyId) (\s@AwsNetworkFirewallFirewallPolicyDetails' {} a -> s {firewallPolicyId = a} :: AwsNetworkFirewallFirewallPolicyDetails)

-- | The name of the firewall policy.
awsNetworkFirewallFirewallPolicyDetails_firewallPolicyName :: Lens.Lens' AwsNetworkFirewallFirewallPolicyDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallPolicyDetails_firewallPolicyName = Lens.lens (\AwsNetworkFirewallFirewallPolicyDetails' {firewallPolicyName} -> firewallPolicyName) (\s@AwsNetworkFirewallFirewallPolicyDetails' {} a -> s {firewallPolicyName = a} :: AwsNetworkFirewallFirewallPolicyDetails)

instance
  Data.FromJSON
    AwsNetworkFirewallFirewallPolicyDetails
  where
  parseJSON =
    Data.withObject
      "AwsNetworkFirewallFirewallPolicyDetails"
      ( \x ->
          AwsNetworkFirewallFirewallPolicyDetails'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FirewallPolicy")
            Prelude.<*> (x Data..:? "FirewallPolicyArn")
            Prelude.<*> (x Data..:? "FirewallPolicyId")
            Prelude.<*> (x Data..:? "FirewallPolicyName")
      )

instance
  Prelude.Hashable
    AwsNetworkFirewallFirewallPolicyDetails
  where
  hashWithSalt
    _salt
    AwsNetworkFirewallFirewallPolicyDetails' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` firewallPolicy
        `Prelude.hashWithSalt` firewallPolicyArn
        `Prelude.hashWithSalt` firewallPolicyId
        `Prelude.hashWithSalt` firewallPolicyName

instance
  Prelude.NFData
    AwsNetworkFirewallFirewallPolicyDetails
  where
  rnf AwsNetworkFirewallFirewallPolicyDetails' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf firewallPolicy
      `Prelude.seq` Prelude.rnf firewallPolicyArn
      `Prelude.seq` Prelude.rnf firewallPolicyId
      `Prelude.seq` Prelude.rnf firewallPolicyName

instance
  Data.ToJSON
    AwsNetworkFirewallFirewallPolicyDetails
  where
  toJSON AwsNetworkFirewallFirewallPolicyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("FirewallPolicy" Data..=)
              Prelude.<$> firewallPolicy,
            ("FirewallPolicyArn" Data..=)
              Prelude.<$> firewallPolicyArn,
            ("FirewallPolicyId" Data..=)
              Prelude.<$> firewallPolicyId,
            ("FirewallPolicyName" Data..=)
              Prelude.<$> firewallPolicyName
          ]
      )
