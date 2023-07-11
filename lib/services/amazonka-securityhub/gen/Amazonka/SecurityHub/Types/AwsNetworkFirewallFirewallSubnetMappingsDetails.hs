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
-- Module      : Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallSubnetMappingsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsNetworkFirewallFirewallSubnetMappingsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A public subnet that Network Firewall uses for the firewall.
--
-- /See:/ 'newAwsNetworkFirewallFirewallSubnetMappingsDetails' smart constructor.
data AwsNetworkFirewallFirewallSubnetMappingsDetails = AwsNetworkFirewallFirewallSubnetMappingsDetails'
  { -- | The identifier of the subnet
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsNetworkFirewallFirewallSubnetMappingsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subnetId', 'awsNetworkFirewallFirewallSubnetMappingsDetails_subnetId' - The identifier of the subnet
newAwsNetworkFirewallFirewallSubnetMappingsDetails ::
  AwsNetworkFirewallFirewallSubnetMappingsDetails
newAwsNetworkFirewallFirewallSubnetMappingsDetails =
  AwsNetworkFirewallFirewallSubnetMappingsDetails'
    { subnetId =
        Prelude.Nothing
    }

-- | The identifier of the subnet
awsNetworkFirewallFirewallSubnetMappingsDetails_subnetId :: Lens.Lens' AwsNetworkFirewallFirewallSubnetMappingsDetails (Prelude.Maybe Prelude.Text)
awsNetworkFirewallFirewallSubnetMappingsDetails_subnetId = Lens.lens (\AwsNetworkFirewallFirewallSubnetMappingsDetails' {subnetId} -> subnetId) (\s@AwsNetworkFirewallFirewallSubnetMappingsDetails' {} a -> s {subnetId = a} :: AwsNetworkFirewallFirewallSubnetMappingsDetails)

instance
  Data.FromJSON
    AwsNetworkFirewallFirewallSubnetMappingsDetails
  where
  parseJSON =
    Data.withObject
      "AwsNetworkFirewallFirewallSubnetMappingsDetails"
      ( \x ->
          AwsNetworkFirewallFirewallSubnetMappingsDetails'
            Prelude.<$> (x Data..:? "SubnetId")
      )

instance
  Prelude.Hashable
    AwsNetworkFirewallFirewallSubnetMappingsDetails
  where
  hashWithSalt
    _salt
    AwsNetworkFirewallFirewallSubnetMappingsDetails' {..} =
      _salt `Prelude.hashWithSalt` subnetId

instance
  Prelude.NFData
    AwsNetworkFirewallFirewallSubnetMappingsDetails
  where
  rnf
    AwsNetworkFirewallFirewallSubnetMappingsDetails' {..} =
      Prelude.rnf subnetId

instance
  Data.ToJSON
    AwsNetworkFirewallFirewallSubnetMappingsDetails
  where
  toJSON
    AwsNetworkFirewallFirewallSubnetMappingsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("SubnetId" Data..=) Prelude.<$> subnetId]
        )
