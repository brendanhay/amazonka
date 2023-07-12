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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies an IPv6 address in an Amazon EC2 launch template.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails' smart constructor.
data AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails = AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails'
  { -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
    -- your subnet.
    ipv6Address :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Address', 'awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails_ipv6Address' - One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet.
newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails ::
  AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails =
  AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails'
    { ipv6Address =
        Prelude.Nothing
    }

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of
-- your subnet.
awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails_ipv6Address :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails_ipv6Address = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails' {ipv6Address} -> ipv6Address) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails' {} a -> s {ipv6Address = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails'
            Prelude.<$> (x Data..:? "Ipv6Address")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails' {..} =
      _salt `Prelude.hashWithSalt` ipv6Address

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
  where
  rnf
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails' {..} =
      Prelude.rnf ipv6Address

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6AddressesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Ipv6Address" Data..=) Prelude.<$> ipv6Address]
        )
