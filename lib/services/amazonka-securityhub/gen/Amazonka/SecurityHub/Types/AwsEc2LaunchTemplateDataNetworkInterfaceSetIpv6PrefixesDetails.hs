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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details on one or more IPv6 prefixes to be assigned to the
-- network interface.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails' smart constructor.
data AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails = AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails'
  { -- | The IPv6 prefix.
    ipv6Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv6Prefix', 'awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails_ipv6Prefix' - The IPv6 prefix.
newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails ::
  AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails =
  AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails'
    { ipv6Prefix =
        Prelude.Nothing
    }

-- | The IPv6 prefix.
awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails_ipv6Prefix :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails_ipv6Prefix = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails' {ipv6Prefix} -> ipv6Prefix) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails' {} a -> s {ipv6Prefix = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails'
            Prelude.<$> (x Data..:? "Ipv6Prefix")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails' {..} =
      _salt `Prelude.hashWithSalt` ipv6Prefix

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
  where
  rnf
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails' {..} =
      Prelude.rnf ipv6Prefix

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv6PrefixesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Ipv6Prefix" Data..=) Prelude.<$> ipv6Prefix]
        )
