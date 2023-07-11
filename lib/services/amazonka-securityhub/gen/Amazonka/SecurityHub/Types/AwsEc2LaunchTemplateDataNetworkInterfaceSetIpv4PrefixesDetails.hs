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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details on one or more IPv4 prefixes for a network interface.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails' smart constructor.
data AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails = AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails'
  { -- | The IPv4 prefix. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    ipv4Prefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipv4Prefix', 'awsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails_ipv4Prefix' - The IPv4 prefix. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails ::
  AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
newAwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails =
  AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails'
    { ipv4Prefix =
        Prelude.Nothing
    }

-- | The IPv4 prefix. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-prefix-eni.html Assigning prefixes to Amazon EC2 network interfaces>
-- in the /Amazon Elastic Compute Cloud User Guide/.
awsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails_ipv4Prefix :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails_ipv4Prefix = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails' {ipv4Prefix} -> ipv4Prefix) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails' {} a -> s {ipv4Prefix = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails'
            Prelude.<$> (x Data..:? "Ipv4Prefix")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails' {..} =
      _salt `Prelude.hashWithSalt` ipv4Prefix

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
  where
  rnf
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails' {..} =
      Prelude.rnf ipv4Prefix

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetIpv4PrefixesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [("Ipv4Prefix" Data..=) Prelude.<$> ipv4Prefix]
        )
