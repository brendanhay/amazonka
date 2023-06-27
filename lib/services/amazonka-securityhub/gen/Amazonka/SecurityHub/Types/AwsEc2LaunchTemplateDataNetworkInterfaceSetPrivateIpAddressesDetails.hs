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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | One or more private IPv4 addresses.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' smart constructor.
data AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails = AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails'
  { -- | Indicates whether the private IPv4 address is the primary private IPv4
    -- address. Only one IPv4 address can be designated as primary.
    primary :: Prelude.Maybe Prelude.Bool,
    -- | The private IPv4 address.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primary', 'awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_primary' - Indicates whether the private IPv4 address is the primary private IPv4
-- address. Only one IPv4 address can be designated as primary.
--
-- 'privateIpAddress', 'awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_privateIpAddress' - The private IPv4 address.
newAwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails ::
  AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
newAwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails =
  AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails'
    { primary =
        Prelude.Nothing,
      privateIpAddress =
        Prelude.Nothing
    }

-- | Indicates whether the private IPv4 address is the primary private IPv4
-- address. Only one IPv4 address can be designated as primary.
awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_primary :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails (Prelude.Maybe Prelude.Bool)
awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_primary = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' {primary} -> primary) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' {} a -> s {primary = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails)

-- | The private IPv4 address.
awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_privateIpAddress :: Lens.Lens' AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails_privateIpAddress = Lens.lens (\AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' {privateIpAddress} -> privateIpAddress) (\s@AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' {} a -> s {privateIpAddress = a} :: AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails'
            Prelude.<$> (x Data..:? "Primary")
            Prelude.<*> (x Data..:? "PrivateIpAddress")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' {..} =
      _salt
        `Prelude.hashWithSalt` primary
        `Prelude.hashWithSalt` privateIpAddress

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
  where
  rnf
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' {..} =
      Prelude.rnf primary
        `Prelude.seq` Prelude.rnf privateIpAddress

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataNetworkInterfaceSetPrivateIpAddressesDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Primary" Data..=) Prelude.<$> primary,
              ("PrivateIpAddress" Data..=)
                Prelude.<$> privateIpAddress
            ]
        )
