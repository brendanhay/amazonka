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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceAttachment
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceIpV6AddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfacePrivateIpAddressDetail
import Amazonka.SecurityHub.Types.AwsEc2NetworkInterfaceSecurityGroup

-- | Details about the network interface
--
-- /See:/ 'newAwsEc2NetworkInterfaceDetails' smart constructor.
data AwsEc2NetworkInterfaceDetails = AwsEc2NetworkInterfaceDetails'
  { -- | The network interface attachment.
    attachment :: Prelude.Maybe AwsEc2NetworkInterfaceAttachment,
    -- | The IPv6 addresses associated with the network interface.
    ipV6Addresses :: Prelude.Maybe [AwsEc2NetworkInterfaceIpV6AddressDetail],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Prelude.Maybe [AwsEc2NetworkInterfacePrivateIpAddressDetail],
    -- | The public DNS name of the network interface.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The address of the Elastic IP address bound to the network interface.
    publicIp :: Prelude.Maybe Prelude.Text,
    -- | Security groups for the network interface.
    securityGroups :: Prelude.Maybe [AwsEc2NetworkInterfaceSecurityGroup],
    -- | Indicates whether traffic to or from the instance is validated.
    sourceDestCheck :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2NetworkInterfaceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'awsEc2NetworkInterfaceDetails_attachment' - The network interface attachment.
--
-- 'ipV6Addresses', 'awsEc2NetworkInterfaceDetails_ipV6Addresses' - The IPv6 addresses associated with the network interface.
--
-- 'networkInterfaceId', 'awsEc2NetworkInterfaceDetails_networkInterfaceId' - The ID of the network interface.
--
-- 'privateIpAddresses', 'awsEc2NetworkInterfaceDetails_privateIpAddresses' - The private IPv4 addresses associated with the network interface.
--
-- 'publicDnsName', 'awsEc2NetworkInterfaceDetails_publicDnsName' - The public DNS name of the network interface.
--
-- 'publicIp', 'awsEc2NetworkInterfaceDetails_publicIp' - The address of the Elastic IP address bound to the network interface.
--
-- 'securityGroups', 'awsEc2NetworkInterfaceDetails_securityGroups' - Security groups for the network interface.
--
-- 'sourceDestCheck', 'awsEc2NetworkInterfaceDetails_sourceDestCheck' - Indicates whether traffic to or from the instance is validated.
newAwsEc2NetworkInterfaceDetails ::
  AwsEc2NetworkInterfaceDetails
newAwsEc2NetworkInterfaceDetails =
  AwsEc2NetworkInterfaceDetails'
    { attachment =
        Prelude.Nothing,
      ipV6Addresses = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      publicIp = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing
    }

-- | The network interface attachment.
awsEc2NetworkInterfaceDetails_attachment :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe AwsEc2NetworkInterfaceAttachment)
awsEc2NetworkInterfaceDetails_attachment = Lens.lens (\AwsEc2NetworkInterfaceDetails' {attachment} -> attachment) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {attachment = a} :: AwsEc2NetworkInterfaceDetails)

-- | The IPv6 addresses associated with the network interface.
awsEc2NetworkInterfaceDetails_ipV6Addresses :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe [AwsEc2NetworkInterfaceIpV6AddressDetail])
awsEc2NetworkInterfaceDetails_ipV6Addresses = Lens.lens (\AwsEc2NetworkInterfaceDetails' {ipV6Addresses} -> ipV6Addresses) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {ipV6Addresses = a} :: AwsEc2NetworkInterfaceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
awsEc2NetworkInterfaceDetails_networkInterfaceId :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceDetails_networkInterfaceId = Lens.lens (\AwsEc2NetworkInterfaceDetails' {networkInterfaceId} -> networkInterfaceId) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {networkInterfaceId = a} :: AwsEc2NetworkInterfaceDetails)

-- | The private IPv4 addresses associated with the network interface.
awsEc2NetworkInterfaceDetails_privateIpAddresses :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe [AwsEc2NetworkInterfacePrivateIpAddressDetail])
awsEc2NetworkInterfaceDetails_privateIpAddresses = Lens.lens (\AwsEc2NetworkInterfaceDetails' {privateIpAddresses} -> privateIpAddresses) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {privateIpAddresses = a} :: AwsEc2NetworkInterfaceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The public DNS name of the network interface.
awsEc2NetworkInterfaceDetails_publicDnsName :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceDetails_publicDnsName = Lens.lens (\AwsEc2NetworkInterfaceDetails' {publicDnsName} -> publicDnsName) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {publicDnsName = a} :: AwsEc2NetworkInterfaceDetails)

-- | The address of the Elastic IP address bound to the network interface.
awsEc2NetworkInterfaceDetails_publicIp :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Text)
awsEc2NetworkInterfaceDetails_publicIp = Lens.lens (\AwsEc2NetworkInterfaceDetails' {publicIp} -> publicIp) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {publicIp = a} :: AwsEc2NetworkInterfaceDetails)

-- | Security groups for the network interface.
awsEc2NetworkInterfaceDetails_securityGroups :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe [AwsEc2NetworkInterfaceSecurityGroup])
awsEc2NetworkInterfaceDetails_securityGroups = Lens.lens (\AwsEc2NetworkInterfaceDetails' {securityGroups} -> securityGroups) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {securityGroups = a} :: AwsEc2NetworkInterfaceDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether traffic to or from the instance is validated.
awsEc2NetworkInterfaceDetails_sourceDestCheck :: Lens.Lens' AwsEc2NetworkInterfaceDetails (Prelude.Maybe Prelude.Bool)
awsEc2NetworkInterfaceDetails_sourceDestCheck = Lens.lens (\AwsEc2NetworkInterfaceDetails' {sourceDestCheck} -> sourceDestCheck) (\s@AwsEc2NetworkInterfaceDetails' {} a -> s {sourceDestCheck = a} :: AwsEc2NetworkInterfaceDetails)

instance Data.FromJSON AwsEc2NetworkInterfaceDetails where
  parseJSON =
    Data.withObject
      "AwsEc2NetworkInterfaceDetails"
      ( \x ->
          AwsEc2NetworkInterfaceDetails'
            Prelude.<$> (x Data..:? "Attachment")
            Prelude.<*> (x Data..:? "IpV6Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "NetworkInterfaceId")
            Prelude.<*> ( x Data..:? "PrivateIpAddresses"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PublicDnsName")
            Prelude.<*> (x Data..:? "PublicIp")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SourceDestCheck")
      )

instance
  Prelude.Hashable
    AwsEc2NetworkInterfaceDetails
  where
  hashWithSalt _salt AwsEc2NetworkInterfaceDetails' {..} =
    _salt `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` ipV6Addresses
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` publicDnsName
      `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` sourceDestCheck

instance Prelude.NFData AwsEc2NetworkInterfaceDetails where
  rnf AwsEc2NetworkInterfaceDetails' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf ipV6Addresses
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf publicDnsName
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf sourceDestCheck

instance Data.ToJSON AwsEc2NetworkInterfaceDetails where
  toJSON AwsEc2NetworkInterfaceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attachment" Data..=) Prelude.<$> attachment,
            ("IpV6Addresses" Data..=) Prelude.<$> ipV6Addresses,
            ("NetworkInterfaceId" Data..=)
              Prelude.<$> networkInterfaceId,
            ("PrivateIpAddresses" Data..=)
              Prelude.<$> privateIpAddresses,
            ("PublicDnsName" Data..=) Prelude.<$> publicDnsName,
            ("PublicIp" Data..=) Prelude.<$> publicIp,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            ("SourceDestCheck" Data..=)
              Prelude.<$> sourceDestCheck
          ]
      )
