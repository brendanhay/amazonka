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
-- Module      : Amazonka.GlobalAccelerator.Types.CustomRoutingAccelerator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.CustomRoutingAccelerator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.CustomRoutingAcceleratorStatus
import Amazonka.GlobalAccelerator.Types.IpAddressType
import Amazonka.GlobalAccelerator.Types.IpSet
import qualified Amazonka.Prelude as Prelude

-- | Attributes of a custom routing accelerator.
--
-- /See:/ 'newCustomRoutingAccelerator' smart constructor.
data CustomRoutingAccelerator = CustomRoutingAccelerator'
  { -- | The Amazon Resource Name (ARN) of the custom routing accelerator.
    acceleratorArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the accelerator was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The Domain Name System (DNS) name that Global Accelerator creates that
    -- points to an accelerator\'s static IPv4 addresses.
    --
    -- The naming convention for the DNS name is the following: A lowercase
    -- letter a, followed by a 16-bit random hex string, followed by
    -- .awsglobalaccelerator.com. For example:
    -- a1234567890abcdef.awsglobalaccelerator.com.
    --
    -- If you have a dual-stack accelerator, you also have a second DNS name,
    -- @DualStackDnsName@, that points to both the A record and the AAAA record
    -- for all four static addresses for the accelerator: two IPv4 addresses
    -- and two IPv6 addresses.
    --
    -- For more information about the default DNS name, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/dns-addressing-custom-domains.dns-addressing.html Support for DNS addressing in Global Accelerator>
    -- in the /Global Accelerator Developer Guide/.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the accelerator is enabled. The value is true or
    -- false. The default value is true.
    --
    -- If the value is set to true, the accelerator cannot be deleted. If set
    -- to false, accelerator can be deleted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The IP address type that an accelerator supports. For a custom routing
    -- accelerator, the value must be IPV4.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The static IP addresses that Global Accelerator associates with the
    -- accelerator.
    ipSets :: Prelude.Maybe [IpSet],
    -- | The date and time that the accelerator was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the accelerator. The name must contain only alphanumeric
    -- characters or hyphens (-), and must not begin or end with a hyphen.
    name :: Prelude.Maybe Prelude.Text,
    -- | Describes the deployment status of the accelerator.
    status :: Prelude.Maybe CustomRoutingAcceleratorStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomRoutingAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'customRoutingAccelerator_acceleratorArn' - The Amazon Resource Name (ARN) of the custom routing accelerator.
--
-- 'createdTime', 'customRoutingAccelerator_createdTime' - The date and time that the accelerator was created.
--
-- 'dnsName', 'customRoutingAccelerator_dnsName' - The Domain Name System (DNS) name that Global Accelerator creates that
-- points to an accelerator\'s static IPv4 addresses.
--
-- The naming convention for the DNS name is the following: A lowercase
-- letter a, followed by a 16-bit random hex string, followed by
-- .awsglobalaccelerator.com. For example:
-- a1234567890abcdef.awsglobalaccelerator.com.
--
-- If you have a dual-stack accelerator, you also have a second DNS name,
-- @DualStackDnsName@, that points to both the A record and the AAAA record
-- for all four static addresses for the accelerator: two IPv4 addresses
-- and two IPv6 addresses.
--
-- For more information about the default DNS name, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/dns-addressing-custom-domains.dns-addressing.html Support for DNS addressing in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
--
-- 'enabled', 'customRoutingAccelerator_enabled' - Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
--
-- 'ipAddressType', 'customRoutingAccelerator_ipAddressType' - The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
--
-- 'ipSets', 'customRoutingAccelerator_ipSets' - The static IP addresses that Global Accelerator associates with the
-- accelerator.
--
-- 'lastModifiedTime', 'customRoutingAccelerator_lastModifiedTime' - The date and time that the accelerator was last modified.
--
-- 'name', 'customRoutingAccelerator_name' - The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
--
-- 'status', 'customRoutingAccelerator_status' - Describes the deployment status of the accelerator.
newCustomRoutingAccelerator ::
  CustomRoutingAccelerator
newCustomRoutingAccelerator =
  CustomRoutingAccelerator'
    { acceleratorArn =
        Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      enabled = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      ipSets = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom routing accelerator.
customRoutingAccelerator_acceleratorArn :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Text)
customRoutingAccelerator_acceleratorArn = Lens.lens (\CustomRoutingAccelerator' {acceleratorArn} -> acceleratorArn) (\s@CustomRoutingAccelerator' {} a -> s {acceleratorArn = a} :: CustomRoutingAccelerator)

-- | The date and time that the accelerator was created.
customRoutingAccelerator_createdTime :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.UTCTime)
customRoutingAccelerator_createdTime = Lens.lens (\CustomRoutingAccelerator' {createdTime} -> createdTime) (\s@CustomRoutingAccelerator' {} a -> s {createdTime = a} :: CustomRoutingAccelerator) Prelude.. Lens.mapping Data._Time

-- | The Domain Name System (DNS) name that Global Accelerator creates that
-- points to an accelerator\'s static IPv4 addresses.
--
-- The naming convention for the DNS name is the following: A lowercase
-- letter a, followed by a 16-bit random hex string, followed by
-- .awsglobalaccelerator.com. For example:
-- a1234567890abcdef.awsglobalaccelerator.com.
--
-- If you have a dual-stack accelerator, you also have a second DNS name,
-- @DualStackDnsName@, that points to both the A record and the AAAA record
-- for all four static addresses for the accelerator: two IPv4 addresses
-- and two IPv6 addresses.
--
-- For more information about the default DNS name, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/dns-addressing-custom-domains.dns-addressing.html Support for DNS addressing in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
customRoutingAccelerator_dnsName :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Text)
customRoutingAccelerator_dnsName = Lens.lens (\CustomRoutingAccelerator' {dnsName} -> dnsName) (\s@CustomRoutingAccelerator' {} a -> s {dnsName = a} :: CustomRoutingAccelerator)

-- | Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
customRoutingAccelerator_enabled :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Bool)
customRoutingAccelerator_enabled = Lens.lens (\CustomRoutingAccelerator' {enabled} -> enabled) (\s@CustomRoutingAccelerator' {} a -> s {enabled = a} :: CustomRoutingAccelerator)

-- | The IP address type that an accelerator supports. For a custom routing
-- accelerator, the value must be IPV4.
customRoutingAccelerator_ipAddressType :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe IpAddressType)
customRoutingAccelerator_ipAddressType = Lens.lens (\CustomRoutingAccelerator' {ipAddressType} -> ipAddressType) (\s@CustomRoutingAccelerator' {} a -> s {ipAddressType = a} :: CustomRoutingAccelerator)

-- | The static IP addresses that Global Accelerator associates with the
-- accelerator.
customRoutingAccelerator_ipSets :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe [IpSet])
customRoutingAccelerator_ipSets = Lens.lens (\CustomRoutingAccelerator' {ipSets} -> ipSets) (\s@CustomRoutingAccelerator' {} a -> s {ipSets = a} :: CustomRoutingAccelerator) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the accelerator was last modified.
customRoutingAccelerator_lastModifiedTime :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.UTCTime)
customRoutingAccelerator_lastModifiedTime = Lens.lens (\CustomRoutingAccelerator' {lastModifiedTime} -> lastModifiedTime) (\s@CustomRoutingAccelerator' {} a -> s {lastModifiedTime = a} :: CustomRoutingAccelerator) Prelude.. Lens.mapping Data._Time

-- | The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
customRoutingAccelerator_name :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Text)
customRoutingAccelerator_name = Lens.lens (\CustomRoutingAccelerator' {name} -> name) (\s@CustomRoutingAccelerator' {} a -> s {name = a} :: CustomRoutingAccelerator)

-- | Describes the deployment status of the accelerator.
customRoutingAccelerator_status :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe CustomRoutingAcceleratorStatus)
customRoutingAccelerator_status = Lens.lens (\CustomRoutingAccelerator' {status} -> status) (\s@CustomRoutingAccelerator' {} a -> s {status = a} :: CustomRoutingAccelerator)

instance Data.FromJSON CustomRoutingAccelerator where
  parseJSON =
    Data.withObject
      "CustomRoutingAccelerator"
      ( \x ->
          CustomRoutingAccelerator'
            Prelude.<$> (x Data..:? "AcceleratorArn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "IpAddressType")
            Prelude.<*> (x Data..:? "IpSets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable CustomRoutingAccelerator where
  hashWithSalt _salt CustomRoutingAccelerator' {..} =
    _salt `Prelude.hashWithSalt` acceleratorArn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` ipSets
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData CustomRoutingAccelerator where
  rnf CustomRoutingAccelerator' {..} =
    Prelude.rnf acceleratorArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf ipSets
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
