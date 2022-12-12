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
-- Module      : Amazonka.GlobalAccelerator.Types.Accelerator
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.Accelerator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GlobalAccelerator.Types.AcceleratorEvent
import Amazonka.GlobalAccelerator.Types.AcceleratorStatus
import Amazonka.GlobalAccelerator.Types.IpAddressType
import Amazonka.GlobalAccelerator.Types.IpSet
import qualified Amazonka.Prelude as Prelude

-- | An accelerator is a complex type that includes one or more listeners
-- that process inbound connections and then direct traffic to one or more
-- endpoint groups, each of which includes endpoints, such as load
-- balancers.
--
-- /See:/ 'newAccelerator' smart constructor.
data Accelerator = Accelerator'
  { -- | The Amazon Resource Name (ARN) of the accelerator.
    acceleratorArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the accelerator was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The Domain Name System (DNS) name that Global Accelerator creates that
    -- points to an accelerator\'s static IPv4 addresses.
    --
    -- The naming convention for the DNS name for an accelerator is the
    -- following: A lowercase letter a, followed by a 16-bit random hex string,
    -- followed by .awsglobalaccelerator.com. For example:
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
    -- | The Domain Name System (DNS) name that Global Accelerator creates that
    -- points to a dual-stack accelerator\'s four static IP addresses: two IPv4
    -- addresses and two IPv6 addresses.
    --
    -- The naming convention for the dual-stack DNS name is the following: A
    -- lowercase letter a, followed by a 16-bit random hex string, followed by
    -- .dualstack.awsglobalaccelerator.com. For example:
    -- a1234567890abcdef.dualstack.awsglobalaccelerator.com.
    --
    -- Note: Global Accelerator also assigns a default DNS name, @DnsName@, to
    -- your accelerator that points just to the static IPv4 addresses.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-accelerators.html#about-accelerators.dns-addressing Support for DNS addressing in Global Accelerator>
    -- in the /Global Accelerator Developer Guide/.
    dualStackDnsName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the accelerator is enabled. The value is true or
    -- false. The default value is true.
    --
    -- If the value is set to true, the accelerator cannot be deleted. If set
    -- to false, accelerator can be deleted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A history of changes that you make to an accelerator in Global
    -- Accelerator.
    events :: Prelude.Maybe [AcceleratorEvent],
    -- | The IP address type that an accelerator supports. For a standard
    -- accelerator, the value can be IPV4 or DUAL_STACK.
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
    status :: Prelude.Maybe AcceleratorStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Accelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorArn', 'accelerator_acceleratorArn' - The Amazon Resource Name (ARN) of the accelerator.
--
-- 'createdTime', 'accelerator_createdTime' - The date and time that the accelerator was created.
--
-- 'dnsName', 'accelerator_dnsName' - The Domain Name System (DNS) name that Global Accelerator creates that
-- points to an accelerator\'s static IPv4 addresses.
--
-- The naming convention for the DNS name for an accelerator is the
-- following: A lowercase letter a, followed by a 16-bit random hex string,
-- followed by .awsglobalaccelerator.com. For example:
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
-- 'dualStackDnsName', 'accelerator_dualStackDnsName' - The Domain Name System (DNS) name that Global Accelerator creates that
-- points to a dual-stack accelerator\'s four static IP addresses: two IPv4
-- addresses and two IPv6 addresses.
--
-- The naming convention for the dual-stack DNS name is the following: A
-- lowercase letter a, followed by a 16-bit random hex string, followed by
-- .dualstack.awsglobalaccelerator.com. For example:
-- a1234567890abcdef.dualstack.awsglobalaccelerator.com.
--
-- Note: Global Accelerator also assigns a default DNS name, @DnsName@, to
-- your accelerator that points just to the static IPv4 addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-accelerators.html#about-accelerators.dns-addressing Support for DNS addressing in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
--
-- 'enabled', 'accelerator_enabled' - Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
--
-- 'events', 'accelerator_events' - A history of changes that you make to an accelerator in Global
-- Accelerator.
--
-- 'ipAddressType', 'accelerator_ipAddressType' - The IP address type that an accelerator supports. For a standard
-- accelerator, the value can be IPV4 or DUAL_STACK.
--
-- 'ipSets', 'accelerator_ipSets' - The static IP addresses that Global Accelerator associates with the
-- accelerator.
--
-- 'lastModifiedTime', 'accelerator_lastModifiedTime' - The date and time that the accelerator was last modified.
--
-- 'name', 'accelerator_name' - The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
--
-- 'status', 'accelerator_status' - Describes the deployment status of the accelerator.
newAccelerator ::
  Accelerator
newAccelerator =
  Accelerator'
    { acceleratorArn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      dualStackDnsName = Prelude.Nothing,
      enabled = Prelude.Nothing,
      events = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      ipSets = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the accelerator.
accelerator_acceleratorArn :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Text)
accelerator_acceleratorArn = Lens.lens (\Accelerator' {acceleratorArn} -> acceleratorArn) (\s@Accelerator' {} a -> s {acceleratorArn = a} :: Accelerator)

-- | The date and time that the accelerator was created.
accelerator_createdTime :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.UTCTime)
accelerator_createdTime = Lens.lens (\Accelerator' {createdTime} -> createdTime) (\s@Accelerator' {} a -> s {createdTime = a} :: Accelerator) Prelude.. Lens.mapping Data._Time

-- | The Domain Name System (DNS) name that Global Accelerator creates that
-- points to an accelerator\'s static IPv4 addresses.
--
-- The naming convention for the DNS name for an accelerator is the
-- following: A lowercase letter a, followed by a 16-bit random hex string,
-- followed by .awsglobalaccelerator.com. For example:
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
accelerator_dnsName :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Text)
accelerator_dnsName = Lens.lens (\Accelerator' {dnsName} -> dnsName) (\s@Accelerator' {} a -> s {dnsName = a} :: Accelerator)

-- | The Domain Name System (DNS) name that Global Accelerator creates that
-- points to a dual-stack accelerator\'s four static IP addresses: two IPv4
-- addresses and two IPv6 addresses.
--
-- The naming convention for the dual-stack DNS name is the following: A
-- lowercase letter a, followed by a 16-bit random hex string, followed by
-- .dualstack.awsglobalaccelerator.com. For example:
-- a1234567890abcdef.dualstack.awsglobalaccelerator.com.
--
-- Note: Global Accelerator also assigns a default DNS name, @DnsName@, to
-- your accelerator that points just to the static IPv4 addresses.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-accelerators.html#about-accelerators.dns-addressing Support for DNS addressing in Global Accelerator>
-- in the /Global Accelerator Developer Guide/.
accelerator_dualStackDnsName :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Text)
accelerator_dualStackDnsName = Lens.lens (\Accelerator' {dualStackDnsName} -> dualStackDnsName) (\s@Accelerator' {} a -> s {dualStackDnsName = a} :: Accelerator)

-- | Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
accelerator_enabled :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Bool)
accelerator_enabled = Lens.lens (\Accelerator' {enabled} -> enabled) (\s@Accelerator' {} a -> s {enabled = a} :: Accelerator)

-- | A history of changes that you make to an accelerator in Global
-- Accelerator.
accelerator_events :: Lens.Lens' Accelerator (Prelude.Maybe [AcceleratorEvent])
accelerator_events = Lens.lens (\Accelerator' {events} -> events) (\s@Accelerator' {} a -> s {events = a} :: Accelerator) Prelude.. Lens.mapping Lens.coerced

-- | The IP address type that an accelerator supports. For a standard
-- accelerator, the value can be IPV4 or DUAL_STACK.
accelerator_ipAddressType :: Lens.Lens' Accelerator (Prelude.Maybe IpAddressType)
accelerator_ipAddressType = Lens.lens (\Accelerator' {ipAddressType} -> ipAddressType) (\s@Accelerator' {} a -> s {ipAddressType = a} :: Accelerator)

-- | The static IP addresses that Global Accelerator associates with the
-- accelerator.
accelerator_ipSets :: Lens.Lens' Accelerator (Prelude.Maybe [IpSet])
accelerator_ipSets = Lens.lens (\Accelerator' {ipSets} -> ipSets) (\s@Accelerator' {} a -> s {ipSets = a} :: Accelerator) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the accelerator was last modified.
accelerator_lastModifiedTime :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.UTCTime)
accelerator_lastModifiedTime = Lens.lens (\Accelerator' {lastModifiedTime} -> lastModifiedTime) (\s@Accelerator' {} a -> s {lastModifiedTime = a} :: Accelerator) Prelude.. Lens.mapping Data._Time

-- | The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
accelerator_name :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Text)
accelerator_name = Lens.lens (\Accelerator' {name} -> name) (\s@Accelerator' {} a -> s {name = a} :: Accelerator)

-- | Describes the deployment status of the accelerator.
accelerator_status :: Lens.Lens' Accelerator (Prelude.Maybe AcceleratorStatus)
accelerator_status = Lens.lens (\Accelerator' {status} -> status) (\s@Accelerator' {} a -> s {status = a} :: Accelerator)

instance Data.FromJSON Accelerator where
  parseJSON =
    Data.withObject
      "Accelerator"
      ( \x ->
          Accelerator'
            Prelude.<$> (x Data..:? "AcceleratorArn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DnsName")
            Prelude.<*> (x Data..:? "DualStackDnsName")
            Prelude.<*> (x Data..:? "Enabled")
            Prelude.<*> (x Data..:? "Events" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IpAddressType")
            Prelude.<*> (x Data..:? "IpSets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Accelerator where
  hashWithSalt _salt Accelerator' {..} =
    _salt `Prelude.hashWithSalt` acceleratorArn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` dualStackDnsName
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` events
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` ipSets
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData Accelerator where
  rnf Accelerator' {..} =
    Prelude.rnf acceleratorArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf dualStackDnsName
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf events
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf ipSets
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
