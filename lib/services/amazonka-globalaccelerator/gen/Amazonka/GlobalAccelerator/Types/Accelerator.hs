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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.Accelerator where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types.AcceleratorStatus
import Amazonka.GlobalAccelerator.Types.IpAddressType
import Amazonka.GlobalAccelerator.Types.IpSet
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An accelerator is a complex type that includes one or more listeners
-- that process inbound connections and then direct traffic to one or more
-- endpoint groups, each of which includes endpoints, such as load
-- balancers.
--
-- /See:/ 'newAccelerator' smart constructor.
data Accelerator = Accelerator'
  { -- | Describes the deployment status of the accelerator.
    status :: Prelude.Maybe AcceleratorStatus,
    -- | The Amazon Resource Name (ARN) of the accelerator.
    acceleratorArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the accelerator is enabled. The value is true or
    -- false. The default value is true.
    --
    -- If the value is set to true, the accelerator cannot be deleted. If set
    -- to false, accelerator can be deleted.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The date and time that the accelerator was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time that the accelerator was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The value for the address type must be IPv4.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The name of the accelerator. The name must contain only alphanumeric
    -- characters or hyphens (-), and must not begin or end with a hyphen.
    name :: Prelude.Maybe Prelude.Text,
    -- | The static IP addresses that Global Accelerator associates with the
    -- accelerator.
    ipSets :: Prelude.Maybe [IpSet],
    -- | The Domain Name System (DNS) name that Global Accelerator creates that
    -- points to your accelerator\'s static IP addresses.
    --
    -- The naming convention for the DNS name is the following: A lowercase
    -- letter a, followed by a 16-bit random hex string, followed by
    -- .awsglobalaccelerator.com. For example:
    -- a1234567890abcdef.awsglobalaccelerator.com.
    --
    -- For more information about the default DNS name, see
    -- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-accelerators.html#about-accelerators.dns-addressing Support for DNS Addressing in Global Accelerator>
    -- in the /AWS Global Accelerator Developer Guide/.
    dnsName :: Prelude.Maybe Prelude.Text
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
-- 'status', 'accelerator_status' - Describes the deployment status of the accelerator.
--
-- 'acceleratorArn', 'accelerator_acceleratorArn' - The Amazon Resource Name (ARN) of the accelerator.
--
-- 'enabled', 'accelerator_enabled' - Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
--
-- 'createdTime', 'accelerator_createdTime' - The date and time that the accelerator was created.
--
-- 'lastModifiedTime', 'accelerator_lastModifiedTime' - The date and time that the accelerator was last modified.
--
-- 'ipAddressType', 'accelerator_ipAddressType' - The value for the address type must be IPv4.
--
-- 'name', 'accelerator_name' - The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
--
-- 'ipSets', 'accelerator_ipSets' - The static IP addresses that Global Accelerator associates with the
-- accelerator.
--
-- 'dnsName', 'accelerator_dnsName' - The Domain Name System (DNS) name that Global Accelerator creates that
-- points to your accelerator\'s static IP addresses.
--
-- The naming convention for the DNS name is the following: A lowercase
-- letter a, followed by a 16-bit random hex string, followed by
-- .awsglobalaccelerator.com. For example:
-- a1234567890abcdef.awsglobalaccelerator.com.
--
-- For more information about the default DNS name, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-accelerators.html#about-accelerators.dns-addressing Support for DNS Addressing in Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
newAccelerator ::
  Accelerator
newAccelerator =
  Accelerator'
    { status = Prelude.Nothing,
      acceleratorArn = Prelude.Nothing,
      enabled = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      name = Prelude.Nothing,
      ipSets = Prelude.Nothing,
      dnsName = Prelude.Nothing
    }

-- | Describes the deployment status of the accelerator.
accelerator_status :: Lens.Lens' Accelerator (Prelude.Maybe AcceleratorStatus)
accelerator_status = Lens.lens (\Accelerator' {status} -> status) (\s@Accelerator' {} a -> s {status = a} :: Accelerator)

-- | The Amazon Resource Name (ARN) of the accelerator.
accelerator_acceleratorArn :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Text)
accelerator_acceleratorArn = Lens.lens (\Accelerator' {acceleratorArn} -> acceleratorArn) (\s@Accelerator' {} a -> s {acceleratorArn = a} :: Accelerator)

-- | Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
accelerator_enabled :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Bool)
accelerator_enabled = Lens.lens (\Accelerator' {enabled} -> enabled) (\s@Accelerator' {} a -> s {enabled = a} :: Accelerator)

-- | The date and time that the accelerator was created.
accelerator_createdTime :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.UTCTime)
accelerator_createdTime = Lens.lens (\Accelerator' {createdTime} -> createdTime) (\s@Accelerator' {} a -> s {createdTime = a} :: Accelerator) Prelude.. Lens.mapping Core._Time

-- | The date and time that the accelerator was last modified.
accelerator_lastModifiedTime :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.UTCTime)
accelerator_lastModifiedTime = Lens.lens (\Accelerator' {lastModifiedTime} -> lastModifiedTime) (\s@Accelerator' {} a -> s {lastModifiedTime = a} :: Accelerator) Prelude.. Lens.mapping Core._Time

-- | The value for the address type must be IPv4.
accelerator_ipAddressType :: Lens.Lens' Accelerator (Prelude.Maybe IpAddressType)
accelerator_ipAddressType = Lens.lens (\Accelerator' {ipAddressType} -> ipAddressType) (\s@Accelerator' {} a -> s {ipAddressType = a} :: Accelerator)

-- | The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
accelerator_name :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Text)
accelerator_name = Lens.lens (\Accelerator' {name} -> name) (\s@Accelerator' {} a -> s {name = a} :: Accelerator)

-- | The static IP addresses that Global Accelerator associates with the
-- accelerator.
accelerator_ipSets :: Lens.Lens' Accelerator (Prelude.Maybe [IpSet])
accelerator_ipSets = Lens.lens (\Accelerator' {ipSets} -> ipSets) (\s@Accelerator' {} a -> s {ipSets = a} :: Accelerator) Prelude.. Lens.mapping Lens.coerced

-- | The Domain Name System (DNS) name that Global Accelerator creates that
-- points to your accelerator\'s static IP addresses.
--
-- The naming convention for the DNS name is the following: A lowercase
-- letter a, followed by a 16-bit random hex string, followed by
-- .awsglobalaccelerator.com. For example:
-- a1234567890abcdef.awsglobalaccelerator.com.
--
-- For more information about the default DNS name, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-accelerators.html#about-accelerators.dns-addressing Support for DNS Addressing in Global Accelerator>
-- in the /AWS Global Accelerator Developer Guide/.
accelerator_dnsName :: Lens.Lens' Accelerator (Prelude.Maybe Prelude.Text)
accelerator_dnsName = Lens.lens (\Accelerator' {dnsName} -> dnsName) (\s@Accelerator' {} a -> s {dnsName = a} :: Accelerator)

instance Core.FromJSON Accelerator where
  parseJSON =
    Core.withObject
      "Accelerator"
      ( \x ->
          Accelerator'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "AcceleratorArn")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "IpAddressType")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "IpSets" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DnsName")
      )

instance Prelude.Hashable Accelerator

instance Prelude.NFData Accelerator
