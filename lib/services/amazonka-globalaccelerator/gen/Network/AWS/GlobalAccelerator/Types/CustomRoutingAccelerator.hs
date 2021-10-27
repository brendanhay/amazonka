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
-- Module      : Network.AWS.GlobalAccelerator.Types.CustomRoutingAccelerator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GlobalAccelerator.Types.CustomRoutingAccelerator where

import qualified Network.AWS.Core as Core
import Network.AWS.GlobalAccelerator.Types.CustomRoutingAcceleratorStatus
import Network.AWS.GlobalAccelerator.Types.IpAddressType
import Network.AWS.GlobalAccelerator.Types.IpSet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Attributes of a custom routing accelerator.
--
-- /See:/ 'newCustomRoutingAccelerator' smart constructor.
data CustomRoutingAccelerator = CustomRoutingAccelerator'
  { -- | Describes the deployment status of the accelerator.
    status :: Prelude.Maybe CustomRoutingAcceleratorStatus,
    -- | The Amazon Resource Name (ARN) of the custom routing accelerator.
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
-- Create a value of 'CustomRoutingAccelerator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'customRoutingAccelerator_status' - Describes the deployment status of the accelerator.
--
-- 'acceleratorArn', 'customRoutingAccelerator_acceleratorArn' - The Amazon Resource Name (ARN) of the custom routing accelerator.
--
-- 'enabled', 'customRoutingAccelerator_enabled' - Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
--
-- 'createdTime', 'customRoutingAccelerator_createdTime' - The date and time that the accelerator was created.
--
-- 'lastModifiedTime', 'customRoutingAccelerator_lastModifiedTime' - The date and time that the accelerator was last modified.
--
-- 'ipAddressType', 'customRoutingAccelerator_ipAddressType' - The value for the address type must be IPv4.
--
-- 'name', 'customRoutingAccelerator_name' - The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
--
-- 'ipSets', 'customRoutingAccelerator_ipSets' - The static IP addresses that Global Accelerator associates with the
-- accelerator.
--
-- 'dnsName', 'customRoutingAccelerator_dnsName' - The Domain Name System (DNS) name that Global Accelerator creates that
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
newCustomRoutingAccelerator ::
  CustomRoutingAccelerator
newCustomRoutingAccelerator =
  CustomRoutingAccelerator'
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
customRoutingAccelerator_status :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe CustomRoutingAcceleratorStatus)
customRoutingAccelerator_status = Lens.lens (\CustomRoutingAccelerator' {status} -> status) (\s@CustomRoutingAccelerator' {} a -> s {status = a} :: CustomRoutingAccelerator)

-- | The Amazon Resource Name (ARN) of the custom routing accelerator.
customRoutingAccelerator_acceleratorArn :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Text)
customRoutingAccelerator_acceleratorArn = Lens.lens (\CustomRoutingAccelerator' {acceleratorArn} -> acceleratorArn) (\s@CustomRoutingAccelerator' {} a -> s {acceleratorArn = a} :: CustomRoutingAccelerator)

-- | Indicates whether the accelerator is enabled. The value is true or
-- false. The default value is true.
--
-- If the value is set to true, the accelerator cannot be deleted. If set
-- to false, accelerator can be deleted.
customRoutingAccelerator_enabled :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Bool)
customRoutingAccelerator_enabled = Lens.lens (\CustomRoutingAccelerator' {enabled} -> enabled) (\s@CustomRoutingAccelerator' {} a -> s {enabled = a} :: CustomRoutingAccelerator)

-- | The date and time that the accelerator was created.
customRoutingAccelerator_createdTime :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.UTCTime)
customRoutingAccelerator_createdTime = Lens.lens (\CustomRoutingAccelerator' {createdTime} -> createdTime) (\s@CustomRoutingAccelerator' {} a -> s {createdTime = a} :: CustomRoutingAccelerator) Prelude.. Lens.mapping Core._Time

-- | The date and time that the accelerator was last modified.
customRoutingAccelerator_lastModifiedTime :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.UTCTime)
customRoutingAccelerator_lastModifiedTime = Lens.lens (\CustomRoutingAccelerator' {lastModifiedTime} -> lastModifiedTime) (\s@CustomRoutingAccelerator' {} a -> s {lastModifiedTime = a} :: CustomRoutingAccelerator) Prelude.. Lens.mapping Core._Time

-- | The value for the address type must be IPv4.
customRoutingAccelerator_ipAddressType :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe IpAddressType)
customRoutingAccelerator_ipAddressType = Lens.lens (\CustomRoutingAccelerator' {ipAddressType} -> ipAddressType) (\s@CustomRoutingAccelerator' {} a -> s {ipAddressType = a} :: CustomRoutingAccelerator)

-- | The name of the accelerator. The name must contain only alphanumeric
-- characters or hyphens (-), and must not begin or end with a hyphen.
customRoutingAccelerator_name :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Text)
customRoutingAccelerator_name = Lens.lens (\CustomRoutingAccelerator' {name} -> name) (\s@CustomRoutingAccelerator' {} a -> s {name = a} :: CustomRoutingAccelerator)

-- | The static IP addresses that Global Accelerator associates with the
-- accelerator.
customRoutingAccelerator_ipSets :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe [IpSet])
customRoutingAccelerator_ipSets = Lens.lens (\CustomRoutingAccelerator' {ipSets} -> ipSets) (\s@CustomRoutingAccelerator' {} a -> s {ipSets = a} :: CustomRoutingAccelerator) Prelude.. Lens.mapping Lens.coerced

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
customRoutingAccelerator_dnsName :: Lens.Lens' CustomRoutingAccelerator (Prelude.Maybe Prelude.Text)
customRoutingAccelerator_dnsName = Lens.lens (\CustomRoutingAccelerator' {dnsName} -> dnsName) (\s@CustomRoutingAccelerator' {} a -> s {dnsName = a} :: CustomRoutingAccelerator)

instance Core.FromJSON CustomRoutingAccelerator where
  parseJSON =
    Core.withObject
      "CustomRoutingAccelerator"
      ( \x ->
          CustomRoutingAccelerator'
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

instance Prelude.Hashable CustomRoutingAccelerator

instance Prelude.NFData CustomRoutingAccelerator
