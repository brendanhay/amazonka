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
-- Module      : Network.AWS.Lightsail.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Instance where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.InstanceHardware
import Network.AWS.Lightsail.Types.InstanceNetworking
import Network.AWS.Lightsail.Types.InstanceState
import Network.AWS.Lightsail.Types.IpAddressType
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance (a virtual private server).
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The IP address type of the instance.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The bundle for the instance (e.g., @micro_1_0@).
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 addresses of the instance.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The size of the vCPU and the amount of RAM for the instance.
    hardware :: Prelude.Maybe InstanceHardware,
    -- | An array of objects representing the add-ons enabled on the instance.
    addOns :: Prelude.Maybe [AddOn],
    -- | The name of the SSH key being used to connect to the instance (e.g.,
    -- @LightsailDefaultKeyPair@).
    sshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the blueprint (e.g., @Amazon Linux@).
    blueprintName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the instance was created (e.g., @1479734909.17@) in
    -- Unix time format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the instance (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The blueprint ID (e.g., @os_amlinux_2016_03@).
    blueprintId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource (usually @Instance@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The status code and the state (e.g., @running@) for the instance.
    state :: Prelude.Maybe InstanceState,
    -- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the public ports and monthly data transfer rates for
    -- the instance.
    networking :: Prelude.Maybe InstanceNetworking,
    -- | The public IP address of the instance.
    publicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The user name for connecting to the instance (e.g., @ec2-user@).
    username :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether this instance has a static IP
    -- assigned to it.
    isStaticIp :: Prelude.Maybe Prelude.Bool,
    -- | The region name and Availability Zone where the instance is located.
    location :: Prelude.Maybe ResourceLocation,
    -- | The private IP address of the instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'instance_ipAddressType' - The IP address type of the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- 'bundleId', 'instance_bundleId' - The bundle for the instance (e.g., @micro_1_0@).
--
-- 'ipv6Addresses', 'instance_ipv6Addresses' - The IPv6 addresses of the instance.
--
-- 'hardware', 'instance_hardware' - The size of the vCPU and the amount of RAM for the instance.
--
-- 'addOns', 'instance_addOns' - An array of objects representing the add-ons enabled on the instance.
--
-- 'sshKeyName', 'instance_sshKeyName' - The name of the SSH key being used to connect to the instance (e.g.,
-- @LightsailDefaultKeyPair@).
--
-- 'blueprintName', 'instance_blueprintName' - The friendly name of the blueprint (e.g., @Amazon Linux@).
--
-- 'createdAt', 'instance_createdAt' - The timestamp when the instance was created (e.g., @1479734909.17@) in
-- Unix time format.
--
-- 'arn', 'instance_arn' - The Amazon Resource Name (ARN) of the instance (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
--
-- 'supportCode', 'instance_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'blueprintId', 'instance_blueprintId' - The blueprint ID (e.g., @os_amlinux_2016_03@).
--
-- 'resourceType', 'instance_resourceType' - The type of resource (usually @Instance@).
--
-- 'state', 'instance_state' - The status code and the state (e.g., @running@) for the instance.
--
-- 'name', 'instance_name' - The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
--
-- 'tags', 'instance_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'networking', 'instance_networking' - Information about the public ports and monthly data transfer rates for
-- the instance.
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IP address of the instance.
--
-- 'username', 'instance_username' - The user name for connecting to the instance (e.g., @ec2-user@).
--
-- 'isStaticIp', 'instance_isStaticIp' - A Boolean value indicating whether this instance has a static IP
-- assigned to it.
--
-- 'location', 'instance_location' - The region name and Availability Zone where the instance is located.
--
-- 'privateIpAddress', 'instance_privateIpAddress' - The private IP address of the instance.
newInstance ::
  Instance
newInstance =
  Instance'
    { ipAddressType = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      hardware = Prelude.Nothing,
      addOns = Prelude.Nothing,
      sshKeyName = Prelude.Nothing,
      blueprintName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      blueprintId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      networking = Prelude.Nothing,
      publicIpAddress = Prelude.Nothing,
      username = Prelude.Nothing,
      isStaticIp = Prelude.Nothing,
      location = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The IP address type of the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
instance_ipAddressType :: Lens.Lens' Instance (Prelude.Maybe IpAddressType)
instance_ipAddressType = Lens.lens (\Instance' {ipAddressType} -> ipAddressType) (\s@Instance' {} a -> s {ipAddressType = a} :: Instance)

-- | The bundle for the instance (e.g., @micro_1_0@).
instance_bundleId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_bundleId = Lens.lens (\Instance' {bundleId} -> bundleId) (\s@Instance' {} a -> s {bundleId = a} :: Instance)

-- | The IPv6 addresses of the instance.
instance_ipv6Addresses :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_ipv6Addresses = Lens.lens (\Instance' {ipv6Addresses} -> ipv6Addresses) (\s@Instance' {} a -> s {ipv6Addresses = a} :: Instance) Prelude.. Lens.mapping Lens._Coerce

-- | The size of the vCPU and the amount of RAM for the instance.
instance_hardware :: Lens.Lens' Instance (Prelude.Maybe InstanceHardware)
instance_hardware = Lens.lens (\Instance' {hardware} -> hardware) (\s@Instance' {} a -> s {hardware = a} :: Instance)

-- | An array of objects representing the add-ons enabled on the instance.
instance_addOns :: Lens.Lens' Instance (Prelude.Maybe [AddOn])
instance_addOns = Lens.lens (\Instance' {addOns} -> addOns) (\s@Instance' {} a -> s {addOns = a} :: Instance) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the SSH key being used to connect to the instance (e.g.,
-- @LightsailDefaultKeyPair@).
instance_sshKeyName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshKeyName = Lens.lens (\Instance' {sshKeyName} -> sshKeyName) (\s@Instance' {} a -> s {sshKeyName = a} :: Instance)

-- | The friendly name of the blueprint (e.g., @Amazon Linux@).
instance_blueprintName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_blueprintName = Lens.lens (\Instance' {blueprintName} -> blueprintName) (\s@Instance' {} a -> s {blueprintName = a} :: Instance)

-- | The timestamp when the instance was created (e.g., @1479734909.17@) in
-- Unix time format.
instance_createdAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the instance (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
instance_arn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
instance_supportCode :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_supportCode = Lens.lens (\Instance' {supportCode} -> supportCode) (\s@Instance' {} a -> s {supportCode = a} :: Instance)

-- | The blueprint ID (e.g., @os_amlinux_2016_03@).
instance_blueprintId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_blueprintId = Lens.lens (\Instance' {blueprintId} -> blueprintId) (\s@Instance' {} a -> s {blueprintId = a} :: Instance)

-- | The type of resource (usually @Instance@).
instance_resourceType :: Lens.Lens' Instance (Prelude.Maybe ResourceType)
instance_resourceType = Lens.lens (\Instance' {resourceType} -> resourceType) (\s@Instance' {} a -> s {resourceType = a} :: Instance)

-- | The status code and the state (e.g., @running@) for the instance.
instance_state :: Lens.Lens' Instance (Prelude.Maybe InstanceState)
instance_state = Lens.lens (\Instance' {state} -> state) (\s@Instance' {} a -> s {state = a} :: Instance)

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
instance_name :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_name = Lens.lens (\Instance' {name} -> name) (\s@Instance' {} a -> s {name = a} :: Instance)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
instance_tags :: Lens.Lens' Instance (Prelude.Maybe [Tag])
instance_tags = Lens.lens (\Instance' {tags} -> tags) (\s@Instance' {} a -> s {tags = a} :: Instance) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the public ports and monthly data transfer rates for
-- the instance.
instance_networking :: Lens.Lens' Instance (Prelude.Maybe InstanceNetworking)
instance_networking = Lens.lens (\Instance' {networking} -> networking) (\s@Instance' {} a -> s {networking = a} :: Instance)

-- | The public IP address of the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | The user name for connecting to the instance (e.g., @ec2-user@).
instance_username :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_username = Lens.lens (\Instance' {username} -> username) (\s@Instance' {} a -> s {username = a} :: Instance)

-- | A Boolean value indicating whether this instance has a static IP
-- assigned to it.
instance_isStaticIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_isStaticIp = Lens.lens (\Instance' {isStaticIp} -> isStaticIp) (\s@Instance' {} a -> s {isStaticIp = a} :: Instance)

-- | The region name and Availability Zone where the instance is located.
instance_location :: Lens.Lens' Instance (Prelude.Maybe ResourceLocation)
instance_location = Lens.lens (\Instance' {location} -> location) (\s@Instance' {} a -> s {location = a} :: Instance)

-- | The private IP address of the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Core..:? "ipAddressType")
            Prelude.<*> (x Core..:? "bundleId")
            Prelude.<*> (x Core..:? "ipv6Addresses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "hardware")
            Prelude.<*> (x Core..:? "addOns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "sshKeyName")
            Prelude.<*> (x Core..:? "blueprintName")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "blueprintId")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "networking")
            Prelude.<*> (x Core..:? "publicIpAddress")
            Prelude.<*> (x Core..:? "username")
            Prelude.<*> (x Core..:? "isStaticIp")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "privateIpAddress")
      )

instance Prelude.Hashable Instance

instance Prelude.NFData Instance
