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

-- | Describes an instance (a virtual private server).
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The IP address type of the instance.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | The IPv6 addresses of the instance.
    ipv6Addresses :: Core.Maybe [Core.Text],
    -- | The bundle for the instance (e.g., @micro_1_0@).
    bundleId :: Core.Maybe Core.Text,
    -- | The size of the vCPU and the amount of RAM for the instance.
    hardware :: Core.Maybe InstanceHardware,
    -- | An array of objects representing the add-ons enabled on the instance.
    addOns :: Core.Maybe [AddOn],
    -- | The friendly name of the blueprint (e.g., @Amazon Linux@).
    blueprintName :: Core.Maybe Core.Text,
    -- | The name of the SSH key being used to connect to the instance (e.g.,
    -- @LightsailDefaultKeyPair@).
    sshKeyName :: Core.Maybe Core.Text,
    -- | The timestamp when the instance was created (e.g., @1479734909.17@) in
    -- Unix time format.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the instance (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
    arn :: Core.Maybe Core.Text,
    -- | The blueprint ID (e.g., @os_amlinux_2016_03@).
    blueprintId :: Core.Maybe Core.Text,
    -- | The type of resource (usually @Instance@).
    resourceType :: Core.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Core.Maybe Core.Text,
    -- | The status code and the state (e.g., @running@) for the instance.
    state :: Core.Maybe InstanceState,
    -- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
    name :: Core.Maybe Core.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Core.Maybe [Tag],
    -- | Information about the public ports and monthly data transfer rates for
    -- the instance.
    networking :: Core.Maybe InstanceNetworking,
    -- | The user name for connecting to the instance (e.g., @ec2-user@).
    username :: Core.Maybe Core.Text,
    -- | The public IP address of the instance.
    publicIpAddress :: Core.Maybe Core.Text,
    -- | A Boolean value indicating whether this instance has a static IP
    -- assigned to it.
    isStaticIp :: Core.Maybe Core.Bool,
    -- | The region name and Availability Zone where the instance is located.
    location :: Core.Maybe ResourceLocation,
    -- | The private IP address of the instance.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'ipv6Addresses', 'instance_ipv6Addresses' - The IPv6 addresses of the instance.
--
-- 'bundleId', 'instance_bundleId' - The bundle for the instance (e.g., @micro_1_0@).
--
-- 'hardware', 'instance_hardware' - The size of the vCPU and the amount of RAM for the instance.
--
-- 'addOns', 'instance_addOns' - An array of objects representing the add-ons enabled on the instance.
--
-- 'blueprintName', 'instance_blueprintName' - The friendly name of the blueprint (e.g., @Amazon Linux@).
--
-- 'sshKeyName', 'instance_sshKeyName' - The name of the SSH key being used to connect to the instance (e.g.,
-- @LightsailDefaultKeyPair@).
--
-- 'createdAt', 'instance_createdAt' - The timestamp when the instance was created (e.g., @1479734909.17@) in
-- Unix time format.
--
-- 'arn', 'instance_arn' - The Amazon Resource Name (ARN) of the instance (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
--
-- 'blueprintId', 'instance_blueprintId' - The blueprint ID (e.g., @os_amlinux_2016_03@).
--
-- 'resourceType', 'instance_resourceType' - The type of resource (usually @Instance@).
--
-- 'supportCode', 'instance_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'state', 'instance_state' - The status code and the state (e.g., @running@) for the instance.
--
-- 'name', 'instance_name' - The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
--
-- 'tags', 'instance_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
--
-- 'networking', 'instance_networking' - Information about the public ports and monthly data transfer rates for
-- the instance.
--
-- 'username', 'instance_username' - The user name for connecting to the instance (e.g., @ec2-user@).
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IP address of the instance.
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
    { ipAddressType = Core.Nothing,
      ipv6Addresses = Core.Nothing,
      bundleId = Core.Nothing,
      hardware = Core.Nothing,
      addOns = Core.Nothing,
      blueprintName = Core.Nothing,
      sshKeyName = Core.Nothing,
      createdAt = Core.Nothing,
      arn = Core.Nothing,
      blueprintId = Core.Nothing,
      resourceType = Core.Nothing,
      supportCode = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      networking = Core.Nothing,
      username = Core.Nothing,
      publicIpAddress = Core.Nothing,
      isStaticIp = Core.Nothing,
      location = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | The IP address type of the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
instance_ipAddressType :: Lens.Lens' Instance (Core.Maybe IpAddressType)
instance_ipAddressType = Lens.lens (\Instance' {ipAddressType} -> ipAddressType) (\s@Instance' {} a -> s {ipAddressType = a} :: Instance)

-- | The IPv6 addresses of the instance.
instance_ipv6Addresses :: Lens.Lens' Instance (Core.Maybe [Core.Text])
instance_ipv6Addresses = Lens.lens (\Instance' {ipv6Addresses} -> ipv6Addresses) (\s@Instance' {} a -> s {ipv6Addresses = a} :: Instance) Core.. Lens.mapping Lens._Coerce

-- | The bundle for the instance (e.g., @micro_1_0@).
instance_bundleId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_bundleId = Lens.lens (\Instance' {bundleId} -> bundleId) (\s@Instance' {} a -> s {bundleId = a} :: Instance)

-- | The size of the vCPU and the amount of RAM for the instance.
instance_hardware :: Lens.Lens' Instance (Core.Maybe InstanceHardware)
instance_hardware = Lens.lens (\Instance' {hardware} -> hardware) (\s@Instance' {} a -> s {hardware = a} :: Instance)

-- | An array of objects representing the add-ons enabled on the instance.
instance_addOns :: Lens.Lens' Instance (Core.Maybe [AddOn])
instance_addOns = Lens.lens (\Instance' {addOns} -> addOns) (\s@Instance' {} a -> s {addOns = a} :: Instance) Core.. Lens.mapping Lens._Coerce

-- | The friendly name of the blueprint (e.g., @Amazon Linux@).
instance_blueprintName :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_blueprintName = Lens.lens (\Instance' {blueprintName} -> blueprintName) (\s@Instance' {} a -> s {blueprintName = a} :: Instance)

-- | The name of the SSH key being used to connect to the instance (e.g.,
-- @LightsailDefaultKeyPair@).
instance_sshKeyName :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_sshKeyName = Lens.lens (\Instance' {sshKeyName} -> sshKeyName) (\s@Instance' {} a -> s {sshKeyName = a} :: Instance)

-- | The timestamp when the instance was created (e.g., @1479734909.17@) in
-- Unix time format.
instance_createdAt :: Lens.Lens' Instance (Core.Maybe Core.UTCTime)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the instance (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
instance_arn :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | The blueprint ID (e.g., @os_amlinux_2016_03@).
instance_blueprintId :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_blueprintId = Lens.lens (\Instance' {blueprintId} -> blueprintId) (\s@Instance' {} a -> s {blueprintId = a} :: Instance)

-- | The type of resource (usually @Instance@).
instance_resourceType :: Lens.Lens' Instance (Core.Maybe ResourceType)
instance_resourceType = Lens.lens (\Instance' {resourceType} -> resourceType) (\s@Instance' {} a -> s {resourceType = a} :: Instance)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
instance_supportCode :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_supportCode = Lens.lens (\Instance' {supportCode} -> supportCode) (\s@Instance' {} a -> s {supportCode = a} :: Instance)

-- | The status code and the state (e.g., @running@) for the instance.
instance_state :: Lens.Lens' Instance (Core.Maybe InstanceState)
instance_state = Lens.lens (\Instance' {state} -> state) (\s@Instance' {} a -> s {state = a} :: Instance)

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
instance_name :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_name = Lens.lens (\Instance' {name} -> name) (\s@Instance' {} a -> s {name = a} :: Instance)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
instance_tags :: Lens.Lens' Instance (Core.Maybe [Tag])
instance_tags = Lens.lens (\Instance' {tags} -> tags) (\s@Instance' {} a -> s {tags = a} :: Instance) Core.. Lens.mapping Lens._Coerce

-- | Information about the public ports and monthly data transfer rates for
-- the instance.
instance_networking :: Lens.Lens' Instance (Core.Maybe InstanceNetworking)
instance_networking = Lens.lens (\Instance' {networking} -> networking) (\s@Instance' {} a -> s {networking = a} :: Instance)

-- | The user name for connecting to the instance (e.g., @ec2-user@).
instance_username :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_username = Lens.lens (\Instance' {username} -> username) (\s@Instance' {} a -> s {username = a} :: Instance)

-- | The public IP address of the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | A Boolean value indicating whether this instance has a static IP
-- assigned to it.
instance_isStaticIp :: Lens.Lens' Instance (Core.Maybe Core.Bool)
instance_isStaticIp = Lens.lens (\Instance' {isStaticIp} -> isStaticIp) (\s@Instance' {} a -> s {isStaticIp = a} :: Instance)

-- | The region name and Availability Zone where the instance is located.
instance_location :: Lens.Lens' Instance (Core.Maybe ResourceLocation)
instance_location = Lens.lens (\Instance' {location} -> location) (\s@Instance' {} a -> s {location = a} :: Instance)

-- | The private IP address of the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Core.Maybe Core.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Core.<$> (x Core..:? "ipAddressType")
            Core.<*> (x Core..:? "ipv6Addresses" Core..!= Core.mempty)
            Core.<*> (x Core..:? "bundleId")
            Core.<*> (x Core..:? "hardware")
            Core.<*> (x Core..:? "addOns" Core..!= Core.mempty)
            Core.<*> (x Core..:? "blueprintName")
            Core.<*> (x Core..:? "sshKeyName")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "blueprintId")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "supportCode")
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "networking")
            Core.<*> (x Core..:? "username")
            Core.<*> (x Core..:? "publicIpAddress")
            Core.<*> (x Core..:? "isStaticIp")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "privateIpAddress")
      )

instance Core.Hashable Instance

instance Core.NFData Instance
