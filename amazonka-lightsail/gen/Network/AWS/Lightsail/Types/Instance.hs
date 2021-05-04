{-# LANGUAGE DeriveDataTypeable #-}
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
    -- | The IPv6 addresses of the instance.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The bundle for the instance (e.g., @micro_1_0@).
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The size of the vCPU and the amount of RAM for the instance.
    hardware :: Prelude.Maybe InstanceHardware,
    -- | An array of objects representing the add-ons enabled on the instance.
    addOns :: Prelude.Maybe [AddOn],
    -- | The friendly name of the blueprint (e.g., @Amazon Linux@).
    blueprintName :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSH key being used to connect to the instance (e.g.,
    -- @LightsailDefaultKeyPair@).
    sshKeyName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the instance was created (e.g., @1479734909.17@) in
    -- Unix time format.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the instance (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The blueprint ID (e.g., @os_amlinux_2016_03@).
    blueprintId :: Prelude.Maybe Prelude.Text,
    -- | The type of resource (usually @Instance@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The status code and the state (e.g., @running@) for the instance.
    state :: Prelude.Maybe InstanceState,
    -- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | Information about the public ports and monthly data transfer rates for
    -- the instance.
    networking :: Prelude.Maybe InstanceNetworking,
    -- | The user name for connecting to the instance (e.g., @ec2-user@).
    username :: Prelude.Maybe Prelude.Text,
    -- | The public IP address of the instance.
    publicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether this instance has a static IP
    -- assigned to it.
    isStaticIp :: Prelude.Maybe Prelude.Bool,
    -- | The region name and Availability Zone where the instance is located.
    location :: Prelude.Maybe ResourceLocation,
    -- | The private IP address of the instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ipAddressType = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      hardware = Prelude.Nothing,
      addOns = Prelude.Nothing,
      blueprintName = Prelude.Nothing,
      sshKeyName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      blueprintId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      networking = Prelude.Nothing,
      username = Prelude.Nothing,
      publicIpAddress = Prelude.Nothing,
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

-- | The IPv6 addresses of the instance.
instance_ipv6Addresses :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_ipv6Addresses = Lens.lens (\Instance' {ipv6Addresses} -> ipv6Addresses) (\s@Instance' {} a -> s {ipv6Addresses = a} :: Instance) Prelude.. Lens.mapping Prelude._Coerce

-- | The bundle for the instance (e.g., @micro_1_0@).
instance_bundleId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_bundleId = Lens.lens (\Instance' {bundleId} -> bundleId) (\s@Instance' {} a -> s {bundleId = a} :: Instance)

-- | The size of the vCPU and the amount of RAM for the instance.
instance_hardware :: Lens.Lens' Instance (Prelude.Maybe InstanceHardware)
instance_hardware = Lens.lens (\Instance' {hardware} -> hardware) (\s@Instance' {} a -> s {hardware = a} :: Instance)

-- | An array of objects representing the add-ons enabled on the instance.
instance_addOns :: Lens.Lens' Instance (Prelude.Maybe [AddOn])
instance_addOns = Lens.lens (\Instance' {addOns} -> addOns) (\s@Instance' {} a -> s {addOns = a} :: Instance) Prelude.. Lens.mapping Prelude._Coerce

-- | The friendly name of the blueprint (e.g., @Amazon Linux@).
instance_blueprintName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_blueprintName = Lens.lens (\Instance' {blueprintName} -> blueprintName) (\s@Instance' {} a -> s {blueprintName = a} :: Instance)

-- | The name of the SSH key being used to connect to the instance (e.g.,
-- @LightsailDefaultKeyPair@).
instance_sshKeyName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshKeyName = Lens.lens (\Instance' {sshKeyName} -> sshKeyName) (\s@Instance' {} a -> s {sshKeyName = a} :: Instance)

-- | The timestamp when the instance was created (e.g., @1479734909.17@) in
-- Unix time format.
instance_createdAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the instance (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
instance_arn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | The blueprint ID (e.g., @os_amlinux_2016_03@).
instance_blueprintId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_blueprintId = Lens.lens (\Instance' {blueprintId} -> blueprintId) (\s@Instance' {} a -> s {blueprintId = a} :: Instance)

-- | The type of resource (usually @Instance@).
instance_resourceType :: Lens.Lens' Instance (Prelude.Maybe ResourceType)
instance_resourceType = Lens.lens (\Instance' {resourceType} -> resourceType) (\s@Instance' {} a -> s {resourceType = a} :: Instance)

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
instance_supportCode :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_supportCode = Lens.lens (\Instance' {supportCode} -> supportCode) (\s@Instance' {} a -> s {supportCode = a} :: Instance)

-- | The status code and the state (e.g., @running@) for the instance.
instance_state :: Lens.Lens' Instance (Prelude.Maybe InstanceState)
instance_state = Lens.lens (\Instance' {state} -> state) (\s@Instance' {} a -> s {state = a} :: Instance)

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
instance_name :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_name = Lens.lens (\Instance' {name} -> name) (\s@Instance' {} a -> s {name = a} :: Instance)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide>.
instance_tags :: Lens.Lens' Instance (Prelude.Maybe [Tag])
instance_tags = Lens.lens (\Instance' {tags} -> tags) (\s@Instance' {} a -> s {tags = a} :: Instance) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the public ports and monthly data transfer rates for
-- the instance.
instance_networking :: Lens.Lens' Instance (Prelude.Maybe InstanceNetworking)
instance_networking = Lens.lens (\Instance' {networking} -> networking) (\s@Instance' {} a -> s {networking = a} :: Instance)

-- | The user name for connecting to the instance (e.g., @ec2-user@).
instance_username :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_username = Lens.lens (\Instance' {username} -> username) (\s@Instance' {} a -> s {username = a} :: Instance)

-- | The public IP address of the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

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

instance Prelude.FromJSON Instance where
  parseJSON =
    Prelude.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Prelude..:? "ipAddressType")
            Prelude.<*> ( x Prelude..:? "ipv6Addresses"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "bundleId")
            Prelude.<*> (x Prelude..:? "hardware")
            Prelude.<*> (x Prelude..:? "addOns" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "blueprintName")
            Prelude.<*> (x Prelude..:? "sshKeyName")
            Prelude.<*> (x Prelude..:? "createdAt")
            Prelude.<*> (x Prelude..:? "arn")
            Prelude.<*> (x Prelude..:? "blueprintId")
            Prelude.<*> (x Prelude..:? "resourceType")
            Prelude.<*> (x Prelude..:? "supportCode")
            Prelude.<*> (x Prelude..:? "state")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "networking")
            Prelude.<*> (x Prelude..:? "username")
            Prelude.<*> (x Prelude..:? "publicIpAddress")
            Prelude.<*> (x Prelude..:? "isStaticIp")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "privateIpAddress")
      )

instance Prelude.Hashable Instance

instance Prelude.NFData Instance
