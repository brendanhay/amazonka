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
-- Module      : Amazonka.Lightsail.Types.Instance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Instance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AddOn
import Amazonka.Lightsail.Types.InstanceHardware
import Amazonka.Lightsail.Types.InstanceMetadataOptions
import Amazonka.Lightsail.Types.InstanceNetworking
import Amazonka.Lightsail.Types.InstanceState
import Amazonka.Lightsail.Types.IpAddressType
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an instance (a virtual private server).
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The type of resource (usually @Instance@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The user name for connecting to the instance (e.g., @ec2-user@).
    username :: Prelude.Maybe Prelude.Text,
    -- | The blueprint ID (e.g., @os_amlinux_2016_03@).
    blueprintId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the instance (e.g.,
    -- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status code and the state (e.g., @running@) for the instance.
    state :: Prelude.Maybe InstanceState,
    -- | A Boolean value indicating whether this instance has a static IP
    -- assigned to it.
    isStaticIp :: Prelude.Maybe Prelude.Bool,
    -- | The region name and Availability Zone where the instance is located.
    location :: Prelude.Maybe ResourceLocation,
    -- | The size of the vCPU and the amount of RAM for the instance.
    hardware :: Prelude.Maybe InstanceHardware,
    -- | The public IP address of the instance.
    publicIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The bundle for the instance (e.g., @micro_1_0@).
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The private IP address of the instance.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSH key being used to connect to the instance (e.g.,
    -- @LightsailDefaultKeyPair@).
    sshKeyName :: Prelude.Maybe Prelude.Text,
    -- | An array of objects representing the add-ons enabled on the instance.
    addOns :: Prelude.Maybe [AddOn],
    -- | The support code. Include this code in your email to support when you
    -- have questions about an instance or another resource in Lightsail. This
    -- code enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the blueprint (e.g., @Amazon Linux@).
    blueprintName :: Prelude.Maybe Prelude.Text,
    -- | The IP address type of the instance.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | Information about the public ports and monthly data transfer rates for
    -- the instance.
    networking :: Prelude.Maybe InstanceNetworking,
    -- | The timestamp when the instance was created (e.g., @1479734909.17@) in
    -- Unix time format.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The IPv6 addresses of the instance.
    ipv6Addresses :: Prelude.Maybe [Prelude.Text],
    -- | The metadata options for the Amazon Lightsail instance.
    metadataOptions :: Prelude.Maybe InstanceMetadataOptions
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
-- 'tags', 'instance_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'resourceType', 'instance_resourceType' - The type of resource (usually @Instance@).
--
-- 'name', 'instance_name' - The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
--
-- 'username', 'instance_username' - The user name for connecting to the instance (e.g., @ec2-user@).
--
-- 'blueprintId', 'instance_blueprintId' - The blueprint ID (e.g., @os_amlinux_2016_03@).
--
-- 'arn', 'instance_arn' - The Amazon Resource Name (ARN) of the instance (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
--
-- 'state', 'instance_state' - The status code and the state (e.g., @running@) for the instance.
--
-- 'isStaticIp', 'instance_isStaticIp' - A Boolean value indicating whether this instance has a static IP
-- assigned to it.
--
-- 'location', 'instance_location' - The region name and Availability Zone where the instance is located.
--
-- 'hardware', 'instance_hardware' - The size of the vCPU and the amount of RAM for the instance.
--
-- 'publicIpAddress', 'instance_publicIpAddress' - The public IP address of the instance.
--
-- 'bundleId', 'instance_bundleId' - The bundle for the instance (e.g., @micro_1_0@).
--
-- 'privateIpAddress', 'instance_privateIpAddress' - The private IP address of the instance.
--
-- 'sshKeyName', 'instance_sshKeyName' - The name of the SSH key being used to connect to the instance (e.g.,
-- @LightsailDefaultKeyPair@).
--
-- 'addOns', 'instance_addOns' - An array of objects representing the add-ons enabled on the instance.
--
-- 'supportCode', 'instance_supportCode' - The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
--
-- 'blueprintName', 'instance_blueprintName' - The friendly name of the blueprint (e.g., @Amazon Linux@).
--
-- 'ipAddressType', 'instance_ipAddressType' - The IP address type of the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
--
-- 'networking', 'instance_networking' - Information about the public ports and monthly data transfer rates for
-- the instance.
--
-- 'createdAt', 'instance_createdAt' - The timestamp when the instance was created (e.g., @1479734909.17@) in
-- Unix time format.
--
-- 'ipv6Addresses', 'instance_ipv6Addresses' - The IPv6 addresses of the instance.
--
-- 'metadataOptions', 'instance_metadataOptions' - The metadata options for the Amazon Lightsail instance.
newInstance ::
  Instance
newInstance =
  Instance'
    { tags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      name = Prelude.Nothing,
      username = Prelude.Nothing,
      blueprintId = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      isStaticIp = Prelude.Nothing,
      location = Prelude.Nothing,
      hardware = Prelude.Nothing,
      publicIpAddress = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      sshKeyName = Prelude.Nothing,
      addOns = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      blueprintName = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      networking = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      metadataOptions = Prelude.Nothing
    }

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
instance_tags :: Lens.Lens' Instance (Prelude.Maybe [Tag])
instance_tags = Lens.lens (\Instance' {tags} -> tags) (\s@Instance' {} a -> s {tags = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The type of resource (usually @Instance@).
instance_resourceType :: Lens.Lens' Instance (Prelude.Maybe ResourceType)
instance_resourceType = Lens.lens (\Instance' {resourceType} -> resourceType) (\s@Instance' {} a -> s {resourceType = a} :: Instance)

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@).
instance_name :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_name = Lens.lens (\Instance' {name} -> name) (\s@Instance' {} a -> s {name = a} :: Instance)

-- | The user name for connecting to the instance (e.g., @ec2-user@).
instance_username :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_username = Lens.lens (\Instance' {username} -> username) (\s@Instance' {} a -> s {username = a} :: Instance)

-- | The blueprint ID (e.g., @os_amlinux_2016_03@).
instance_blueprintId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_blueprintId = Lens.lens (\Instance' {blueprintId} -> blueprintId) (\s@Instance' {} a -> s {blueprintId = a} :: Instance)

-- | The Amazon Resource Name (ARN) of the instance (e.g.,
-- @arn:aws:lightsail:us-east-2:123456789101:Instance\/244ad76f-8aad-4741-809f-12345EXAMPLE@).
instance_arn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | The status code and the state (e.g., @running@) for the instance.
instance_state :: Lens.Lens' Instance (Prelude.Maybe InstanceState)
instance_state = Lens.lens (\Instance' {state} -> state) (\s@Instance' {} a -> s {state = a} :: Instance)

-- | A Boolean value indicating whether this instance has a static IP
-- assigned to it.
instance_isStaticIp :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_isStaticIp = Lens.lens (\Instance' {isStaticIp} -> isStaticIp) (\s@Instance' {} a -> s {isStaticIp = a} :: Instance)

-- | The region name and Availability Zone where the instance is located.
instance_location :: Lens.Lens' Instance (Prelude.Maybe ResourceLocation)
instance_location = Lens.lens (\Instance' {location} -> location) (\s@Instance' {} a -> s {location = a} :: Instance)

-- | The size of the vCPU and the amount of RAM for the instance.
instance_hardware :: Lens.Lens' Instance (Prelude.Maybe InstanceHardware)
instance_hardware = Lens.lens (\Instance' {hardware} -> hardware) (\s@Instance' {} a -> s {hardware = a} :: Instance)

-- | The public IP address of the instance.
instance_publicIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_publicIpAddress = Lens.lens (\Instance' {publicIpAddress} -> publicIpAddress) (\s@Instance' {} a -> s {publicIpAddress = a} :: Instance)

-- | The bundle for the instance (e.g., @micro_1_0@).
instance_bundleId :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_bundleId = Lens.lens (\Instance' {bundleId} -> bundleId) (\s@Instance' {} a -> s {bundleId = a} :: Instance)

-- | The private IP address of the instance.
instance_privateIpAddress :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_privateIpAddress = Lens.lens (\Instance' {privateIpAddress} -> privateIpAddress) (\s@Instance' {} a -> s {privateIpAddress = a} :: Instance)

-- | The name of the SSH key being used to connect to the instance (e.g.,
-- @LightsailDefaultKeyPair@).
instance_sshKeyName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_sshKeyName = Lens.lens (\Instance' {sshKeyName} -> sshKeyName) (\s@Instance' {} a -> s {sshKeyName = a} :: Instance)

-- | An array of objects representing the add-ons enabled on the instance.
instance_addOns :: Lens.Lens' Instance (Prelude.Maybe [AddOn])
instance_addOns = Lens.lens (\Instance' {addOns} -> addOns) (\s@Instance' {} a -> s {addOns = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The support code. Include this code in your email to support when you
-- have questions about an instance or another resource in Lightsail. This
-- code enables our support team to look up your Lightsail information more
-- easily.
instance_supportCode :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_supportCode = Lens.lens (\Instance' {supportCode} -> supportCode) (\s@Instance' {} a -> s {supportCode = a} :: Instance)

-- | The friendly name of the blueprint (e.g., @Amazon Linux@).
instance_blueprintName :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_blueprintName = Lens.lens (\Instance' {blueprintName} -> blueprintName) (\s@Instance' {} a -> s {blueprintName = a} :: Instance)

-- | The IP address type of the instance.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
instance_ipAddressType :: Lens.Lens' Instance (Prelude.Maybe IpAddressType)
instance_ipAddressType = Lens.lens (\Instance' {ipAddressType} -> ipAddressType) (\s@Instance' {} a -> s {ipAddressType = a} :: Instance)

-- | Information about the public ports and monthly data transfer rates for
-- the instance.
instance_networking :: Lens.Lens' Instance (Prelude.Maybe InstanceNetworking)
instance_networking = Lens.lens (\Instance' {networking} -> networking) (\s@Instance' {} a -> s {networking = a} :: Instance)

-- | The timestamp when the instance was created (e.g., @1479734909.17@) in
-- Unix time format.
instance_createdAt :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_createdAt = Lens.lens (\Instance' {createdAt} -> createdAt) (\s@Instance' {} a -> s {createdAt = a} :: Instance) Prelude.. Lens.mapping Data._Time

-- | The IPv6 addresses of the instance.
instance_ipv6Addresses :: Lens.Lens' Instance (Prelude.Maybe [Prelude.Text])
instance_ipv6Addresses = Lens.lens (\Instance' {ipv6Addresses} -> ipv6Addresses) (\s@Instance' {} a -> s {ipv6Addresses = a} :: Instance) Prelude.. Lens.mapping Lens.coerced

-- | The metadata options for the Amazon Lightsail instance.
instance_metadataOptions :: Lens.Lens' Instance (Prelude.Maybe InstanceMetadataOptions)
instance_metadataOptions = Lens.lens (\Instance' {metadataOptions} -> metadataOptions) (\s@Instance' {} a -> s {metadataOptions = a} :: Instance)

instance Data.FromJSON Instance where
  parseJSON =
    Data.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "username")
            Prelude.<*> (x Data..:? "blueprintId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "isStaticIp")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "hardware")
            Prelude.<*> (x Data..:? "publicIpAddress")
            Prelude.<*> (x Data..:? "bundleId")
            Prelude.<*> (x Data..:? "privateIpAddress")
            Prelude.<*> (x Data..:? "sshKeyName")
            Prelude.<*> (x Data..:? "addOns" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "blueprintName")
            Prelude.<*> (x Data..:? "ipAddressType")
            Prelude.<*> (x Data..:? "networking")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "ipv6Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "metadataOptions")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` blueprintId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` isStaticIp
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` hardware
      `Prelude.hashWithSalt` publicIpAddress
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` sshKeyName
      `Prelude.hashWithSalt` addOns
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` blueprintName
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` networking
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` metadataOptions

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf blueprintId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf isStaticIp
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf hardware
      `Prelude.seq` Prelude.rnf publicIpAddress
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf sshKeyName
      `Prelude.seq` Prelude.rnf addOns
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf blueprintName
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf networking
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf
        metadataOptions
