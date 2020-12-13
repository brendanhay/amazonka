{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Instance
  ( Instance (..),

    -- * Smart constructor
    mkInstance,

    -- * Lenses
    iState,
    iIpv6Address,
    iResourceType,
    iArn,
    iCreatedAt,
    iLocation,
    iSshKeyName,
    iAddOns,
    iUsername,
    iNetworking,
    iBundleId,
    iName,
    iSupportCode,
    iBlueprintId,
    iPrivateIPAddress,
    iBlueprintName,
    iIsStaticIP,
    iPublicIPAddress,
    iHardware,
    iTags,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AddOn
import Network.AWS.Lightsail.Types.InstanceHardware
import Network.AWS.Lightsail.Types.InstanceNetworking
import Network.AWS.Lightsail.Types.InstanceState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance (a virtual private server).
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { -- | The status code and the state (e.g., @running@ ) for the instance.
    state :: Lude.Maybe InstanceState,
    -- | The IPv6 address of the instance.
    ipv6Address :: Lude.Maybe Lude.Text,
    -- | The type of resource (usually @Instance@ ).
    resourceType :: Lude.Maybe ResourceType,
    -- | The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
    arn :: Lude.Maybe Lude.Text,
    -- | The timestamp when the instance was created (e.g., @1479734909.17@ ) in Unix time format.
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The region name and Availability Zone where the instance is located.
    location :: Lude.Maybe ResourceLocation,
    -- | The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
    sshKeyName :: Lude.Maybe Lude.Text,
    -- | An array of objects representing the add-ons enabled on the instance.
    addOns :: Lude.Maybe [AddOn],
    -- | The user name for connecting to the instance (e.g., @ec2-user@ ).
    username :: Lude.Maybe Lude.Text,
    -- | Information about the public ports and monthly data transfer rates for the instance.
    networking :: Lude.Maybe InstanceNetworking,
    -- | The bundle for the instance (e.g., @micro_1_0@ ).
    bundleId :: Lude.Maybe Lude.Text,
    -- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
    name :: Lude.Maybe Lude.Text,
    -- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
    supportCode :: Lude.Maybe Lude.Text,
    -- | The blueprint ID (e.g., @os_amlinux_2016_03@ ).
    blueprintId :: Lude.Maybe Lude.Text,
    -- | The private IP address of the instance.
    privateIPAddress :: Lude.Maybe Lude.Text,
    -- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
    blueprintName :: Lude.Maybe Lude.Text,
    -- | A Boolean value indicating whether this instance has a static IP assigned to it.
    isStaticIP :: Lude.Maybe Lude.Bool,
    -- | The public IP address of the instance.
    publicIPAddress :: Lude.Maybe Lude.Text,
    -- | The size of the vCPU and the amount of RAM for the instance.
    hardware :: Lude.Maybe InstanceHardware,
    -- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Instance' with the minimum fields required to make a request.
--
-- * 'state' - The status code and the state (e.g., @running@ ) for the instance.
-- * 'ipv6Address' - The IPv6 address of the instance.
-- * 'resourceType' - The type of resource (usually @Instance@ ).
-- * 'arn' - The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
-- * 'createdAt' - The timestamp when the instance was created (e.g., @1479734909.17@ ) in Unix time format.
-- * 'location' - The region name and Availability Zone where the instance is located.
-- * 'sshKeyName' - The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
-- * 'addOns' - An array of objects representing the add-ons enabled on the instance.
-- * 'username' - The user name for connecting to the instance (e.g., @ec2-user@ ).
-- * 'networking' - Information about the public ports and monthly data transfer rates for the instance.
-- * 'bundleId' - The bundle for the instance (e.g., @micro_1_0@ ).
-- * 'name' - The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
-- * 'supportCode' - The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
-- * 'blueprintId' - The blueprint ID (e.g., @os_amlinux_2016_03@ ).
-- * 'privateIPAddress' - The private IP address of the instance.
-- * 'blueprintName' - The friendly name of the blueprint (e.g., @Amazon Linux@ ).
-- * 'isStaticIP' - A Boolean value indicating whether this instance has a static IP assigned to it.
-- * 'publicIPAddress' - The public IP address of the instance.
-- * 'hardware' - The size of the vCPU and the amount of RAM for the instance.
-- * 'tags' - The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
mkInstance ::
  Instance
mkInstance =
  Instance'
    { state = Lude.Nothing,
      ipv6Address = Lude.Nothing,
      resourceType = Lude.Nothing,
      arn = Lude.Nothing,
      createdAt = Lude.Nothing,
      location = Lude.Nothing,
      sshKeyName = Lude.Nothing,
      addOns = Lude.Nothing,
      username = Lude.Nothing,
      networking = Lude.Nothing,
      bundleId = Lude.Nothing,
      name = Lude.Nothing,
      supportCode = Lude.Nothing,
      blueprintId = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      blueprintName = Lude.Nothing,
      isStaticIP = Lude.Nothing,
      publicIPAddress = Lude.Nothing,
      hardware = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The status code and the state (e.g., @running@ ) for the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Instance (Lude.Maybe InstanceState)
iState = Lens.lens (state :: Instance -> Lude.Maybe InstanceState) (\s a -> s {state = a} :: Instance)
{-# DEPRECATED iState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The IPv6 address of the instance.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIpv6Address :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iIpv6Address = Lens.lens (ipv6Address :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {ipv6Address = a} :: Instance)
{-# DEPRECATED iIpv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead." #-}

-- | The type of resource (usually @Instance@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iResourceType :: Lens.Lens' Instance (Lude.Maybe ResourceType)
iResourceType = Lens.lens (resourceType :: Instance -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Instance)
{-# DEPRECATED iResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArn :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iArn = Lens.lens (arn :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Instance)
{-# DEPRECATED iArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The timestamp when the instance was created (e.g., @1479734909.17@ ) in Unix time format.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedAt :: Lens.Lens' Instance (Lude.Maybe Lude.Timestamp)
iCreatedAt = Lens.lens (createdAt :: Instance -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Instance)
{-# DEPRECATED iCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The region name and Availability Zone where the instance is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLocation :: Lens.Lens' Instance (Lude.Maybe ResourceLocation)
iLocation = Lens.lens (location :: Instance -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: Instance)
{-# DEPRECATED iLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSshKeyName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iSshKeyName = Lens.lens (sshKeyName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {sshKeyName = a} :: Instance)
{-# DEPRECATED iSshKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead." #-}

-- | An array of objects representing the add-ons enabled on the instance.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAddOns :: Lens.Lens' Instance (Lude.Maybe [AddOn])
iAddOns = Lens.lens (addOns :: Instance -> Lude.Maybe [AddOn]) (\s a -> s {addOns = a} :: Instance)
{-# DEPRECATED iAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | The user name for connecting to the instance (e.g., @ec2-user@ ).
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iUsername :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iUsername = Lens.lens (username :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: Instance)
{-# DEPRECATED iUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | Information about the public ports and monthly data transfer rates for the instance.
--
-- /Note:/ Consider using 'networking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iNetworking :: Lens.Lens' Instance (Lude.Maybe InstanceNetworking)
iNetworking = Lens.lens (networking :: Instance -> Lude.Maybe InstanceNetworking) (\s a -> s {networking = a} :: Instance)
{-# DEPRECATED iNetworking "Use generic-lens or generic-optics with 'networking' instead." #-}

-- | The bundle for the instance (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBundleId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iBundleId = Lens.lens (bundleId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {bundleId = a} :: Instance)
{-# DEPRECATED iBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iName = Lens.lens (name :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Instance)
{-# DEPRECATED iName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSupportCode :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iSupportCode = Lens.lens (supportCode :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {supportCode = a} :: Instance)
{-# DEPRECATED iSupportCode "Use generic-lens or generic-optics with 'supportCode' instead." #-}

-- | The blueprint ID (e.g., @os_amlinux_2016_03@ ).
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlueprintId :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iBlueprintId = Lens.lens (blueprintId :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {blueprintId = a} :: Instance)
{-# DEPRECATED iBlueprintId "Use generic-lens or generic-optics with 'blueprintId' instead." #-}

-- | The private IP address of the instance.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPrivateIPAddress = Lens.lens (privateIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: Instance)
{-# DEPRECATED iPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
--
-- /Note:/ Consider using 'blueprintName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlueprintName :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iBlueprintName = Lens.lens (blueprintName :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {blueprintName = a} :: Instance)
{-# DEPRECATED iBlueprintName "Use generic-lens or generic-optics with 'blueprintName' instead." #-}

-- | A Boolean value indicating whether this instance has a static IP assigned to it.
--
-- /Note:/ Consider using 'isStaticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIsStaticIP :: Lens.Lens' Instance (Lude.Maybe Lude.Bool)
iIsStaticIP = Lens.lens (isStaticIP :: Instance -> Lude.Maybe Lude.Bool) (\s a -> s {isStaticIP = a} :: Instance)
{-# DEPRECATED iIsStaticIP "Use generic-lens or generic-optics with 'isStaticIP' instead." #-}

-- | The public IP address of the instance.
--
-- /Note:/ Consider using 'publicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIPAddress :: Lens.Lens' Instance (Lude.Maybe Lude.Text)
iPublicIPAddress = Lens.lens (publicIPAddress :: Instance -> Lude.Maybe Lude.Text) (\s a -> s {publicIPAddress = a} :: Instance)
{-# DEPRECATED iPublicIPAddress "Use generic-lens or generic-optics with 'publicIPAddress' instead." #-}

-- | The size of the vCPU and the amount of RAM for the instance.
--
-- /Note:/ Consider using 'hardware' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHardware :: Lens.Lens' Instance (Lude.Maybe InstanceHardware)
iHardware = Lens.lens (hardware :: Instance -> Lude.Maybe InstanceHardware) (\s a -> s {hardware = a} :: Instance)
{-# DEPRECATED iHardware "Use generic-lens or generic-optics with 'hardware' instead." #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTags :: Lens.Lens' Instance (Lude.Maybe [Tag])
iTags = Lens.lens (tags :: Instance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: Instance)
{-# DEPRECATED iTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Instance where
  parseJSON =
    Lude.withObject
      "Instance"
      ( \x ->
          Instance'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "ipv6Address")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "sshKeyName")
            Lude.<*> (x Lude..:? "addOns" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "username")
            Lude.<*> (x Lude..:? "networking")
            Lude.<*> (x Lude..:? "bundleId")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "supportCode")
            Lude.<*> (x Lude..:? "blueprintId")
            Lude.<*> (x Lude..:? "privateIpAddress")
            Lude.<*> (x Lude..:? "blueprintName")
            Lude.<*> (x Lude..:? "isStaticIp")
            Lude.<*> (x Lude..:? "publicIpAddress")
            Lude.<*> (x Lude..:? "hardware")
            Lude.<*> (x Lude..:? "tags" Lude..!= Lude.mempty)
      )
