{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Instance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Instance
  ( Instance (..)
  -- * Smart constructor
  , mkInstance
  -- * Lenses
  , iAddOns
  , iArn
  , iBlueprintId
  , iBlueprintName
  , iBundleId
  , iCreatedAt
  , iHardware
  , iIpv6Address
  , iIsStaticIp
  , iLocation
  , iName
  , iNetworking
  , iPrivateIpAddress
  , iPublicIpAddress
  , iResourceType
  , iSshKeyName
  , iState
  , iSupportCode
  , iTags
  , iUsername
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.AddOn as Types
import qualified Network.AWS.Lightsail.Types.InstanceHardware as Types
import qualified Network.AWS.Lightsail.Types.InstanceNetworking as Types
import qualified Network.AWS.Lightsail.Types.InstanceState as Types
import qualified Network.AWS.Lightsail.Types.IpAddress as Types
import qualified Network.AWS.Lightsail.Types.IpV6Address as Types
import qualified Network.AWS.Lightsail.Types.NonEmptyString as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Lightsail.Types.Tag as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an instance (a virtual private server).
--
-- /See:/ 'mkInstance' smart constructor.
data Instance = Instance'
  { addOns :: Core.Maybe [Types.AddOn]
    -- ^ An array of objects representing the add-ons enabled on the instance.
  , arn :: Core.Maybe Types.NonEmptyString
    -- ^ The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
  , blueprintId :: Core.Maybe Types.NonEmptyString
    -- ^ The blueprint ID (e.g., @os_amlinux_2016_03@ ).
  , blueprintName :: Core.Maybe Types.NonEmptyString
    -- ^ The friendly name of the blueprint (e.g., @Amazon Linux@ ).
  , bundleId :: Core.Maybe Types.NonEmptyString
    -- ^ The bundle for the instance (e.g., @micro_1_0@ ).
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the instance was created (e.g., @1479734909.17@ ) in Unix time format.
  , hardware :: Core.Maybe Types.InstanceHardware
    -- ^ The size of the vCPU and the amount of RAM for the instance.
  , ipv6Address :: Core.Maybe Types.IpV6Address
    -- ^ The IPv6 address of the instance.
  , isStaticIp :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether this instance has a static IP assigned to it.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The region name and Availability Zone where the instance is located.
  , name :: Core.Maybe Types.ResourceName
    -- ^ The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
  , networking :: Core.Maybe Types.InstanceNetworking
    -- ^ Information about the public ports and monthly data transfer rates for the instance.
  , privateIpAddress :: Core.Maybe Types.IpAddress
    -- ^ The private IP address of the instance.
  , publicIpAddress :: Core.Maybe Types.IpAddress
    -- ^ The public IP address of the instance.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of resource (usually @Instance@ ).
  , sshKeyName :: Core.Maybe Types.ResourceName
    -- ^ The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
  , state :: Core.Maybe Types.InstanceState
    -- ^ The status code and the state (e.g., @running@ ) for the instance.
  , supportCode :: Core.Maybe Core.Text
    -- ^ The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
  , username :: Core.Maybe Types.NonEmptyString
    -- ^ The user name for connecting to the instance (e.g., @ec2-user@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Instance' value with any optional fields omitted.
mkInstance
    :: Instance
mkInstance
  = Instance'{addOns = Core.Nothing, arn = Core.Nothing,
              blueprintId = Core.Nothing, blueprintName = Core.Nothing,
              bundleId = Core.Nothing, createdAt = Core.Nothing,
              hardware = Core.Nothing, ipv6Address = Core.Nothing,
              isStaticIp = Core.Nothing, location = Core.Nothing,
              name = Core.Nothing, networking = Core.Nothing,
              privateIpAddress = Core.Nothing, publicIpAddress = Core.Nothing,
              resourceType = Core.Nothing, sshKeyName = Core.Nothing,
              state = Core.Nothing, supportCode = Core.Nothing,
              tags = Core.Nothing, username = Core.Nothing}

-- | An array of objects representing the add-ons enabled on the instance.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAddOns :: Lens.Lens' Instance (Core.Maybe [Types.AddOn])
iAddOns = Lens.field @"addOns"
{-# INLINEABLE iAddOns #-}
{-# DEPRECATED addOns "Use generic-lens or generic-optics with 'addOns' instead"  #-}

-- | The Amazon Resource Name (ARN) of the instance (e.g., @arn:aws:lightsail:us-east-2:123456789101:Instance/244ad76f-8aad-4741-809f-12345EXAMPLE@ ).
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iArn :: Lens.Lens' Instance (Core.Maybe Types.NonEmptyString)
iArn = Lens.field @"arn"
{-# INLINEABLE iArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The blueprint ID (e.g., @os_amlinux_2016_03@ ).
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlueprintId :: Lens.Lens' Instance (Core.Maybe Types.NonEmptyString)
iBlueprintId = Lens.field @"blueprintId"
{-# INLINEABLE iBlueprintId #-}
{-# DEPRECATED blueprintId "Use generic-lens or generic-optics with 'blueprintId' instead"  #-}

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
--
-- /Note:/ Consider using 'blueprintName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBlueprintName :: Lens.Lens' Instance (Core.Maybe Types.NonEmptyString)
iBlueprintName = Lens.field @"blueprintName"
{-# INLINEABLE iBlueprintName #-}
{-# DEPRECATED blueprintName "Use generic-lens or generic-optics with 'blueprintName' instead"  #-}

-- | The bundle for the instance (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iBundleId :: Lens.Lens' Instance (Core.Maybe Types.NonEmptyString)
iBundleId = Lens.field @"bundleId"
{-# INLINEABLE iBundleId #-}
{-# DEPRECATED bundleId "Use generic-lens or generic-optics with 'bundleId' instead"  #-}

-- | The timestamp when the instance was created (e.g., @1479734909.17@ ) in Unix time format.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreatedAt :: Lens.Lens' Instance (Core.Maybe Core.NominalDiffTime)
iCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE iCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The size of the vCPU and the amount of RAM for the instance.
--
-- /Note:/ Consider using 'hardware' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iHardware :: Lens.Lens' Instance (Core.Maybe Types.InstanceHardware)
iHardware = Lens.field @"hardware"
{-# INLINEABLE iHardware #-}
{-# DEPRECATED hardware "Use generic-lens or generic-optics with 'hardware' instead"  #-}

-- | The IPv6 address of the instance.
--
-- /Note:/ Consider using 'ipv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIpv6Address :: Lens.Lens' Instance (Core.Maybe Types.IpV6Address)
iIpv6Address = Lens.field @"ipv6Address"
{-# INLINEABLE iIpv6Address #-}
{-# DEPRECATED ipv6Address "Use generic-lens or generic-optics with 'ipv6Address' instead"  #-}

-- | A Boolean value indicating whether this instance has a static IP assigned to it.
--
-- /Note:/ Consider using 'isStaticIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iIsStaticIp :: Lens.Lens' Instance (Core.Maybe Core.Bool)
iIsStaticIp = Lens.field @"isStaticIp"
{-# INLINEABLE iIsStaticIp #-}
{-# DEPRECATED isStaticIp "Use generic-lens or generic-optics with 'isStaticIp' instead"  #-}

-- | The region name and Availability Zone where the instance is located.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iLocation :: Lens.Lens' Instance (Core.Maybe Types.ResourceLocation)
iLocation = Lens.field @"location"
{-# INLINEABLE iLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The name the user gave the instance (e.g., @Amazon_Linux-1GB-Ohio-1@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iName :: Lens.Lens' Instance (Core.Maybe Types.ResourceName)
iName = Lens.field @"name"
{-# INLINEABLE iName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Information about the public ports and monthly data transfer rates for the instance.
--
-- /Note:/ Consider using 'networking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iNetworking :: Lens.Lens' Instance (Core.Maybe Types.InstanceNetworking)
iNetworking = Lens.field @"networking"
{-# INLINEABLE iNetworking #-}
{-# DEPRECATED networking "Use generic-lens or generic-optics with 'networking' instead"  #-}

-- | The private IP address of the instance.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPrivateIpAddress :: Lens.Lens' Instance (Core.Maybe Types.IpAddress)
iPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE iPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | The public IP address of the instance.
--
-- /Note:/ Consider using 'publicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iPublicIpAddress :: Lens.Lens' Instance (Core.Maybe Types.IpAddress)
iPublicIpAddress = Lens.field @"publicIpAddress"
{-# INLINEABLE iPublicIpAddress #-}
{-# DEPRECATED publicIpAddress "Use generic-lens or generic-optics with 'publicIpAddress' instead"  #-}

-- | The type of resource (usually @Instance@ ).
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iResourceType :: Lens.Lens' Instance (Core.Maybe Types.ResourceType)
iResourceType = Lens.field @"resourceType"
{-# INLINEABLE iResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The name of the SSH key being used to connect to the instance (e.g., @LightsailDefaultKeyPair@ ).
--
-- /Note:/ Consider using 'sshKeyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSshKeyName :: Lens.Lens' Instance (Core.Maybe Types.ResourceName)
iSshKeyName = Lens.field @"sshKeyName"
{-# INLINEABLE iSshKeyName #-}
{-# DEPRECATED sshKeyName "Use generic-lens or generic-optics with 'sshKeyName' instead"  #-}

-- | The status code and the state (e.g., @running@ ) for the instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iState :: Lens.Lens' Instance (Core.Maybe Types.InstanceState)
iState = Lens.field @"state"
{-# INLINEABLE iState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The support code. Include this code in your email to support when you have questions about an instance or another resource in Lightsail. This code enables our support team to look up your Lightsail information more easily.
--
-- /Note:/ Consider using 'supportCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iSupportCode :: Lens.Lens' Instance (Core.Maybe Core.Text)
iSupportCode = Lens.field @"supportCode"
{-# INLINEABLE iSupportCode #-}
{-# DEPRECATED supportCode "Use generic-lens or generic-optics with 'supportCode' instead"  #-}

-- | The tag keys and optional values for the resource. For more information about tags in Lightsail, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Lightsail Dev Guide> .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iTags :: Lens.Lens' Instance (Core.Maybe [Types.Tag])
iTags = Lens.field @"tags"
{-# INLINEABLE iTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The user name for connecting to the instance (e.g., @ec2-user@ ).
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iUsername :: Lens.Lens' Instance (Core.Maybe Types.NonEmptyString)
iUsername = Lens.field @"username"
{-# INLINEABLE iUsername #-}
{-# DEPRECATED username "Use generic-lens or generic-optics with 'username' instead"  #-}

instance Core.FromJSON Instance where
        parseJSON
          = Core.withObject "Instance" Core.$
              \ x ->
                Instance' Core.<$>
                  (x Core..:? "addOns") Core.<*> x Core..:? "arn" Core.<*>
                    x Core..:? "blueprintId"
                    Core.<*> x Core..:? "blueprintName"
                    Core.<*> x Core..:? "bundleId"
                    Core.<*> x Core..:? "createdAt"
                    Core.<*> x Core..:? "hardware"
                    Core.<*> x Core..:? "ipv6Address"
                    Core.<*> x Core..:? "isStaticIp"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "networking"
                    Core.<*> x Core..:? "privateIpAddress"
                    Core.<*> x Core..:? "publicIpAddress"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "sshKeyName"
                    Core.<*> x Core..:? "state"
                    Core.<*> x Core..:? "supportCode"
                    Core.<*> x Core..:? "tags"
                    Core.<*> x Core..:? "username"
