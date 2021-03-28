{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Image
  ( Image (..)
  -- * Smart constructor
  , mkImage
  -- * Lenses
  , ifArchitecture
  , ifBlockDeviceMappings
  , ifCreationDate
  , ifDescription
  , ifEnaSupport
  , ifHypervisor
  , ifImageId
  , ifImageLocation
  , ifImageOwnerAlias
  , ifImageType
  , ifKernelId
  , ifName
  , ifOwnerId
  , ifPlatform
  , ifPlatformDetails
  , ifProductCodes
  , ifPublic
  , ifRamdiskId
  , ifRootDeviceName
  , ifRootDeviceType
  , ifSriovNetSupport
  , ifState
  , ifStateReason
  , ifTags
  , ifUsageOperation
  , ifVirtualizationType
  ) where

import qualified Network.AWS.EC2.Types.ArchitectureValues as Types
import qualified Network.AWS.EC2.Types.BlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.DeviceType as Types
import qualified Network.AWS.EC2.Types.HypervisorType as Types
import qualified Network.AWS.EC2.Types.ImageState as Types
import qualified Network.AWS.EC2.Types.ImageTypeValues as Types
import qualified Network.AWS.EC2.Types.PlatformValues as Types
import qualified Network.AWS.EC2.Types.ProductCode as Types
import qualified Network.AWS.EC2.Types.StateReason as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.VirtualizationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an image.
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { architecture :: Types.ArchitectureValues
    -- ^ The architecture of the image.
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ Any block device mapping entries.
  , creationDate :: Core.Maybe Core.Text
    -- ^ The date and time the image was created.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the AMI that was provided during image creation.
  , enaSupport :: Core.Maybe Core.Bool
    -- ^ Specifies whether enhanced networking with ENA is enabled.
  , hypervisor :: Types.HypervisorType
    -- ^ The hypervisor type of the image.
  , imageId :: Core.Text
    -- ^ The ID of the AMI.
  , imageLocation :: Core.Text
    -- ^ The location of the AMI.
  , imageOwnerAlias :: Core.Maybe Core.Text
    -- ^ The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
  , imageType :: Types.ImageTypeValues
    -- ^ The type of image.
  , kernelId :: Core.Maybe Core.Text
    -- ^ The kernel associated with the image, if any. Only applicable for machine images.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the AMI that was provided during image creation.
  , ownerId :: Core.Text
    -- ^ The AWS account ID of the image owner.
  , platform :: Core.Maybe Types.PlatformValues
    -- ^ This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
  , platformDetails :: Core.Maybe Core.Text
    -- ^ The platform details associated with the billing code of the AMI. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information> in the /Amazon Elastic Compute Cloud User Guide/ .
  , productCodes :: Core.Maybe [Types.ProductCode]
    -- ^ Any product codes associated with the AMI.
  , public :: Core.Bool
    -- ^ Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
  , ramdiskId :: Core.Maybe Core.Text
    -- ^ The RAM disk associated with the image, if any. Only applicable for machine images.
  , rootDeviceName :: Core.Maybe Core.Text
    -- ^ The device name of the root device volume (for example, @/dev/sda1@ ).
  , rootDeviceType :: Types.DeviceType
    -- ^ The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
  , sriovNetSupport :: Core.Maybe Core.Text
    -- ^ Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
  , state :: Types.ImageState
    -- ^ The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
  , stateReason :: Core.Maybe Types.StateReason
    -- ^ The reason for the state change.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the image.
  , usageOperation :: Core.Maybe Core.Text
    -- ^ The operation of the Amazon EC2 instance and the billing code that is associated with the AMI. @usageOperation@ corresponds to the <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem/Operation> column on your AWS Cost and Usage Report and in the <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API> . For the list of @UsageOperation@ codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes> in the /Amazon Elastic Compute Cloud User Guide/ .
  , virtualizationType :: Types.VirtualizationType
    -- ^ The type of virtualization of the AMI.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Image' value with any optional fields omitted.
mkImage
    :: Types.ArchitectureValues -- ^ 'architecture'
    -> Types.HypervisorType -- ^ 'hypervisor'
    -> Core.Text -- ^ 'imageId'
    -> Core.Text -- ^ 'imageLocation'
    -> Types.ImageTypeValues -- ^ 'imageType'
    -> Core.Text -- ^ 'ownerId'
    -> Core.Bool -- ^ 'public'
    -> Types.DeviceType -- ^ 'rootDeviceType'
    -> Types.ImageState -- ^ 'state'
    -> Types.VirtualizationType -- ^ 'virtualizationType'
    -> Image
mkImage architecture hypervisor imageId imageLocation imageType
  ownerId public rootDeviceType state virtualizationType
  = Image'{architecture, blockDeviceMappings = Core.Nothing,
           creationDate = Core.Nothing, description = Core.Nothing,
           enaSupport = Core.Nothing, hypervisor, imageId, imageLocation,
           imageOwnerAlias = Core.Nothing, imageType, kernelId = Core.Nothing,
           name = Core.Nothing, ownerId, platform = Core.Nothing,
           platformDetails = Core.Nothing, productCodes = Core.Nothing,
           public, ramdiskId = Core.Nothing, rootDeviceName = Core.Nothing,
           rootDeviceType, sriovNetSupport = Core.Nothing, state,
           stateReason = Core.Nothing, tags = Core.Nothing,
           usageOperation = Core.Nothing, virtualizationType}

-- | The architecture of the image.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifArchitecture :: Lens.Lens' Image Types.ArchitectureValues
ifArchitecture = Lens.field @"architecture"
{-# INLINEABLE ifArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | Any block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifBlockDeviceMappings :: Lens.Lens' Image (Core.Maybe [Types.BlockDeviceMapping])
ifBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE ifBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | The date and time the image was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifCreationDate :: Lens.Lens' Image (Core.Maybe Core.Text)
ifCreationDate = Lens.field @"creationDate"
{-# INLINEABLE ifCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The description of the AMI that was provided during image creation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDescription :: Lens.Lens' Image (Core.Maybe Core.Text)
ifDescription = Lens.field @"description"
{-# INLINEABLE ifDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifEnaSupport :: Lens.Lens' Image (Core.Maybe Core.Bool)
ifEnaSupport = Lens.field @"enaSupport"
{-# INLINEABLE ifEnaSupport #-}
{-# DEPRECATED enaSupport "Use generic-lens or generic-optics with 'enaSupport' instead"  #-}

-- | The hypervisor type of the image.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifHypervisor :: Lens.Lens' Image Types.HypervisorType
ifHypervisor = Lens.field @"hypervisor"
{-# INLINEABLE ifHypervisor #-}
{-# DEPRECATED hypervisor "Use generic-lens or generic-optics with 'hypervisor' instead"  #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageId :: Lens.Lens' Image Core.Text
ifImageId = Lens.field @"imageId"
{-# INLINEABLE ifImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The location of the AMI.
--
-- /Note:/ Consider using 'imageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageLocation :: Lens.Lens' Image Core.Text
ifImageLocation = Lens.field @"imageLocation"
{-# INLINEABLE ifImageLocation #-}
{-# DEPRECATED imageLocation "Use generic-lens or generic-optics with 'imageLocation' instead"  #-}

-- | The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
--
-- /Note:/ Consider using 'imageOwnerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageOwnerAlias :: Lens.Lens' Image (Core.Maybe Core.Text)
ifImageOwnerAlias = Lens.field @"imageOwnerAlias"
{-# INLINEABLE ifImageOwnerAlias #-}
{-# DEPRECATED imageOwnerAlias "Use generic-lens or generic-optics with 'imageOwnerAlias' instead"  #-}

-- | The type of image.
--
-- /Note:/ Consider using 'imageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageType :: Lens.Lens' Image Types.ImageTypeValues
ifImageType = Lens.field @"imageType"
{-# INLINEABLE ifImageType #-}
{-# DEPRECATED imageType "Use generic-lens or generic-optics with 'imageType' instead"  #-}

-- | The kernel associated with the image, if any. Only applicable for machine images.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifKernelId :: Lens.Lens' Image (Core.Maybe Core.Text)
ifKernelId = Lens.field @"kernelId"
{-# INLINEABLE ifKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The name of the AMI that was provided during image creation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifName :: Lens.Lens' Image (Core.Maybe Core.Text)
ifName = Lens.field @"name"
{-# INLINEABLE ifName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The AWS account ID of the image owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifOwnerId :: Lens.Lens' Image Core.Text
ifOwnerId = Lens.field @"ownerId"
{-# INLINEABLE ifOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPlatform :: Lens.Lens' Image (Core.Maybe Types.PlatformValues)
ifPlatform = Lens.field @"platform"
{-# INLINEABLE ifPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

-- | The platform details associated with the billing code of the AMI. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'platformDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPlatformDetails :: Lens.Lens' Image (Core.Maybe Core.Text)
ifPlatformDetails = Lens.field @"platformDetails"
{-# INLINEABLE ifPlatformDetails #-}
{-# DEPRECATED platformDetails "Use generic-lens or generic-optics with 'platformDetails' instead"  #-}

-- | Any product codes associated with the AMI.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifProductCodes :: Lens.Lens' Image (Core.Maybe [Types.ProductCode])
ifProductCodes = Lens.field @"productCodes"
{-# INLINEABLE ifProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
--
-- /Note:/ Consider using 'public' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPublic :: Lens.Lens' Image Core.Bool
ifPublic = Lens.field @"public"
{-# INLINEABLE ifPublic #-}
{-# DEPRECATED public "Use generic-lens or generic-optics with 'public' instead"  #-}

-- | The RAM disk associated with the image, if any. Only applicable for machine images.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRamdiskId :: Lens.Lens' Image (Core.Maybe Core.Text)
ifRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE ifRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRootDeviceName :: Lens.Lens' Image (Core.Maybe Core.Text)
ifRootDeviceName = Lens.field @"rootDeviceName"
{-# INLINEABLE ifRootDeviceName #-}
{-# DEPRECATED rootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead"  #-}

-- | The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRootDeviceType :: Lens.Lens' Image Types.DeviceType
ifRootDeviceType = Lens.field @"rootDeviceType"
{-# INLINEABLE ifRootDeviceType #-}
{-# DEPRECATED rootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead"  #-}

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifSriovNetSupport :: Lens.Lens' Image (Core.Maybe Core.Text)
ifSriovNetSupport = Lens.field @"sriovNetSupport"
{-# INLINEABLE ifSriovNetSupport #-}
{-# DEPRECATED sriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead"  #-}

-- | The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifState :: Lens.Lens' Image Types.ImageState
ifState = Lens.field @"state"
{-# INLINEABLE ifState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The reason for the state change.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifStateReason :: Lens.Lens' Image (Core.Maybe Types.StateReason)
ifStateReason = Lens.field @"stateReason"
{-# INLINEABLE ifStateReason #-}
{-# DEPRECATED stateReason "Use generic-lens or generic-optics with 'stateReason' instead"  #-}

-- | Any tags assigned to the image.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTags :: Lens.Lens' Image (Core.Maybe [Types.Tag])
ifTags = Lens.field @"tags"
{-# INLINEABLE ifTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The operation of the Amazon EC2 instance and the billing code that is associated with the AMI. @usageOperation@ corresponds to the <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem/Operation> column on your AWS Cost and Usage Report and in the <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API> . For the list of @UsageOperation@ codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'usageOperation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifUsageOperation :: Lens.Lens' Image (Core.Maybe Core.Text)
ifUsageOperation = Lens.field @"usageOperation"
{-# INLINEABLE ifUsageOperation #-}
{-# DEPRECATED usageOperation "Use generic-lens or generic-optics with 'usageOperation' instead"  #-}

-- | The type of virtualization of the AMI.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifVirtualizationType :: Lens.Lens' Image Types.VirtualizationType
ifVirtualizationType = Lens.field @"virtualizationType"
{-# INLINEABLE ifVirtualizationType #-}
{-# DEPRECATED virtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead"  #-}

instance Core.FromXML Image where
        parseXML x
          = Image' Core.<$>
              (x Core..@ "architecture") Core.<*>
                x Core..@? "blockDeviceMapping" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "creationDate"
                Core.<*> x Core..@? "description"
                Core.<*> x Core..@? "enaSupport"
                Core.<*> x Core..@ "hypervisor"
                Core.<*> x Core..@ "imageId"
                Core.<*> x Core..@ "imageLocation"
                Core.<*> x Core..@? "imageOwnerAlias"
                Core.<*> x Core..@ "imageType"
                Core.<*> x Core..@? "kernelId"
                Core.<*> x Core..@? "name"
                Core.<*> x Core..@ "imageOwnerId"
                Core.<*> x Core..@? "platform"
                Core.<*> x Core..@? "platformDetails"
                Core.<*>
                x Core..@? "productCodes" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@ "isPublic"
                Core.<*> x Core..@? "ramdiskId"
                Core.<*> x Core..@? "rootDeviceName"
                Core.<*> x Core..@ "rootDeviceType"
                Core.<*> x Core..@? "sriovNetSupport"
                Core.<*> x Core..@ "imageState"
                Core.<*> x Core..@? "stateReason"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "usageOperation"
                Core.<*> x Core..@ "virtualizationType"
