{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Image
  ( Image (..),

    -- * Smart constructor
    mkImage,

    -- * Lenses
    ifArchitecture,
    ifBlockDeviceMappings,
    ifCreationDate,
    ifDescription,
    ifEnaSupport,
    ifHypervisor,
    ifImageId,
    ifImageLocation,
    ifImageOwnerAlias,
    ifImageType,
    ifKernelId,
    ifName,
    ifOwnerId,
    ifPlatform,
    ifPlatformDetails,
    ifProductCodes,
    ifPublic,
    ifRamdiskId,
    ifRootDeviceName,
    ifRootDeviceType,
    ifSriovNetSupport,
    ifState,
    ifStateReason,
    ifTags,
    ifUsageOperation,
    ifVirtualizationType,
  )
where

import qualified Network.AWS.EC2.Types.ArchitectureValues as Types
import qualified Network.AWS.EC2.Types.BlockDeviceMapping as Types
import qualified Network.AWS.EC2.Types.CreationDate as Types
import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.EC2.Types.DeviceType as Types
import qualified Network.AWS.EC2.Types.HypervisorType as Types
import qualified Network.AWS.EC2.Types.ImageId as Types
import qualified Network.AWS.EC2.Types.ImageLocation as Types
import qualified Network.AWS.EC2.Types.ImageOwnerAlias as Types
import qualified Network.AWS.EC2.Types.ImageState as Types
import qualified Network.AWS.EC2.Types.ImageTypeValues as Types
import qualified Network.AWS.EC2.Types.KernelId as Types
import qualified Network.AWS.EC2.Types.Name as Types
import qualified Network.AWS.EC2.Types.OwnerId as Types
import qualified Network.AWS.EC2.Types.PlatformDetails as Types
import qualified Network.AWS.EC2.Types.PlatformValues as Types
import qualified Network.AWS.EC2.Types.ProductCode as Types
import qualified Network.AWS.EC2.Types.RamdiskId as Types
import qualified Network.AWS.EC2.Types.RootDeviceName as Types
import qualified Network.AWS.EC2.Types.SriovNetSupport as Types
import qualified Network.AWS.EC2.Types.StateReason as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.EC2.Types.UsageOperation as Types
import qualified Network.AWS.EC2.Types.VirtualizationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an image.
--
-- /See:/ 'mkImage' smart constructor.
data Image = Image'
  { -- | The architecture of the image.
    architecture :: Types.ArchitectureValues,
    -- | Any block device mapping entries.
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | The date and time the image was created.
    creationDate :: Core.Maybe Types.CreationDate,
    -- | The description of the AMI that was provided during image creation.
    description :: Core.Maybe Types.Description,
    -- | Specifies whether enhanced networking with ENA is enabled.
    enaSupport :: Core.Maybe Core.Bool,
    -- | The hypervisor type of the image.
    hypervisor :: Types.HypervisorType,
    -- | The ID of the AMI.
    imageId :: Types.ImageId,
    -- | The location of the AMI.
    imageLocation :: Types.ImageLocation,
    -- | The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
    imageOwnerAlias :: Core.Maybe Types.ImageOwnerAlias,
    -- | The type of image.
    imageType :: Types.ImageTypeValues,
    -- | The kernel associated with the image, if any. Only applicable for machine images.
    kernelId :: Core.Maybe Types.KernelId,
    -- | The name of the AMI that was provided during image creation.
    name :: Core.Maybe Types.Name,
    -- | The AWS account ID of the image owner.
    ownerId :: Types.OwnerId,
    -- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
    platform :: Core.Maybe Types.PlatformValues,
    -- | The platform details associated with the billing code of the AMI. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information> in the /Amazon Elastic Compute Cloud User Guide/ .
    platformDetails :: Core.Maybe Types.PlatformDetails,
    -- | Any product codes associated with the AMI.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
    public :: Core.Bool,
    -- | The RAM disk associated with the image, if any. Only applicable for machine images.
    ramdiskId :: Core.Maybe Types.RamdiskId,
    -- | The device name of the root device volume (for example, @/dev/sda1@ ).
    rootDeviceName :: Core.Maybe Types.RootDeviceName,
    -- | The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
    rootDeviceType :: Types.DeviceType,
    -- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
    sriovNetSupport :: Core.Maybe Types.SriovNetSupport,
    -- | The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
    state :: Types.ImageState,
    -- | The reason for the state change.
    stateReason :: Core.Maybe Types.StateReason,
    -- | Any tags assigned to the image.
    tags :: Core.Maybe [Types.Tag],
    -- | The operation of the Amazon EC2 instance and the billing code that is associated with the AMI. @usageOperation@ corresponds to the <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem/Operation> column on your AWS Cost and Usage Report and in the <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API> . For the list of @UsageOperation@ codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes> in the /Amazon Elastic Compute Cloud User Guide/ .
    usageOperation :: Core.Maybe Types.UsageOperation,
    -- | The type of virtualization of the AMI.
    virtualizationType :: Types.VirtualizationType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Image' value with any optional fields omitted.
mkImage ::
  -- | 'architecture'
  Types.ArchitectureValues ->
  -- | 'hypervisor'
  Types.HypervisorType ->
  -- | 'imageId'
  Types.ImageId ->
  -- | 'imageLocation'
  Types.ImageLocation ->
  -- | 'imageType'
  Types.ImageTypeValues ->
  -- | 'ownerId'
  Types.OwnerId ->
  -- | 'public'
  Core.Bool ->
  -- | 'rootDeviceType'
  Types.DeviceType ->
  -- | 'state'
  Types.ImageState ->
  -- | 'virtualizationType'
  Types.VirtualizationType ->
  Image
mkImage
  architecture
  hypervisor
  imageId
  imageLocation
  imageType
  ownerId
  public
  rootDeviceType
  state
  virtualizationType =
    Image'
      { architecture,
        blockDeviceMappings = Core.Nothing,
        creationDate = Core.Nothing,
        description = Core.Nothing,
        enaSupport = Core.Nothing,
        hypervisor,
        imageId,
        imageLocation,
        imageOwnerAlias = Core.Nothing,
        imageType,
        kernelId = Core.Nothing,
        name = Core.Nothing,
        ownerId,
        platform = Core.Nothing,
        platformDetails = Core.Nothing,
        productCodes = Core.Nothing,
        public,
        ramdiskId = Core.Nothing,
        rootDeviceName = Core.Nothing,
        rootDeviceType,
        sriovNetSupport = Core.Nothing,
        state,
        stateReason = Core.Nothing,
        tags = Core.Nothing,
        usageOperation = Core.Nothing,
        virtualizationType
      }

-- | The architecture of the image.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifArchitecture :: Lens.Lens' Image Types.ArchitectureValues
ifArchitecture = Lens.field @"architecture"
{-# DEPRECATED ifArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | Any block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifBlockDeviceMappings :: Lens.Lens' Image (Core.Maybe [Types.BlockDeviceMapping])
ifBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED ifBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The date and time the image was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifCreationDate :: Lens.Lens' Image (Core.Maybe Types.CreationDate)
ifCreationDate = Lens.field @"creationDate"
{-# DEPRECATED ifCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The description of the AMI that was provided during image creation.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifDescription :: Lens.Lens' Image (Core.Maybe Types.Description)
ifDescription = Lens.field @"description"
{-# DEPRECATED ifDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifEnaSupport :: Lens.Lens' Image (Core.Maybe Core.Bool)
ifEnaSupport = Lens.field @"enaSupport"
{-# DEPRECATED ifEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | The hypervisor type of the image.
--
-- /Note:/ Consider using 'hypervisor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifHypervisor :: Lens.Lens' Image Types.HypervisorType
ifHypervisor = Lens.field @"hypervisor"
{-# DEPRECATED ifHypervisor "Use generic-lens or generic-optics with 'hypervisor' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageId :: Lens.Lens' Image Types.ImageId
ifImageId = Lens.field @"imageId"
{-# DEPRECATED ifImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The location of the AMI.
--
-- /Note:/ Consider using 'imageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageLocation :: Lens.Lens' Image Types.ImageLocation
ifImageLocation = Lens.field @"imageLocation"
{-# DEPRECATED ifImageLocation "Use generic-lens or generic-optics with 'imageLocation' instead." #-}

-- | The AWS account alias (for example, @amazon@ , @self@ ) or the AWS account ID of the AMI owner.
--
-- /Note:/ Consider using 'imageOwnerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageOwnerAlias :: Lens.Lens' Image (Core.Maybe Types.ImageOwnerAlias)
ifImageOwnerAlias = Lens.field @"imageOwnerAlias"
{-# DEPRECATED ifImageOwnerAlias "Use generic-lens or generic-optics with 'imageOwnerAlias' instead." #-}

-- | The type of image.
--
-- /Note:/ Consider using 'imageType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifImageType :: Lens.Lens' Image Types.ImageTypeValues
ifImageType = Lens.field @"imageType"
{-# DEPRECATED ifImageType "Use generic-lens or generic-optics with 'imageType' instead." #-}

-- | The kernel associated with the image, if any. Only applicable for machine images.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifKernelId :: Lens.Lens' Image (Core.Maybe Types.KernelId)
ifKernelId = Lens.field @"kernelId"
{-# DEPRECATED ifKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The name of the AMI that was provided during image creation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifName :: Lens.Lens' Image (Core.Maybe Types.Name)
ifName = Lens.field @"name"
{-# DEPRECATED ifName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The AWS account ID of the image owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifOwnerId :: Lens.Lens' Image Types.OwnerId
ifOwnerId = Lens.field @"ownerId"
{-# DEPRECATED ifOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPlatform :: Lens.Lens' Image (Core.Maybe Types.PlatformValues)
ifPlatform = Lens.field @"platform"
{-# DEPRECATED ifPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The platform details associated with the billing code of the AMI. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'platformDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPlatformDetails :: Lens.Lens' Image (Core.Maybe Types.PlatformDetails)
ifPlatformDetails = Lens.field @"platformDetails"
{-# DEPRECATED ifPlatformDetails "Use generic-lens or generic-optics with 'platformDetails' instead." #-}

-- | Any product codes associated with the AMI.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifProductCodes :: Lens.Lens' Image (Core.Maybe [Types.ProductCode])
ifProductCodes = Lens.field @"productCodes"
{-# DEPRECATED ifProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | Indicates whether the image has public launch permissions. The value is @true@ if this image has public launch permissions or @false@ if it has only implicit and explicit launch permissions.
--
-- /Note:/ Consider using 'public' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifPublic :: Lens.Lens' Image Core.Bool
ifPublic = Lens.field @"public"
{-# DEPRECATED ifPublic "Use generic-lens or generic-optics with 'public' instead." #-}

-- | The RAM disk associated with the image, if any. Only applicable for machine images.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRamdiskId :: Lens.Lens' Image (Core.Maybe Types.RamdiskId)
ifRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED ifRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRootDeviceName :: Lens.Lens' Image (Core.Maybe Types.RootDeviceName)
ifRootDeviceName = Lens.field @"rootDeviceName"
{-# DEPRECATED ifRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | The type of root device used by the AMI. The AMI can use an EBS volume or an instance store volume.
--
-- /Note:/ Consider using 'rootDeviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifRootDeviceType :: Lens.Lens' Image Types.DeviceType
ifRootDeviceType = Lens.field @"rootDeviceType"
{-# DEPRECATED ifRootDeviceType "Use generic-lens or generic-optics with 'rootDeviceType' instead." #-}

-- | Specifies whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifSriovNetSupport :: Lens.Lens' Image (Core.Maybe Types.SriovNetSupport)
ifSriovNetSupport = Lens.field @"sriovNetSupport"
{-# DEPRECATED ifSriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The current state of the AMI. If the state is @available@ , the image is successfully registered and can be used to launch an instance.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifState :: Lens.Lens' Image Types.ImageState
ifState = Lens.field @"state"
{-# DEPRECATED ifState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The reason for the state change.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifStateReason :: Lens.Lens' Image (Core.Maybe Types.StateReason)
ifStateReason = Lens.field @"stateReason"
{-# DEPRECATED ifStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | Any tags assigned to the image.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifTags :: Lens.Lens' Image (Core.Maybe [Types.Tag])
ifTags = Lens.field @"tags"
{-# DEPRECATED ifTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The operation of the Amazon EC2 instance and the billing code that is associated with the AMI. @usageOperation@ corresponds to the <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem/Operation> column on your AWS Cost and Usage Report and in the <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API> . For the list of @UsageOperation@ codes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'usageOperation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifUsageOperation :: Lens.Lens' Image (Core.Maybe Types.UsageOperation)
ifUsageOperation = Lens.field @"usageOperation"
{-# DEPRECATED ifUsageOperation "Use generic-lens or generic-optics with 'usageOperation' instead." #-}

-- | The type of virtualization of the AMI.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifVirtualizationType :: Lens.Lens' Image Types.VirtualizationType
ifVirtualizationType = Lens.field @"virtualizationType"
{-# DEPRECATED ifVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

instance Core.FromXML Image where
  parseXML x =
    Image'
      Core.<$> (x Core..@ "architecture")
      Core.<*> ( x Core..@? "blockDeviceMapping"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "creationDate")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "enaSupport")
      Core.<*> (x Core..@ "hypervisor")
      Core.<*> (x Core..@ "imageId")
      Core.<*> (x Core..@ "imageLocation")
      Core.<*> (x Core..@? "imageOwnerAlias")
      Core.<*> (x Core..@ "imageType")
      Core.<*> (x Core..@? "kernelId")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@ "imageOwnerId")
      Core.<*> (x Core..@? "platform")
      Core.<*> (x Core..@? "platformDetails")
      Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@ "isPublic")
      Core.<*> (x Core..@? "ramdiskId")
      Core.<*> (x Core..@? "rootDeviceName")
      Core.<*> (x Core..@ "rootDeviceType")
      Core.<*> (x Core..@? "sriovNetSupport")
      Core.<*> (x Core..@ "imageState")
      Core.<*> (x Core..@? "stateReason")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "usageOperation")
      Core.<*> (x Core..@ "virtualizationType")
