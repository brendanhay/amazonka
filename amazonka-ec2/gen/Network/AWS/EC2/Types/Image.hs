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
-- Module      : Network.AWS.EC2.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Image where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ArchitectureValues
import Network.AWS.EC2.Types.BlockDeviceMapping
import Network.AWS.EC2.Types.DeviceType
import Network.AWS.EC2.Types.HypervisorType
import Network.AWS.EC2.Types.ImageState
import Network.AWS.EC2.Types.ImageTypeValues
import Network.AWS.EC2.Types.PlatformValues
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.StateReason
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.VirtualizationType
import qualified Network.AWS.Lens as Lens

-- | Describes an image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
    platform :: Core.Maybe PlatformValues,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Core.Maybe Core.Text,
    -- | The RAM disk associated with the image, if any. Only applicable for
    -- machine images.
    ramdiskId :: Core.Maybe Core.Text,
    -- | The reason for the state change.
    stateReason :: Core.Maybe StateReason,
    -- | The operation of the Amazon EC2 instance and the billing code that is
    -- associated with the AMI. @usageOperation@ corresponds to the
    -- <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem\/Operation>
    -- column on your AWS Cost and Usage Report and in the
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API>.
    -- For the list of @UsageOperation@ codes, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    usageOperation :: Core.Maybe Core.Text,
    -- | The date and time the image was created.
    creationDate :: Core.Maybe Core.Text,
    -- | Any product codes associated with the AMI.
    productCodes :: Core.Maybe [ProductCode],
    -- | The platform details associated with the billing code of the AMI. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    platformDetails :: Core.Maybe Core.Text,
    -- | The name of the AMI that was provided during image creation.
    name :: Core.Maybe Core.Text,
    -- | Any tags assigned to the image.
    tags :: Core.Maybe [Tag],
    -- | Specifies whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Core.Maybe Core.Text,
    -- | Any block device mapping entries.
    blockDeviceMappings :: Core.Maybe [BlockDeviceMapping],
    -- | The kernel associated with the image, if any. Only applicable for
    -- machine images.
    kernelId :: Core.Maybe Core.Text,
    -- | The description of the AMI that was provided during image creation.
    description :: Core.Maybe Core.Text,
    -- | The AWS account alias (for example, @amazon@, @self@) or the AWS account
    -- ID of the AMI owner.
    imageOwnerAlias :: Core.Maybe Core.Text,
    -- | Specifies whether enhanced networking with ENA is enabled.
    enaSupport :: Core.Maybe Core.Bool,
    -- | The ID of the AMI.
    imageId :: Core.Text,
    -- | The location of the AMI.
    imageLocation :: Core.Text,
    -- | The current state of the AMI. If the state is @available@, the image is
    -- successfully registered and can be used to launch an instance.
    state :: ImageState,
    -- | The AWS account ID of the image owner.
    ownerId :: Core.Text,
    -- | Indicates whether the image has public launch permissions. The value is
    -- @true@ if this image has public launch permissions or @false@ if it has
    -- only implicit and explicit launch permissions.
    public :: Core.Bool,
    -- | The architecture of the image.
    architecture :: ArchitectureValues,
    -- | The type of image.
    imageType :: ImageTypeValues,
    -- | The type of root device used by the AMI. The AMI can use an EBS volume
    -- or an instance store volume.
    rootDeviceType :: DeviceType,
    -- | The type of virtualization of the AMI.
    virtualizationType :: VirtualizationType,
    -- | The hypervisor type of the image.
    hypervisor :: HypervisorType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Image' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'image_platform' - This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
--
-- 'rootDeviceName', 'image_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'ramdiskId', 'image_ramdiskId' - The RAM disk associated with the image, if any. Only applicable for
-- machine images.
--
-- 'stateReason', 'image_stateReason' - The reason for the state change.
--
-- 'usageOperation', 'image_usageOperation' - The operation of the Amazon EC2 instance and the billing code that is
-- associated with the AMI. @usageOperation@ corresponds to the
-- <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem\/Operation>
-- column on your AWS Cost and Usage Report and in the
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API>.
-- For the list of @UsageOperation@ codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'creationDate', 'image_creationDate' - The date and time the image was created.
--
-- 'productCodes', 'image_productCodes' - Any product codes associated with the AMI.
--
-- 'platformDetails', 'image_platformDetails' - The platform details associated with the billing code of the AMI. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'name', 'image_name' - The name of the AMI that was provided during image creation.
--
-- 'tags', 'image_tags' - Any tags assigned to the image.
--
-- 'sriovNetSupport', 'image_sriovNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'blockDeviceMappings', 'image_blockDeviceMappings' - Any block device mapping entries.
--
-- 'kernelId', 'image_kernelId' - The kernel associated with the image, if any. Only applicable for
-- machine images.
--
-- 'description', 'image_description' - The description of the AMI that was provided during image creation.
--
-- 'imageOwnerAlias', 'image_imageOwnerAlias' - The AWS account alias (for example, @amazon@, @self@) or the AWS account
-- ID of the AMI owner.
--
-- 'enaSupport', 'image_enaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- 'imageId', 'image_imageId' - The ID of the AMI.
--
-- 'imageLocation', 'image_imageLocation' - The location of the AMI.
--
-- 'state', 'image_state' - The current state of the AMI. If the state is @available@, the image is
-- successfully registered and can be used to launch an instance.
--
-- 'ownerId', 'image_ownerId' - The AWS account ID of the image owner.
--
-- 'public', 'image_public' - Indicates whether the image has public launch permissions. The value is
-- @true@ if this image has public launch permissions or @false@ if it has
-- only implicit and explicit launch permissions.
--
-- 'architecture', 'image_architecture' - The architecture of the image.
--
-- 'imageType', 'image_imageType' - The type of image.
--
-- 'rootDeviceType', 'image_rootDeviceType' - The type of root device used by the AMI. The AMI can use an EBS volume
-- or an instance store volume.
--
-- 'virtualizationType', 'image_virtualizationType' - The type of virtualization of the AMI.
--
-- 'hypervisor', 'image_hypervisor' - The hypervisor type of the image.
newImage ::
  -- | 'imageId'
  Core.Text ->
  -- | 'imageLocation'
  Core.Text ->
  -- | 'state'
  ImageState ->
  -- | 'ownerId'
  Core.Text ->
  -- | 'public'
  Core.Bool ->
  -- | 'architecture'
  ArchitectureValues ->
  -- | 'imageType'
  ImageTypeValues ->
  -- | 'rootDeviceType'
  DeviceType ->
  -- | 'virtualizationType'
  VirtualizationType ->
  -- | 'hypervisor'
  HypervisorType ->
  Image
newImage
  pImageId_
  pImageLocation_
  pState_
  pOwnerId_
  pPublic_
  pArchitecture_
  pImageType_
  pRootDeviceType_
  pVirtualizationType_
  pHypervisor_ =
    Image'
      { platform = Core.Nothing,
        rootDeviceName = Core.Nothing,
        ramdiskId = Core.Nothing,
        stateReason = Core.Nothing,
        usageOperation = Core.Nothing,
        creationDate = Core.Nothing,
        productCodes = Core.Nothing,
        platformDetails = Core.Nothing,
        name = Core.Nothing,
        tags = Core.Nothing,
        sriovNetSupport = Core.Nothing,
        blockDeviceMappings = Core.Nothing,
        kernelId = Core.Nothing,
        description = Core.Nothing,
        imageOwnerAlias = Core.Nothing,
        enaSupport = Core.Nothing,
        imageId = pImageId_,
        imageLocation = pImageLocation_,
        state = pState_,
        ownerId = pOwnerId_,
        public = pPublic_,
        architecture = pArchitecture_,
        imageType = pImageType_,
        rootDeviceType = pRootDeviceType_,
        virtualizationType = pVirtualizationType_,
        hypervisor = pHypervisor_
      }

-- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
image_platform :: Lens.Lens' Image (Core.Maybe PlatformValues)
image_platform = Lens.lens (\Image' {platform} -> platform) (\s@Image' {} a -> s {platform = a} :: Image)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
image_rootDeviceName :: Lens.Lens' Image (Core.Maybe Core.Text)
image_rootDeviceName = Lens.lens (\Image' {rootDeviceName} -> rootDeviceName) (\s@Image' {} a -> s {rootDeviceName = a} :: Image)

-- | The RAM disk associated with the image, if any. Only applicable for
-- machine images.
image_ramdiskId :: Lens.Lens' Image (Core.Maybe Core.Text)
image_ramdiskId = Lens.lens (\Image' {ramdiskId} -> ramdiskId) (\s@Image' {} a -> s {ramdiskId = a} :: Image)

-- | The reason for the state change.
image_stateReason :: Lens.Lens' Image (Core.Maybe StateReason)
image_stateReason = Lens.lens (\Image' {stateReason} -> stateReason) (\s@Image' {} a -> s {stateReason = a} :: Image)

-- | The operation of the Amazon EC2 instance and the billing code that is
-- associated with the AMI. @usageOperation@ corresponds to the
-- <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem\/Operation>
-- column on your AWS Cost and Usage Report and in the
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html AWS Price List API>.
-- For the list of @UsageOperation@ codes, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html#billing-info Platform Details and Usage Operation Billing Codes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_usageOperation :: Lens.Lens' Image (Core.Maybe Core.Text)
image_usageOperation = Lens.lens (\Image' {usageOperation} -> usageOperation) (\s@Image' {} a -> s {usageOperation = a} :: Image)

-- | The date and time the image was created.
image_creationDate :: Lens.Lens' Image (Core.Maybe Core.Text)
image_creationDate = Lens.lens (\Image' {creationDate} -> creationDate) (\s@Image' {} a -> s {creationDate = a} :: Image)

-- | Any product codes associated with the AMI.
image_productCodes :: Lens.Lens' Image (Core.Maybe [ProductCode])
image_productCodes = Lens.lens (\Image' {productCodes} -> productCodes) (\s@Image' {} a -> s {productCodes = a} :: Image) Core.. Lens.mapping Lens._Coerce

-- | The platform details associated with the billing code of the AMI. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining Billing Information>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_platformDetails :: Lens.Lens' Image (Core.Maybe Core.Text)
image_platformDetails = Lens.lens (\Image' {platformDetails} -> platformDetails) (\s@Image' {} a -> s {platformDetails = a} :: Image)

-- | The name of the AMI that was provided during image creation.
image_name :: Lens.Lens' Image (Core.Maybe Core.Text)
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

-- | Any tags assigned to the image.
image_tags :: Lens.Lens' Image (Core.Maybe [Tag])
image_tags = Lens.lens (\Image' {tags} -> tags) (\s@Image' {} a -> s {tags = a} :: Image) Core.. Lens.mapping Lens._Coerce

-- | Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
image_sriovNetSupport :: Lens.Lens' Image (Core.Maybe Core.Text)
image_sriovNetSupport = Lens.lens (\Image' {sriovNetSupport} -> sriovNetSupport) (\s@Image' {} a -> s {sriovNetSupport = a} :: Image)

-- | Any block device mapping entries.
image_blockDeviceMappings :: Lens.Lens' Image (Core.Maybe [BlockDeviceMapping])
image_blockDeviceMappings = Lens.lens (\Image' {blockDeviceMappings} -> blockDeviceMappings) (\s@Image' {} a -> s {blockDeviceMappings = a} :: Image) Core.. Lens.mapping Lens._Coerce

-- | The kernel associated with the image, if any. Only applicable for
-- machine images.
image_kernelId :: Lens.Lens' Image (Core.Maybe Core.Text)
image_kernelId = Lens.lens (\Image' {kernelId} -> kernelId) (\s@Image' {} a -> s {kernelId = a} :: Image)

-- | The description of the AMI that was provided during image creation.
image_description :: Lens.Lens' Image (Core.Maybe Core.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | The AWS account alias (for example, @amazon@, @self@) or the AWS account
-- ID of the AMI owner.
image_imageOwnerAlias :: Lens.Lens' Image (Core.Maybe Core.Text)
image_imageOwnerAlias = Lens.lens (\Image' {imageOwnerAlias} -> imageOwnerAlias) (\s@Image' {} a -> s {imageOwnerAlias = a} :: Image)

-- | Specifies whether enhanced networking with ENA is enabled.
image_enaSupport :: Lens.Lens' Image (Core.Maybe Core.Bool)
image_enaSupport = Lens.lens (\Image' {enaSupport} -> enaSupport) (\s@Image' {} a -> s {enaSupport = a} :: Image)

-- | The ID of the AMI.
image_imageId :: Lens.Lens' Image Core.Text
image_imageId = Lens.lens (\Image' {imageId} -> imageId) (\s@Image' {} a -> s {imageId = a} :: Image)

-- | The location of the AMI.
image_imageLocation :: Lens.Lens' Image Core.Text
image_imageLocation = Lens.lens (\Image' {imageLocation} -> imageLocation) (\s@Image' {} a -> s {imageLocation = a} :: Image)

-- | The current state of the AMI. If the state is @available@, the image is
-- successfully registered and can be used to launch an instance.
image_state :: Lens.Lens' Image ImageState
image_state = Lens.lens (\Image' {state} -> state) (\s@Image' {} a -> s {state = a} :: Image)

-- | The AWS account ID of the image owner.
image_ownerId :: Lens.Lens' Image Core.Text
image_ownerId = Lens.lens (\Image' {ownerId} -> ownerId) (\s@Image' {} a -> s {ownerId = a} :: Image)

-- | Indicates whether the image has public launch permissions. The value is
-- @true@ if this image has public launch permissions or @false@ if it has
-- only implicit and explicit launch permissions.
image_public :: Lens.Lens' Image Core.Bool
image_public = Lens.lens (\Image' {public} -> public) (\s@Image' {} a -> s {public = a} :: Image)

-- | The architecture of the image.
image_architecture :: Lens.Lens' Image ArchitectureValues
image_architecture = Lens.lens (\Image' {architecture} -> architecture) (\s@Image' {} a -> s {architecture = a} :: Image)

-- | The type of image.
image_imageType :: Lens.Lens' Image ImageTypeValues
image_imageType = Lens.lens (\Image' {imageType} -> imageType) (\s@Image' {} a -> s {imageType = a} :: Image)

-- | The type of root device used by the AMI. The AMI can use an EBS volume
-- or an instance store volume.
image_rootDeviceType :: Lens.Lens' Image DeviceType
image_rootDeviceType = Lens.lens (\Image' {rootDeviceType} -> rootDeviceType) (\s@Image' {} a -> s {rootDeviceType = a} :: Image)

-- | The type of virtualization of the AMI.
image_virtualizationType :: Lens.Lens' Image VirtualizationType
image_virtualizationType = Lens.lens (\Image' {virtualizationType} -> virtualizationType) (\s@Image' {} a -> s {virtualizationType = a} :: Image)

-- | The hypervisor type of the image.
image_hypervisor :: Lens.Lens' Image HypervisorType
image_hypervisor = Lens.lens (\Image' {hypervisor} -> hypervisor) (\s@Image' {} a -> s {hypervisor = a} :: Image)

instance Core.FromXML Image where
  parseXML x =
    Image'
      Core.<$> (x Core..@? "platform")
      Core.<*> (x Core..@? "rootDeviceName")
      Core.<*> (x Core..@? "ramdiskId")
      Core.<*> (x Core..@? "stateReason")
      Core.<*> (x Core..@? "usageOperation")
      Core.<*> (x Core..@? "creationDate")
      Core.<*> ( x Core..@? "productCodes" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "platformDetails")
      Core.<*> (x Core..@? "name")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "sriovNetSupport")
      Core.<*> ( x Core..@? "blockDeviceMapping" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "kernelId")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "imageOwnerAlias")
      Core.<*> (x Core..@? "enaSupport")
      Core.<*> (x Core..@ "imageId")
      Core.<*> (x Core..@ "imageLocation")
      Core.<*> (x Core..@ "imageState")
      Core.<*> (x Core..@ "imageOwnerId")
      Core.<*> (x Core..@ "isPublic")
      Core.<*> (x Core..@ "architecture")
      Core.<*> (x Core..@ "imageType")
      Core.<*> (x Core..@ "rootDeviceType")
      Core.<*> (x Core..@ "virtualizationType")
      Core.<*> (x Core..@ "hypervisor")

instance Core.Hashable Image

instance Core.NFData Image
