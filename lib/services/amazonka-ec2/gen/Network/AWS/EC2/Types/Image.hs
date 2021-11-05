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
-- Module      : Amazonka.EC2.Types.Image
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Image where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ArchitectureValues
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.BootModeValues
import Amazonka.EC2.Types.DeviceType
import Amazonka.EC2.Types.HypervisorType
import Amazonka.EC2.Types.ImageState
import Amazonka.EC2.Types.ImageTypeValues
import Amazonka.EC2.Types.PlatformValues
import Amazonka.EC2.Types.ProductCode
import Amazonka.EC2.Types.StateReason
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.VirtualizationType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
    platform :: Prelude.Maybe PlatformValues,
    -- | The platform details associated with the billing code of the AMI. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Understanding AMI billing>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    platformDetails :: Prelude.Maybe Prelude.Text,
    -- | The date and time to deprecate the AMI, in UTC, in the following format:
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specified a value for seconds,
    -- Amazon EC2 rounds the seconds to the nearest minute.
    deprecationTime :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether enhanced networking with ENA is enabled.
    enaSupport :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services account alias (for example, @amazon@, @self@) or
    -- the Amazon Web Services account ID of the AMI owner.
    imageOwnerAlias :: Prelude.Maybe Prelude.Text,
    -- | The operation of the Amazon EC2 instance and the billing code that is
    -- associated with the AMI. @usageOperation@ corresponds to the
    -- <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem\/Operation>
    -- column on your Amazon Web Services Cost and Usage Report and in the
    -- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html Amazon Web Services Price List API>.
    -- You can view these fields on the __Instances__ or __AMIs__ pages in the
    -- Amazon EC2 console, or in the responses that are returned by the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeImages.html DescribeImages>
    -- command in the Amazon EC2 API, or the
    -- <https://docs.aws.amazon.com/cli/latest/reference/ec2/describe-images.html describe-images>
    -- command in the CLI.
    usageOperation :: Prelude.Maybe Prelude.Text,
    -- | The RAM disk associated with the image, if any. Only applicable for
    -- machine images.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The kernel associated with the image, if any. Only applicable for
    -- machine images.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Prelude.Maybe Prelude.Text,
    -- | The name of the AMI that was provided during image creation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The boot mode of the image. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    bootMode :: Prelude.Maybe BootModeValues,
    -- | The date and time the image was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | Any product codes associated with the AMI.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The reason for the state change.
    stateReason :: Prelude.Maybe StateReason,
    -- | The description of the AMI that was provided during image creation.
    description :: Prelude.Maybe Prelude.Text,
    -- | Any block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | Any tags assigned to the image.
    tags :: Prelude.Maybe [Tag],
    -- | The ID of the AMI.
    imageId :: Prelude.Text,
    -- | The location of the AMI.
    imageLocation :: Prelude.Text,
    -- | The current state of the AMI. If the state is @available@, the image is
    -- successfully registered and can be used to launch an instance.
    state :: ImageState,
    -- | The ID of the Amazon Web Services account that owns the image.
    ownerId :: Prelude.Text,
    -- | Indicates whether the image has public launch permissions. The value is
    -- @true@ if this image has public launch permissions or @false@ if it has
    -- only implicit and explicit launch permissions.
    public :: Prelude.Bool,
    -- | The architecture of the image.
    architecture :: ArchitectureValues,
    -- | The type of image.
    imageType :: ImageTypeValues,
    -- | The type of root device used by the AMI. The AMI can use an Amazon EBS
    -- volume or an instance store volume.
    rootDeviceType :: DeviceType,
    -- | The type of virtualization of the AMI.
    virtualizationType :: VirtualizationType,
    -- | The hypervisor type of the image.
    hypervisor :: HypervisorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'platformDetails', 'image_platformDetails' - The platform details associated with the billing code of the AMI. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Understanding AMI billing>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'deprecationTime', 'image_deprecationTime' - The date and time to deprecate the AMI, in UTC, in the following format:
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specified a value for seconds,
-- Amazon EC2 rounds the seconds to the nearest minute.
--
-- 'enaSupport', 'image_enaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- 'imageOwnerAlias', 'image_imageOwnerAlias' - The Amazon Web Services account alias (for example, @amazon@, @self@) or
-- the Amazon Web Services account ID of the AMI owner.
--
-- 'usageOperation', 'image_usageOperation' - The operation of the Amazon EC2 instance and the billing code that is
-- associated with the AMI. @usageOperation@ corresponds to the
-- <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem\/Operation>
-- column on your Amazon Web Services Cost and Usage Report and in the
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html Amazon Web Services Price List API>.
-- You can view these fields on the __Instances__ or __AMIs__ pages in the
-- Amazon EC2 console, or in the responses that are returned by the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeImages.html DescribeImages>
-- command in the Amazon EC2 API, or the
-- <https://docs.aws.amazon.com/cli/latest/reference/ec2/describe-images.html describe-images>
-- command in the CLI.
--
-- 'ramdiskId', 'image_ramdiskId' - The RAM disk associated with the image, if any. Only applicable for
-- machine images.
--
-- 'kernelId', 'image_kernelId' - The kernel associated with the image, if any. Only applicable for
-- machine images.
--
-- 'rootDeviceName', 'image_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'sriovNetSupport', 'image_sriovNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'name', 'image_name' - The name of the AMI that was provided during image creation.
--
-- 'bootMode', 'image_bootMode' - The boot mode of the image. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'creationDate', 'image_creationDate' - The date and time the image was created.
--
-- 'productCodes', 'image_productCodes' - Any product codes associated with the AMI.
--
-- 'stateReason', 'image_stateReason' - The reason for the state change.
--
-- 'description', 'image_description' - The description of the AMI that was provided during image creation.
--
-- 'blockDeviceMappings', 'image_blockDeviceMappings' - Any block device mapping entries.
--
-- 'tags', 'image_tags' - Any tags assigned to the image.
--
-- 'imageId', 'image_imageId' - The ID of the AMI.
--
-- 'imageLocation', 'image_imageLocation' - The location of the AMI.
--
-- 'state', 'image_state' - The current state of the AMI. If the state is @available@, the image is
-- successfully registered and can be used to launch an instance.
--
-- 'ownerId', 'image_ownerId' - The ID of the Amazon Web Services account that owns the image.
--
-- 'public', 'image_public' - Indicates whether the image has public launch permissions. The value is
-- @true@ if this image has public launch permissions or @false@ if it has
-- only implicit and explicit launch permissions.
--
-- 'architecture', 'image_architecture' - The architecture of the image.
--
-- 'imageType', 'image_imageType' - The type of image.
--
-- 'rootDeviceType', 'image_rootDeviceType' - The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
--
-- 'virtualizationType', 'image_virtualizationType' - The type of virtualization of the AMI.
--
-- 'hypervisor', 'image_hypervisor' - The hypervisor type of the image.
newImage ::
  -- | 'imageId'
  Prelude.Text ->
  -- | 'imageLocation'
  Prelude.Text ->
  -- | 'state'
  ImageState ->
  -- | 'ownerId'
  Prelude.Text ->
  -- | 'public'
  Prelude.Bool ->
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
      { platform = Prelude.Nothing,
        platformDetails = Prelude.Nothing,
        deprecationTime = Prelude.Nothing,
        enaSupport = Prelude.Nothing,
        imageOwnerAlias = Prelude.Nothing,
        usageOperation = Prelude.Nothing,
        ramdiskId = Prelude.Nothing,
        kernelId = Prelude.Nothing,
        rootDeviceName = Prelude.Nothing,
        sriovNetSupport = Prelude.Nothing,
        name = Prelude.Nothing,
        bootMode = Prelude.Nothing,
        creationDate = Prelude.Nothing,
        productCodes = Prelude.Nothing,
        stateReason = Prelude.Nothing,
        description = Prelude.Nothing,
        blockDeviceMappings = Prelude.Nothing,
        tags = Prelude.Nothing,
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
image_platform :: Lens.Lens' Image (Prelude.Maybe PlatformValues)
image_platform = Lens.lens (\Image' {platform} -> platform) (\s@Image' {} a -> s {platform = a} :: Image)

-- | The platform details associated with the billing code of the AMI. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Understanding AMI billing>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_platformDetails :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_platformDetails = Lens.lens (\Image' {platformDetails} -> platformDetails) (\s@Image' {} a -> s {platformDetails = a} :: Image)

-- | The date and time to deprecate the AMI, in UTC, in the following format:
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specified a value for seconds,
-- Amazon EC2 rounds the seconds to the nearest minute.
image_deprecationTime :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_deprecationTime = Lens.lens (\Image' {deprecationTime} -> deprecationTime) (\s@Image' {} a -> s {deprecationTime = a} :: Image)

-- | Specifies whether enhanced networking with ENA is enabled.
image_enaSupport :: Lens.Lens' Image (Prelude.Maybe Prelude.Bool)
image_enaSupport = Lens.lens (\Image' {enaSupport} -> enaSupport) (\s@Image' {} a -> s {enaSupport = a} :: Image)

-- | The Amazon Web Services account alias (for example, @amazon@, @self@) or
-- the Amazon Web Services account ID of the AMI owner.
image_imageOwnerAlias :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageOwnerAlias = Lens.lens (\Image' {imageOwnerAlias} -> imageOwnerAlias) (\s@Image' {} a -> s {imageOwnerAlias = a} :: Image)

-- | The operation of the Amazon EC2 instance and the billing code that is
-- associated with the AMI. @usageOperation@ corresponds to the
-- <https://docs.aws.amazon.com/cur/latest/userguide/Lineitem-columns.html#Lineitem-details-O-Operation lineitem\/Operation>
-- column on your Amazon Web Services Cost and Usage Report and in the
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/price-changes.html Amazon Web Services Price List API>.
-- You can view these fields on the __Instances__ or __AMIs__ pages in the
-- Amazon EC2 console, or in the responses that are returned by the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DescribeImages.html DescribeImages>
-- command in the Amazon EC2 API, or the
-- <https://docs.aws.amazon.com/cli/latest/reference/ec2/describe-images.html describe-images>
-- command in the CLI.
image_usageOperation :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_usageOperation = Lens.lens (\Image' {usageOperation} -> usageOperation) (\s@Image' {} a -> s {usageOperation = a} :: Image)

-- | The RAM disk associated with the image, if any. Only applicable for
-- machine images.
image_ramdiskId :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_ramdiskId = Lens.lens (\Image' {ramdiskId} -> ramdiskId) (\s@Image' {} a -> s {ramdiskId = a} :: Image)

-- | The kernel associated with the image, if any. Only applicable for
-- machine images.
image_kernelId :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_kernelId = Lens.lens (\Image' {kernelId} -> kernelId) (\s@Image' {} a -> s {kernelId = a} :: Image)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
image_rootDeviceName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_rootDeviceName = Lens.lens (\Image' {rootDeviceName} -> rootDeviceName) (\s@Image' {} a -> s {rootDeviceName = a} :: Image)

-- | Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
image_sriovNetSupport :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_sriovNetSupport = Lens.lens (\Image' {sriovNetSupport} -> sriovNetSupport) (\s@Image' {} a -> s {sriovNetSupport = a} :: Image)

-- | The name of the AMI that was provided during image creation.
image_name :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

-- | The boot mode of the image. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_bootMode :: Lens.Lens' Image (Prelude.Maybe BootModeValues)
image_bootMode = Lens.lens (\Image' {bootMode} -> bootMode) (\s@Image' {} a -> s {bootMode = a} :: Image)

-- | The date and time the image was created.
image_creationDate :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_creationDate = Lens.lens (\Image' {creationDate} -> creationDate) (\s@Image' {} a -> s {creationDate = a} :: Image)

-- | Any product codes associated with the AMI.
image_productCodes :: Lens.Lens' Image (Prelude.Maybe [ProductCode])
image_productCodes = Lens.lens (\Image' {productCodes} -> productCodes) (\s@Image' {} a -> s {productCodes = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | The reason for the state change.
image_stateReason :: Lens.Lens' Image (Prelude.Maybe StateReason)
image_stateReason = Lens.lens (\Image' {stateReason} -> stateReason) (\s@Image' {} a -> s {stateReason = a} :: Image)

-- | The description of the AMI that was provided during image creation.
image_description :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | Any block device mapping entries.
image_blockDeviceMappings :: Lens.Lens' Image (Prelude.Maybe [BlockDeviceMapping])
image_blockDeviceMappings = Lens.lens (\Image' {blockDeviceMappings} -> blockDeviceMappings) (\s@Image' {} a -> s {blockDeviceMappings = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | Any tags assigned to the image.
image_tags :: Lens.Lens' Image (Prelude.Maybe [Tag])
image_tags = Lens.lens (\Image' {tags} -> tags) (\s@Image' {} a -> s {tags = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the AMI.
image_imageId :: Lens.Lens' Image Prelude.Text
image_imageId = Lens.lens (\Image' {imageId} -> imageId) (\s@Image' {} a -> s {imageId = a} :: Image)

-- | The location of the AMI.
image_imageLocation :: Lens.Lens' Image Prelude.Text
image_imageLocation = Lens.lens (\Image' {imageLocation} -> imageLocation) (\s@Image' {} a -> s {imageLocation = a} :: Image)

-- | The current state of the AMI. If the state is @available@, the image is
-- successfully registered and can be used to launch an instance.
image_state :: Lens.Lens' Image ImageState
image_state = Lens.lens (\Image' {state} -> state) (\s@Image' {} a -> s {state = a} :: Image)

-- | The ID of the Amazon Web Services account that owns the image.
image_ownerId :: Lens.Lens' Image Prelude.Text
image_ownerId = Lens.lens (\Image' {ownerId} -> ownerId) (\s@Image' {} a -> s {ownerId = a} :: Image)

-- | Indicates whether the image has public launch permissions. The value is
-- @true@ if this image has public launch permissions or @false@ if it has
-- only implicit and explicit launch permissions.
image_public :: Lens.Lens' Image Prelude.Bool
image_public = Lens.lens (\Image' {public} -> public) (\s@Image' {} a -> s {public = a} :: Image)

-- | The architecture of the image.
image_architecture :: Lens.Lens' Image ArchitectureValues
image_architecture = Lens.lens (\Image' {architecture} -> architecture) (\s@Image' {} a -> s {architecture = a} :: Image)

-- | The type of image.
image_imageType :: Lens.Lens' Image ImageTypeValues
image_imageType = Lens.lens (\Image' {imageType} -> imageType) (\s@Image' {} a -> s {imageType = a} :: Image)

-- | The type of root device used by the AMI. The AMI can use an Amazon EBS
-- volume or an instance store volume.
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
      Prelude.<$> (x Core..@? "platform")
      Prelude.<*> (x Core..@? "platformDetails")
      Prelude.<*> (x Core..@? "deprecationTime")
      Prelude.<*> (x Core..@? "enaSupport")
      Prelude.<*> (x Core..@? "imageOwnerAlias")
      Prelude.<*> (x Core..@? "usageOperation")
      Prelude.<*> (x Core..@? "ramdiskId")
      Prelude.<*> (x Core..@? "kernelId")
      Prelude.<*> (x Core..@? "rootDeviceName")
      Prelude.<*> (x Core..@? "sriovNetSupport")
      Prelude.<*> (x Core..@? "name")
      Prelude.<*> (x Core..@? "bootMode")
      Prelude.<*> (x Core..@? "creationDate")
      Prelude.<*> ( x Core..@? "productCodes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "stateReason")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> ( x Core..@? "blockDeviceMapping"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@ "imageId")
      Prelude.<*> (x Core..@ "imageLocation")
      Prelude.<*> (x Core..@ "imageState")
      Prelude.<*> (x Core..@ "imageOwnerId")
      Prelude.<*> (x Core..@ "isPublic")
      Prelude.<*> (x Core..@ "architecture")
      Prelude.<*> (x Core..@ "imageType")
      Prelude.<*> (x Core..@ "rootDeviceType")
      Prelude.<*> (x Core..@ "virtualizationType")
      Prelude.<*> (x Core..@ "hypervisor")

instance Prelude.Hashable Image

instance Prelude.NFData Image
