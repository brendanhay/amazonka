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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Image where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ArchitectureValues
import Amazonka.EC2.Types.BlockDeviceMapping
import Amazonka.EC2.Types.BootModeValues
import Amazonka.EC2.Types.DeviceType
import Amazonka.EC2.Types.HypervisorType
import Amazonka.EC2.Types.ImageState
import Amazonka.EC2.Types.ImageTypeValues
import Amazonka.EC2.Types.ImdsSupportValues
import Amazonka.EC2.Types.PlatformValues
import Amazonka.EC2.Types.ProductCode
import Amazonka.EC2.Types.StateReason
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TpmSupportValues
import Amazonka.EC2.Types.VirtualizationType
import qualified Amazonka.Prelude as Prelude

-- | Describes an image.
--
-- /See:/ 'newImage' smart constructor.
data Image = Image'
  { -- | Any block device mapping entries.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The boot mode of the image. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    bootMode :: Prelude.Maybe BootModeValues,
    -- | The date and time the image was created.
    creationDate :: Prelude.Maybe Prelude.Text,
    -- | The date and time to deprecate the AMI, in UTC, in the following format:
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specified a value for seconds,
    -- Amazon EC2 rounds the seconds to the nearest minute.
    deprecationTime :: Prelude.Maybe Prelude.Text,
    -- | The description of the AMI that was provided during image creation.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether enhanced networking with ENA is enabled.
    enaSupport :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services account alias (for example, @amazon@, @self@) or
    -- the Amazon Web Services account ID of the AMI owner.
    imageOwnerAlias :: Prelude.Maybe Prelude.Text,
    -- | If @v2.0@, it indicates that IMDSv2 is specified in the AMI. Instances
    -- launched from this AMI will have @HttpTokens@ automatically set to
    -- @required@ so that, by default, the instance requires that IMDSv2 is
    -- used when requesting instance metadata. In addition,
    -- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    imdsSupport :: Prelude.Maybe ImdsSupportValues,
    -- | The kernel associated with the image, if any. Only applicable for
    -- machine images.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The name of the AMI that was provided during image creation.
    name :: Prelude.Maybe Prelude.Text,
    -- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
    platform :: Prelude.Maybe PlatformValues,
    -- | The platform details associated with the billing code of the AMI. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Understand AMI billing information>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    platformDetails :: Prelude.Maybe Prelude.Text,
    -- | Any product codes associated with the AMI.
    productCodes :: Prelude.Maybe [ProductCode],
    -- | The RAM disk associated with the image, if any. Only applicable for
    -- machine images.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether enhanced networking with the Intel 82599 Virtual
    -- Function interface is enabled.
    sriovNetSupport :: Prelude.Maybe Prelude.Text,
    -- | The reason for the state change.
    stateReason :: Prelude.Maybe StateReason,
    -- | Any tags assigned to the image.
    tags :: Prelude.Maybe [Tag],
    -- | If the image is configured for NitroTPM support, the value is @v2.0@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    tpmSupport :: Prelude.Maybe TpmSupportValues,
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
-- 'blockDeviceMappings', 'image_blockDeviceMappings' - Any block device mapping entries.
--
-- 'bootMode', 'image_bootMode' - The boot mode of the image. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'creationDate', 'image_creationDate' - The date and time the image was created.
--
-- 'deprecationTime', 'image_deprecationTime' - The date and time to deprecate the AMI, in UTC, in the following format:
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specified a value for seconds,
-- Amazon EC2 rounds the seconds to the nearest minute.
--
-- 'description', 'image_description' - The description of the AMI that was provided during image creation.
--
-- 'enaSupport', 'image_enaSupport' - Specifies whether enhanced networking with ENA is enabled.
--
-- 'imageOwnerAlias', 'image_imageOwnerAlias' - The Amazon Web Services account alias (for example, @amazon@, @self@) or
-- the Amazon Web Services account ID of the AMI owner.
--
-- 'imdsSupport', 'image_imdsSupport' - If @v2.0@, it indicates that IMDSv2 is specified in the AMI. Instances
-- launched from this AMI will have @HttpTokens@ automatically set to
-- @required@ so that, by default, the instance requires that IMDSv2 is
-- used when requesting instance metadata. In addition,
-- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'kernelId', 'image_kernelId' - The kernel associated with the image, if any. Only applicable for
-- machine images.
--
-- 'name', 'image_name' - The name of the AMI that was provided during image creation.
--
-- 'platform', 'image_platform' - This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
--
-- 'platformDetails', 'image_platformDetails' - The platform details associated with the billing code of the AMI. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Understand AMI billing information>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'productCodes', 'image_productCodes' - Any product codes associated with the AMI.
--
-- 'ramdiskId', 'image_ramdiskId' - The RAM disk associated with the image, if any. Only applicable for
-- machine images.
--
-- 'rootDeviceName', 'image_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'sriovNetSupport', 'image_sriovNetSupport' - Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
--
-- 'stateReason', 'image_stateReason' - The reason for the state change.
--
-- 'tags', 'image_tags' - Any tags assigned to the image.
--
-- 'tpmSupport', 'image_tpmSupport' - If the image is configured for NitroTPM support, the value is @v2.0@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
-- in the /Amazon Elastic Compute Cloud User Guide/.
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
      { blockDeviceMappings = Prelude.Nothing,
        bootMode = Prelude.Nothing,
        creationDate = Prelude.Nothing,
        deprecationTime = Prelude.Nothing,
        description = Prelude.Nothing,
        enaSupport = Prelude.Nothing,
        imageOwnerAlias = Prelude.Nothing,
        imdsSupport = Prelude.Nothing,
        kernelId = Prelude.Nothing,
        name = Prelude.Nothing,
        platform = Prelude.Nothing,
        platformDetails = Prelude.Nothing,
        productCodes = Prelude.Nothing,
        ramdiskId = Prelude.Nothing,
        rootDeviceName = Prelude.Nothing,
        sriovNetSupport = Prelude.Nothing,
        stateReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        tpmSupport = Prelude.Nothing,
        usageOperation = Prelude.Nothing,
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

-- | Any block device mapping entries.
image_blockDeviceMappings :: Lens.Lens' Image (Prelude.Maybe [BlockDeviceMapping])
image_blockDeviceMappings = Lens.lens (\Image' {blockDeviceMappings} -> blockDeviceMappings) (\s@Image' {} a -> s {blockDeviceMappings = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | The boot mode of the image. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_bootMode :: Lens.Lens' Image (Prelude.Maybe BootModeValues)
image_bootMode = Lens.lens (\Image' {bootMode} -> bootMode) (\s@Image' {} a -> s {bootMode = a} :: Image)

-- | The date and time the image was created.
image_creationDate :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_creationDate = Lens.lens (\Image' {creationDate} -> creationDate) (\s@Image' {} a -> s {creationDate = a} :: Image)

-- | The date and time to deprecate the AMI, in UTC, in the following format:
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z. If you specified a value for seconds,
-- Amazon EC2 rounds the seconds to the nearest minute.
image_deprecationTime :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_deprecationTime = Lens.lens (\Image' {deprecationTime} -> deprecationTime) (\s@Image' {} a -> s {deprecationTime = a} :: Image)

-- | The description of the AMI that was provided during image creation.
image_description :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_description = Lens.lens (\Image' {description} -> description) (\s@Image' {} a -> s {description = a} :: Image)

-- | Specifies whether enhanced networking with ENA is enabled.
image_enaSupport :: Lens.Lens' Image (Prelude.Maybe Prelude.Bool)
image_enaSupport = Lens.lens (\Image' {enaSupport} -> enaSupport) (\s@Image' {} a -> s {enaSupport = a} :: Image)

-- | The Amazon Web Services account alias (for example, @amazon@, @self@) or
-- the Amazon Web Services account ID of the AMI owner.
image_imageOwnerAlias :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_imageOwnerAlias = Lens.lens (\Image' {imageOwnerAlias} -> imageOwnerAlias) (\s@Image' {} a -> s {imageOwnerAlias = a} :: Image)

-- | If @v2.0@, it indicates that IMDSv2 is specified in the AMI. Instances
-- launched from this AMI will have @HttpTokens@ automatically set to
-- @required@ so that, by default, the instance requires that IMDSv2 is
-- used when requesting instance metadata. In addition,
-- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_imdsSupport :: Lens.Lens' Image (Prelude.Maybe ImdsSupportValues)
image_imdsSupport = Lens.lens (\Image' {imdsSupport} -> imdsSupport) (\s@Image' {} a -> s {imdsSupport = a} :: Image)

-- | The kernel associated with the image, if any. Only applicable for
-- machine images.
image_kernelId :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_kernelId = Lens.lens (\Image' {kernelId} -> kernelId) (\s@Image' {} a -> s {kernelId = a} :: Image)

-- | The name of the AMI that was provided during image creation.
image_name :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_name = Lens.lens (\Image' {name} -> name) (\s@Image' {} a -> s {name = a} :: Image)

-- | This value is set to @windows@ for Windows AMIs; otherwise, it is blank.
image_platform :: Lens.Lens' Image (Prelude.Maybe PlatformValues)
image_platform = Lens.lens (\Image' {platform} -> platform) (\s@Image' {} a -> s {platform = a} :: Image)

-- | The platform details associated with the billing code of the AMI. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Understand AMI billing information>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_platformDetails :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_platformDetails = Lens.lens (\Image' {platformDetails} -> platformDetails) (\s@Image' {} a -> s {platformDetails = a} :: Image)

-- | Any product codes associated with the AMI.
image_productCodes :: Lens.Lens' Image (Prelude.Maybe [ProductCode])
image_productCodes = Lens.lens (\Image' {productCodes} -> productCodes) (\s@Image' {} a -> s {productCodes = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | The RAM disk associated with the image, if any. Only applicable for
-- machine images.
image_ramdiskId :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_ramdiskId = Lens.lens (\Image' {ramdiskId} -> ramdiskId) (\s@Image' {} a -> s {ramdiskId = a} :: Image)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
image_rootDeviceName :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_rootDeviceName = Lens.lens (\Image' {rootDeviceName} -> rootDeviceName) (\s@Image' {} a -> s {rootDeviceName = a} :: Image)

-- | Specifies whether enhanced networking with the Intel 82599 Virtual
-- Function interface is enabled.
image_sriovNetSupport :: Lens.Lens' Image (Prelude.Maybe Prelude.Text)
image_sriovNetSupport = Lens.lens (\Image' {sriovNetSupport} -> sriovNetSupport) (\s@Image' {} a -> s {sriovNetSupport = a} :: Image)

-- | The reason for the state change.
image_stateReason :: Lens.Lens' Image (Prelude.Maybe StateReason)
image_stateReason = Lens.lens (\Image' {stateReason} -> stateReason) (\s@Image' {} a -> s {stateReason = a} :: Image)

-- | Any tags assigned to the image.
image_tags :: Lens.Lens' Image (Prelude.Maybe [Tag])
image_tags = Lens.lens (\Image' {tags} -> tags) (\s@Image' {} a -> s {tags = a} :: Image) Prelude.. Lens.mapping Lens.coerced

-- | If the image is configured for NitroTPM support, the value is @v2.0@.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
-- in the /Amazon Elastic Compute Cloud User Guide/.
image_tpmSupport :: Lens.Lens' Image (Prelude.Maybe TpmSupportValues)
image_tpmSupport = Lens.lens (\Image' {tpmSupport} -> tpmSupport) (\s@Image' {} a -> s {tpmSupport = a} :: Image)

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

instance Data.FromXML Image where
  parseXML x =
    Image'
      Prelude.<$> ( x Data..@? "blockDeviceMapping"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "bootMode")
      Prelude.<*> (x Data..@? "creationDate")
      Prelude.<*> (x Data..@? "deprecationTime")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "enaSupport")
      Prelude.<*> (x Data..@? "imageOwnerAlias")
      Prelude.<*> (x Data..@? "imdsSupport")
      Prelude.<*> (x Data..@? "kernelId")
      Prelude.<*> (x Data..@? "name")
      Prelude.<*> (x Data..@? "platform")
      Prelude.<*> (x Data..@? "platformDetails")
      Prelude.<*> ( x Data..@? "productCodes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ramdiskId")
      Prelude.<*> (x Data..@? "rootDeviceName")
      Prelude.<*> (x Data..@? "sriovNetSupport")
      Prelude.<*> (x Data..@? "stateReason")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "tpmSupport")
      Prelude.<*> (x Data..@? "usageOperation")
      Prelude.<*> (x Data..@ "imageId")
      Prelude.<*> (x Data..@ "imageLocation")
      Prelude.<*> (x Data..@ "imageState")
      Prelude.<*> (x Data..@ "imageOwnerId")
      Prelude.<*> (x Data..@ "isPublic")
      Prelude.<*> (x Data..@ "architecture")
      Prelude.<*> (x Data..@ "imageType")
      Prelude.<*> (x Data..@ "rootDeviceType")
      Prelude.<*> (x Data..@ "virtualizationType")
      Prelude.<*> (x Data..@ "hypervisor")

instance Prelude.Hashable Image where
  hashWithSalt _salt Image' {..} =
    _salt `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` deprecationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enaSupport
      `Prelude.hashWithSalt` imageOwnerAlias
      `Prelude.hashWithSalt` imdsSupport
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` platformDetails
      `Prelude.hashWithSalt` productCodes
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` rootDeviceName
      `Prelude.hashWithSalt` sriovNetSupport
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` tpmSupport
      `Prelude.hashWithSalt` usageOperation
      `Prelude.hashWithSalt` imageId
      `Prelude.hashWithSalt` imageLocation
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` public
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` imageType
      `Prelude.hashWithSalt` rootDeviceType
      `Prelude.hashWithSalt` virtualizationType
      `Prelude.hashWithSalt` hypervisor

instance Prelude.NFData Image where
  rnf Image' {..} =
    Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf deprecationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf enaSupport
      `Prelude.seq` Prelude.rnf imageOwnerAlias
      `Prelude.seq` Prelude.rnf imdsSupport
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf platformDetails
      `Prelude.seq` Prelude.rnf productCodes
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf rootDeviceName
      `Prelude.seq` Prelude.rnf sriovNetSupport
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf tpmSupport
      `Prelude.seq` Prelude.rnf usageOperation
      `Prelude.seq` Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf imageLocation
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf public
      `Prelude.seq` Prelude.rnf
        architecture
      `Prelude.seq` Prelude.rnf
        imageType
      `Prelude.seq` Prelude.rnf
        rootDeviceType
      `Prelude.seq` Prelude.rnf
        virtualizationType
      `Prelude.seq` Prelude.rnf
        hypervisor
