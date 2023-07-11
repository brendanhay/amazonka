{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.RegisterImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AMI. When you\'re creating an AMI, this is the final step
-- you must complete before you can launch an instance from the AMI. For
-- more information about creating AMIs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami.html Create your own AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For Amazon EBS-backed instances, CreateImage creates and registers the
-- AMI in a single request, so you don\'t have to register the AMI
-- yourself. We recommend that you always use CreateImage unless you have a
-- specific reason to use RegisterImage.
--
-- If needed, you can deregister an AMI at any time. Any modifications you
-- make to an AMI backed by an instance store volume invalidates its
-- registration. If you make changes to an image, deregister the previous
-- image and register the new image.
--
-- __Register a snapshot of a root device volume__
--
-- You can use @RegisterImage@ to create an Amazon EBS-backed Linux AMI
-- from a snapshot of a root device volume. You specify the snapshot using
-- a block device mapping. You can\'t set the encryption state of the
-- volume using the block device mapping. If the snapshot is encrypted, or
-- encryption by default is enabled, the root volume of an instance
-- launched from the AMI is encrypted.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html#creating-launching-ami-from-snapshot Create a Linux AMI from a snapshot>
-- and
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Use encryption with Amazon EBS-backed AMIs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- __Amazon Web Services Marketplace product codes__
--
-- If any snapshots have Amazon Web Services Marketplace product codes,
-- they are copied to the new AMI.
--
-- Windows and some Linux distributions, such as Red Hat Enterprise Linux
-- (RHEL) and SUSE Linux Enterprise Server (SLES), use the Amazon EC2
-- billing product code associated with an AMI to verify the subscription
-- status for package updates. To create a new AMI for operating systems
-- that require a billing product code, instead of registering the AMI, do
-- the following to preserve the billing product code association:
--
-- 1.  Launch an instance from an existing AMI with that billing product
--     code.
--
-- 2.  Customize the instance.
--
-- 3.  Create an AMI from the instance using CreateImage.
--
-- If you purchase a Reserved Instance to apply to an On-Demand Instance
-- that was launched from an AMI with a billing product code, make sure
-- that the Reserved Instance has the matching billing product code. If you
-- purchase a Reserved Instance without the matching billing product code,
-- the Reserved Instance will not be applied to the On-Demand Instance. For
-- information about how to obtain the platform details and billing
-- information of an AMI, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Understand AMI billing information>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.RegisterImage
  ( -- * Creating a Request
    RegisterImage (..),
    newRegisterImage,

    -- * Request Lenses
    registerImage_architecture,
    registerImage_billingProducts,
    registerImage_blockDeviceMappings,
    registerImage_bootMode,
    registerImage_description,
    registerImage_dryRun,
    registerImage_enaSupport,
    registerImage_imageLocation,
    registerImage_imdsSupport,
    registerImage_kernelId,
    registerImage_ramdiskId,
    registerImage_rootDeviceName,
    registerImage_sriovNetSupport,
    registerImage_tpmSupport,
    registerImage_uefiData,
    registerImage_virtualizationType,
    registerImage_name,

    -- * Destructuring the Response
    RegisterImageResponse (..),
    newRegisterImageResponse,

    -- * Response Lenses
    registerImageResponse_imageId,
    registerImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for RegisterImage.
--
-- /See:/ 'newRegisterImage' smart constructor.
data RegisterImage = RegisterImage'
  { -- | The architecture of the AMI.
    --
    -- Default: For Amazon EBS-backed AMIs, @i386@. For instance store-backed
    -- AMIs, the architecture specified in the manifest file.
    architecture :: Prelude.Maybe ArchitectureValues,
    -- | The billing product codes. Your account must be authorized to specify
    -- billing product codes.
    --
    -- If your account is not authorized to specify billing product codes, you
    -- can publish AMIs that include billable software and list them on the
    -- Amazon Web Services Marketplace. You must first register as a seller on
    -- the Amazon Web Services Marketplace. For more information, see
    -- <https://docs.aws.amazon.com/marketplace/latest/userguide/user-guide-for-sellers.html Getting started as a seller>
    -- and
    -- <https://docs.aws.amazon.com/marketplace/latest/userguide/ami-products.html AMI-based products>
    -- in the /Amazon Web Services Marketplace Seller Guide/.
    billingProducts :: Prelude.Maybe [Prelude.Text],
    -- | The block device mapping entries.
    --
    -- If you specify an Amazon EBS volume using the ID of an Amazon EBS
    -- snapshot, you can\'t specify the encryption state of the volume.
    --
    -- If you create an AMI on an Outpost, then all backing snapshots must be
    -- on the same Outpost or in the Region of that Outpost. AMIs on an Outpost
    -- that include local snapshots can be used to launch instances on the same
    -- Outpost only. For more information,
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
    -- in the /Amazon EC2 User Guide/.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The boot mode of the AMI. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
    -- in the /Amazon EC2 User Guide/.
    bootMode :: Prelude.Maybe BootModeValues,
    -- | A description for your AMI.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Set to @true@ to enable enhanced networking with ENA for the AMI and any
    -- instances that you launch from the AMI.
    --
    -- This option is supported only for HVM AMIs. Specifying this option with
    -- a PV AMI can make instances launched from the AMI unreachable.
    enaSupport :: Prelude.Maybe Prelude.Bool,
    -- | The full path to your AMI manifest in Amazon S3 storage. The specified
    -- bucket must have the @aws-exec-read@ canned access control list (ACL) to
    -- ensure that it can be accessed by Amazon EC2. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs>
    -- in the /Amazon S3 Service Developer Guide/.
    imageLocation :: Prelude.Maybe Prelude.Text,
    -- | Set to @v2.0@ to indicate that IMDSv2 is specified in the AMI. Instances
    -- launched from this AMI will have @HttpTokens@ automatically set to
    -- @required@ so that, by default, the instance requires that IMDSv2 is
    -- used when requesting instance metadata. In addition,
    -- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
    -- in the /Amazon EC2 User Guide/.
    --
    -- If you set the value to @v2.0@, make sure that your AMI software can
    -- support IMDSv2.
    imdsSupport :: Prelude.Maybe ImdsSupportValues,
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RAM disk.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe Prelude.Text,
    -- | Set to @simple@ to enable enhanced networking with the Intel 82599
    -- Virtual Function interface for the AMI and any instances that you launch
    -- from the AMI.
    --
    -- There is no way to disable @sriovNetSupport@ at this time.
    --
    -- This option is supported only for HVM AMIs. Specifying this option with
    -- a PV AMI can make instances launched from the AMI unreachable.
    sriovNetSupport :: Prelude.Maybe Prelude.Text,
    -- | Set to @v2.0@ to enable Trusted Platform Module (TPM) support. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
    -- in the /Amazon EC2 User Guide/.
    tpmSupport :: Prelude.Maybe TpmSupportValues,
    -- | Base64 representation of the non-volatile UEFI variable store. To
    -- retrieve the UEFI data, use the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceUefiData GetInstanceUefiData>
    -- command. You can inspect and modify the UEFI data by using the
    -- <https://github.com/awslabs/python-uefivars python-uefivars tool> on
    -- GitHub. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/uefi-secure-boot.html UEFI Secure Boot>
    -- in the /Amazon EC2 User Guide/.
    uefiData :: Prelude.Maybe Prelude.Text,
    -- | The type of virtualization (@hvm@ | @paravirtual@).
    --
    -- Default: @paravirtual@
    virtualizationType :: Prelude.Maybe Prelude.Text,
    -- | A name for your AMI.
    --
    -- Constraints: 3-128 alphanumeric characters, parentheses (()), square
    -- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
    -- quotes (\'), at-signs (\@), or underscores(_)
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'registerImage_architecture' - The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@. For instance store-backed
-- AMIs, the architecture specified in the manifest file.
--
-- 'billingProducts', 'registerImage_billingProducts' - The billing product codes. Your account must be authorized to specify
-- billing product codes.
--
-- If your account is not authorized to specify billing product codes, you
-- can publish AMIs that include billable software and list them on the
-- Amazon Web Services Marketplace. You must first register as a seller on
-- the Amazon Web Services Marketplace. For more information, see
-- <https://docs.aws.amazon.com/marketplace/latest/userguide/user-guide-for-sellers.html Getting started as a seller>
-- and
-- <https://docs.aws.amazon.com/marketplace/latest/userguide/ami-products.html AMI-based products>
-- in the /Amazon Web Services Marketplace Seller Guide/.
--
-- 'blockDeviceMappings', 'registerImage_blockDeviceMappings' - The block device mapping entries.
--
-- If you specify an Amazon EBS volume using the ID of an Amazon EBS
-- snapshot, you can\'t specify the encryption state of the volume.
--
-- If you create an AMI on an Outpost, then all backing snapshots must be
-- on the same Outpost or in the Region of that Outpost. AMIs on an Outpost
-- that include local snapshots can be used to launch instances on the same
-- Outpost only. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
-- in the /Amazon EC2 User Guide/.
--
-- 'bootMode', 'registerImage_bootMode' - The boot mode of the AMI. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
--
-- 'description', 'registerImage_description' - A description for your AMI.
--
-- 'dryRun', 'registerImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'enaSupport', 'registerImage_enaSupport' - Set to @true@ to enable enhanced networking with ENA for the AMI and any
-- instances that you launch from the AMI.
--
-- This option is supported only for HVM AMIs. Specifying this option with
-- a PV AMI can make instances launched from the AMI unreachable.
--
-- 'imageLocation', 'registerImage_imageLocation' - The full path to your AMI manifest in Amazon S3 storage. The specified
-- bucket must have the @aws-exec-read@ canned access control list (ACL) to
-- ensure that it can be accessed by Amazon EC2. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs>
-- in the /Amazon S3 Service Developer Guide/.
--
-- 'imdsSupport', 'registerImage_imdsSupport' - Set to @v2.0@ to indicate that IMDSv2 is specified in the AMI. Instances
-- launched from this AMI will have @HttpTokens@ automatically set to
-- @required@ so that, by default, the instance requires that IMDSv2 is
-- used when requesting instance metadata. In addition,
-- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
-- in the /Amazon EC2 User Guide/.
--
-- If you set the value to @v2.0@, make sure that your AMI software can
-- support IMDSv2.
--
-- 'kernelId', 'registerImage_kernelId' - The ID of the kernel.
--
-- 'ramdiskId', 'registerImage_ramdiskId' - The ID of the RAM disk.
--
-- 'rootDeviceName', 'registerImage_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'sriovNetSupport', 'registerImage_sriovNetSupport' - Set to @simple@ to enable enhanced networking with the Intel 82599
-- Virtual Function interface for the AMI and any instances that you launch
-- from the AMI.
--
-- There is no way to disable @sriovNetSupport@ at this time.
--
-- This option is supported only for HVM AMIs. Specifying this option with
-- a PV AMI can make instances launched from the AMI unreachable.
--
-- 'tpmSupport', 'registerImage_tpmSupport' - Set to @v2.0@ to enable Trusted Platform Module (TPM) support. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
-- in the /Amazon EC2 User Guide/.
--
-- 'uefiData', 'registerImage_uefiData' - Base64 representation of the non-volatile UEFI variable store. To
-- retrieve the UEFI data, use the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceUefiData GetInstanceUefiData>
-- command. You can inspect and modify the UEFI data by using the
-- <https://github.com/awslabs/python-uefivars python-uefivars tool> on
-- GitHub. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/uefi-secure-boot.html UEFI Secure Boot>
-- in the /Amazon EC2 User Guide/.
--
-- 'virtualizationType', 'registerImage_virtualizationType' - The type of virtualization (@hvm@ | @paravirtual@).
--
-- Default: @paravirtual@
--
-- 'name', 'registerImage_name' - A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
newRegisterImage ::
  -- | 'name'
  Prelude.Text ->
  RegisterImage
newRegisterImage pName_ =
  RegisterImage'
    { architecture = Prelude.Nothing,
      billingProducts = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      bootMode = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      enaSupport = Prelude.Nothing,
      imageLocation = Prelude.Nothing,
      imdsSupport = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      rootDeviceName = Prelude.Nothing,
      sriovNetSupport = Prelude.Nothing,
      tpmSupport = Prelude.Nothing,
      uefiData = Prelude.Nothing,
      virtualizationType = Prelude.Nothing,
      name = pName_
    }

-- | The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@. For instance store-backed
-- AMIs, the architecture specified in the manifest file.
registerImage_architecture :: Lens.Lens' RegisterImage (Prelude.Maybe ArchitectureValues)
registerImage_architecture = Lens.lens (\RegisterImage' {architecture} -> architecture) (\s@RegisterImage' {} a -> s {architecture = a} :: RegisterImage)

-- | The billing product codes. Your account must be authorized to specify
-- billing product codes.
--
-- If your account is not authorized to specify billing product codes, you
-- can publish AMIs that include billable software and list them on the
-- Amazon Web Services Marketplace. You must first register as a seller on
-- the Amazon Web Services Marketplace. For more information, see
-- <https://docs.aws.amazon.com/marketplace/latest/userguide/user-guide-for-sellers.html Getting started as a seller>
-- and
-- <https://docs.aws.amazon.com/marketplace/latest/userguide/ami-products.html AMI-based products>
-- in the /Amazon Web Services Marketplace Seller Guide/.
registerImage_billingProducts :: Lens.Lens' RegisterImage (Prelude.Maybe [Prelude.Text])
registerImage_billingProducts = Lens.lens (\RegisterImage' {billingProducts} -> billingProducts) (\s@RegisterImage' {} a -> s {billingProducts = a} :: RegisterImage) Prelude.. Lens.mapping Lens.coerced

-- | The block device mapping entries.
--
-- If you specify an Amazon EBS volume using the ID of an Amazon EBS
-- snapshot, you can\'t specify the encryption state of the volume.
--
-- If you create an AMI on an Outpost, then all backing snapshots must be
-- on the same Outpost or in the Region of that Outpost. AMIs on an Outpost
-- that include local snapshots can be used to launch instances on the same
-- Outpost only. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
-- in the /Amazon EC2 User Guide/.
registerImage_blockDeviceMappings :: Lens.Lens' RegisterImage (Prelude.Maybe [BlockDeviceMapping])
registerImage_blockDeviceMappings = Lens.lens (\RegisterImage' {blockDeviceMappings} -> blockDeviceMappings) (\s@RegisterImage' {} a -> s {blockDeviceMappings = a} :: RegisterImage) Prelude.. Lens.mapping Lens.coerced

-- | The boot mode of the AMI. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-boot.html Boot modes>
-- in the /Amazon EC2 User Guide/.
registerImage_bootMode :: Lens.Lens' RegisterImage (Prelude.Maybe BootModeValues)
registerImage_bootMode = Lens.lens (\RegisterImage' {bootMode} -> bootMode) (\s@RegisterImage' {} a -> s {bootMode = a} :: RegisterImage)

-- | A description for your AMI.
registerImage_description :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_description = Lens.lens (\RegisterImage' {description} -> description) (\s@RegisterImage' {} a -> s {description = a} :: RegisterImage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
registerImage_dryRun :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Bool)
registerImage_dryRun = Lens.lens (\RegisterImage' {dryRun} -> dryRun) (\s@RegisterImage' {} a -> s {dryRun = a} :: RegisterImage)

-- | Set to @true@ to enable enhanced networking with ENA for the AMI and any
-- instances that you launch from the AMI.
--
-- This option is supported only for HVM AMIs. Specifying this option with
-- a PV AMI can make instances launched from the AMI unreachable.
registerImage_enaSupport :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Bool)
registerImage_enaSupport = Lens.lens (\RegisterImage' {enaSupport} -> enaSupport) (\s@RegisterImage' {} a -> s {enaSupport = a} :: RegisterImage)

-- | The full path to your AMI manifest in Amazon S3 storage. The specified
-- bucket must have the @aws-exec-read@ canned access control list (ACL) to
-- ensure that it can be accessed by Amazon EC2. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs>
-- in the /Amazon S3 Service Developer Guide/.
registerImage_imageLocation :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_imageLocation = Lens.lens (\RegisterImage' {imageLocation} -> imageLocation) (\s@RegisterImage' {} a -> s {imageLocation = a} :: RegisterImage)

-- | Set to @v2.0@ to indicate that IMDSv2 is specified in the AMI. Instances
-- launched from this AMI will have @HttpTokens@ automatically set to
-- @required@ so that, by default, the instance requires that IMDSv2 is
-- used when requesting instance metadata. In addition,
-- @HttpPutResponseHopLimit@ is set to @2@. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-IMDS-new-instances.html#configure-IMDS-new-instances-ami-configuration Configure the AMI>
-- in the /Amazon EC2 User Guide/.
--
-- If you set the value to @v2.0@, make sure that your AMI software can
-- support IMDSv2.
registerImage_imdsSupport :: Lens.Lens' RegisterImage (Prelude.Maybe ImdsSupportValues)
registerImage_imdsSupport = Lens.lens (\RegisterImage' {imdsSupport} -> imdsSupport) (\s@RegisterImage' {} a -> s {imdsSupport = a} :: RegisterImage)

-- | The ID of the kernel.
registerImage_kernelId :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_kernelId = Lens.lens (\RegisterImage' {kernelId} -> kernelId) (\s@RegisterImage' {} a -> s {kernelId = a} :: RegisterImage)

-- | The ID of the RAM disk.
registerImage_ramdiskId :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_ramdiskId = Lens.lens (\RegisterImage' {ramdiskId} -> ramdiskId) (\s@RegisterImage' {} a -> s {ramdiskId = a} :: RegisterImage)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
registerImage_rootDeviceName :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_rootDeviceName = Lens.lens (\RegisterImage' {rootDeviceName} -> rootDeviceName) (\s@RegisterImage' {} a -> s {rootDeviceName = a} :: RegisterImage)

-- | Set to @simple@ to enable enhanced networking with the Intel 82599
-- Virtual Function interface for the AMI and any instances that you launch
-- from the AMI.
--
-- There is no way to disable @sriovNetSupport@ at this time.
--
-- This option is supported only for HVM AMIs. Specifying this option with
-- a PV AMI can make instances launched from the AMI unreachable.
registerImage_sriovNetSupport :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_sriovNetSupport = Lens.lens (\RegisterImage' {sriovNetSupport} -> sriovNetSupport) (\s@RegisterImage' {} a -> s {sriovNetSupport = a} :: RegisterImage)

-- | Set to @v2.0@ to enable Trusted Platform Module (TPM) support. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/nitrotpm.html NitroTPM>
-- in the /Amazon EC2 User Guide/.
registerImage_tpmSupport :: Lens.Lens' RegisterImage (Prelude.Maybe TpmSupportValues)
registerImage_tpmSupport = Lens.lens (\RegisterImage' {tpmSupport} -> tpmSupport) (\s@RegisterImage' {} a -> s {tpmSupport = a} :: RegisterImage)

-- | Base64 representation of the non-volatile UEFI variable store. To
-- retrieve the UEFI data, use the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_GetInstanceUefiData GetInstanceUefiData>
-- command. You can inspect and modify the UEFI data by using the
-- <https://github.com/awslabs/python-uefivars python-uefivars tool> on
-- GitHub. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/uefi-secure-boot.html UEFI Secure Boot>
-- in the /Amazon EC2 User Guide/.
registerImage_uefiData :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_uefiData = Lens.lens (\RegisterImage' {uefiData} -> uefiData) (\s@RegisterImage' {} a -> s {uefiData = a} :: RegisterImage)

-- | The type of virtualization (@hvm@ | @paravirtual@).
--
-- Default: @paravirtual@
registerImage_virtualizationType :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_virtualizationType = Lens.lens (\RegisterImage' {virtualizationType} -> virtualizationType) (\s@RegisterImage' {} a -> s {virtualizationType = a} :: RegisterImage)

-- | A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
registerImage_name :: Lens.Lens' RegisterImage Prelude.Text
registerImage_name = Lens.lens (\RegisterImage' {name} -> name) (\s@RegisterImage' {} a -> s {name = a} :: RegisterImage)

instance Core.AWSRequest RegisterImage where
  type
    AWSResponse RegisterImage =
      RegisterImageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterImageResponse'
            Prelude.<$> (x Data..@? "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterImage where
  hashWithSalt _salt RegisterImage' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` billingProducts
      `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` bootMode
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` enaSupport
      `Prelude.hashWithSalt` imageLocation
      `Prelude.hashWithSalt` imdsSupport
      `Prelude.hashWithSalt` kernelId
      `Prelude.hashWithSalt` ramdiskId
      `Prelude.hashWithSalt` rootDeviceName
      `Prelude.hashWithSalt` sriovNetSupport
      `Prelude.hashWithSalt` tpmSupport
      `Prelude.hashWithSalt` uefiData
      `Prelude.hashWithSalt` virtualizationType
      `Prelude.hashWithSalt` name

instance Prelude.NFData RegisterImage where
  rnf RegisterImage' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf billingProducts
      `Prelude.seq` Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf bootMode
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf enaSupport
      `Prelude.seq` Prelude.rnf imageLocation
      `Prelude.seq` Prelude.rnf imdsSupport
      `Prelude.seq` Prelude.rnf kernelId
      `Prelude.seq` Prelude.rnf ramdiskId
      `Prelude.seq` Prelude.rnf rootDeviceName
      `Prelude.seq` Prelude.rnf sriovNetSupport
      `Prelude.seq` Prelude.rnf tpmSupport
      `Prelude.seq` Prelude.rnf uefiData
      `Prelude.seq` Prelude.rnf virtualizationType
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders RegisterImage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RegisterImage where
  toPath = Prelude.const "/"

instance Data.ToQuery RegisterImage where
  toQuery RegisterImage' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RegisterImage" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "Architecture" Data.=: architecture,
        Data.toQuery
          ( Data.toQueryList "BillingProduct"
              Prelude.<$> billingProducts
          ),
        Data.toQuery
          ( Data.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "BootMode" Data.=: bootMode,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "EnaSupport" Data.=: enaSupport,
        "ImageLocation" Data.=: imageLocation,
        "ImdsSupport" Data.=: imdsSupport,
        "KernelId" Data.=: kernelId,
        "RamdiskId" Data.=: ramdiskId,
        "RootDeviceName" Data.=: rootDeviceName,
        "SriovNetSupport" Data.=: sriovNetSupport,
        "TpmSupport" Data.=: tpmSupport,
        "UefiData" Data.=: uefiData,
        "VirtualizationType" Data.=: virtualizationType,
        "Name" Data.=: name
      ]

-- | Contains the output of RegisterImage.
--
-- /See:/ 'newRegisterImageResponse' smart constructor.
data RegisterImageResponse = RegisterImageResponse'
  { -- | The ID of the newly registered AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegisterImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'registerImageResponse_imageId' - The ID of the newly registered AMI.
--
-- 'httpStatus', 'registerImageResponse_httpStatus' - The response's http status code.
newRegisterImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegisterImageResponse
newRegisterImageResponse pHttpStatus_ =
  RegisterImageResponse'
    { imageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the newly registered AMI.
registerImageResponse_imageId :: Lens.Lens' RegisterImageResponse (Prelude.Maybe Prelude.Text)
registerImageResponse_imageId = Lens.lens (\RegisterImageResponse' {imageId} -> imageId) (\s@RegisterImageResponse' {} a -> s {imageId = a} :: RegisterImageResponse)

-- | The response's http status code.
registerImageResponse_httpStatus :: Lens.Lens' RegisterImageResponse Prelude.Int
registerImageResponse_httpStatus = Lens.lens (\RegisterImageResponse' {httpStatus} -> httpStatus) (\s@RegisterImageResponse' {} a -> s {httpStatus = a} :: RegisterImageResponse)

instance Prelude.NFData RegisterImageResponse where
  rnf RegisterImageResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
