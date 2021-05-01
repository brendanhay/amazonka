{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.RegisterImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AMI. When you\'re creating an AMI, this is the final step
-- you must complete before you can launch an instance from the AMI. For
-- more information about creating AMIs, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami.html Creating your own AMIs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For Amazon EBS-backed instances, CreateImage creates and registers the
-- AMI in a single request, so you don\'t have to register the AMI
-- yourself.
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
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AMIEncryption.html Use encryption with EBS-backed AMIs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- __AWS Marketplace product codes__
--
-- If any snapshots have AWS Marketplace product codes, they are copied to
-- the new AMI.
--
-- Windows and some Linux distributions, such as Red Hat Enterprise Linux
-- (RHEL) and SUSE Linux Enterprise Server (SLES), use the EC2 billing
-- product code associated with an AMI to verify the subscription status
-- for package updates. To create a new AMI for operating systems that
-- require a billing product code, instead of registering the AMI, do the
-- following to preserve the billing product code association:
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
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining billing information>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.RegisterImage
  ( -- * Creating a Request
    RegisterImage (..),
    newRegisterImage,

    -- * Request Lenses
    registerImage_virtualizationType,
    registerImage_rootDeviceName,
    registerImage_dryRun,
    registerImage_ramdiskId,
    registerImage_architecture,
    registerImage_sriovNetSupport,
    registerImage_blockDeviceMappings,
    registerImage_kernelId,
    registerImage_description,
    registerImage_billingProducts,
    registerImage_enaSupport,
    registerImage_imageLocation,
    registerImage_name,

    -- * Destructuring the Response
    RegisterImageResponse (..),
    newRegisterImageResponse,

    -- * Response Lenses
    registerImageResponse_imageId,
    registerImageResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RegisterImage.
--
-- /See:/ 'newRegisterImage' smart constructor.
data RegisterImage = RegisterImage'
  { -- | The type of virtualization (@hvm@ | @paravirtual@).
    --
    -- Default: @paravirtual@
    virtualizationType :: Prelude.Maybe Prelude.Text,
    -- | The device name of the root device volume (for example, @\/dev\/sda1@).
    rootDeviceName :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the RAM disk.
    ramdiskId :: Prelude.Maybe Prelude.Text,
    -- | The architecture of the AMI.
    --
    -- Default: For Amazon EBS-backed AMIs, @i386@. For instance store-backed
    -- AMIs, the architecture specified in the manifest file.
    architecture :: Prelude.Maybe ArchitectureValues,
    -- | Set to @simple@ to enable enhanced networking with the Intel 82599
    -- Virtual Function interface for the AMI and any instances that you launch
    -- from the AMI.
    --
    -- There is no way to disable @sriovNetSupport@ at this time.
    --
    -- This option is supported only for HVM AMIs. Specifying this option with
    -- a PV AMI can make instances launched from the AMI unreachable.
    sriovNetSupport :: Prelude.Maybe Prelude.Text,
    -- | The block device mapping entries.
    --
    -- If you specify an EBS volume using the ID of an EBS snapshot, you can\'t
    -- specify the encryption state of the volume.
    --
    -- If you create an AMI on an Outpost, then all backing snapshots must be
    -- on the same Outpost or in the Region of that Outpost. AMIs on an Outpost
    -- that include local snapshots can be used to launch instances on the same
    -- Outpost only. For more information,
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
    -- in the /Amazon Elastic Compute Cloud User Guide/.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | The ID of the kernel.
    kernelId :: Prelude.Maybe Prelude.Text,
    -- | A description for your AMI.
    description :: Prelude.Maybe Prelude.Text,
    -- | The billing product codes. Your account must be authorized to specify
    -- billing product codes. Otherwise, you can use the AWS Marketplace to
    -- bill for the use of an AMI.
    billingProducts :: Prelude.Maybe [Prelude.Text],
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
    -- | A name for your AMI.
    --
    -- Constraints: 3-128 alphanumeric characters, parentheses (()), square
    -- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
    -- quotes (\'), at-signs (\@), or underscores(_)
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RegisterImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualizationType', 'registerImage_virtualizationType' - The type of virtualization (@hvm@ | @paravirtual@).
--
-- Default: @paravirtual@
--
-- 'rootDeviceName', 'registerImage_rootDeviceName' - The device name of the root device volume (for example, @\/dev\/sda1@).
--
-- 'dryRun', 'registerImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'ramdiskId', 'registerImage_ramdiskId' - The ID of the RAM disk.
--
-- 'architecture', 'registerImage_architecture' - The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@. For instance store-backed
-- AMIs, the architecture specified in the manifest file.
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
-- 'blockDeviceMappings', 'registerImage_blockDeviceMappings' - The block device mapping entries.
--
-- If you specify an EBS volume using the ID of an EBS snapshot, you can\'t
-- specify the encryption state of the volume.
--
-- If you create an AMI on an Outpost, then all backing snapshots must be
-- on the same Outpost or in the Region of that Outpost. AMIs on an Outpost
-- that include local snapshots can be used to launch instances on the same
-- Outpost only. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- 'kernelId', 'registerImage_kernelId' - The ID of the kernel.
--
-- 'description', 'registerImage_description' - A description for your AMI.
--
-- 'billingProducts', 'registerImage_billingProducts' - The billing product codes. Your account must be authorized to specify
-- billing product codes. Otherwise, you can use the AWS Marketplace to
-- bill for the use of an AMI.
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
    { virtualizationType =
        Prelude.Nothing,
      rootDeviceName = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      ramdiskId = Prelude.Nothing,
      architecture = Prelude.Nothing,
      sriovNetSupport = Prelude.Nothing,
      blockDeviceMappings = Prelude.Nothing,
      kernelId = Prelude.Nothing,
      description = Prelude.Nothing,
      billingProducts = Prelude.Nothing,
      enaSupport = Prelude.Nothing,
      imageLocation = Prelude.Nothing,
      name = pName_
    }

-- | The type of virtualization (@hvm@ | @paravirtual@).
--
-- Default: @paravirtual@
registerImage_virtualizationType :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_virtualizationType = Lens.lens (\RegisterImage' {virtualizationType} -> virtualizationType) (\s@RegisterImage' {} a -> s {virtualizationType = a} :: RegisterImage)

-- | The device name of the root device volume (for example, @\/dev\/sda1@).
registerImage_rootDeviceName :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_rootDeviceName = Lens.lens (\RegisterImage' {rootDeviceName} -> rootDeviceName) (\s@RegisterImage' {} a -> s {rootDeviceName = a} :: RegisterImage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
registerImage_dryRun :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Bool)
registerImage_dryRun = Lens.lens (\RegisterImage' {dryRun} -> dryRun) (\s@RegisterImage' {} a -> s {dryRun = a} :: RegisterImage)

-- | The ID of the RAM disk.
registerImage_ramdiskId :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_ramdiskId = Lens.lens (\RegisterImage' {ramdiskId} -> ramdiskId) (\s@RegisterImage' {} a -> s {ramdiskId = a} :: RegisterImage)

-- | The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@. For instance store-backed
-- AMIs, the architecture specified in the manifest file.
registerImage_architecture :: Lens.Lens' RegisterImage (Prelude.Maybe ArchitectureValues)
registerImage_architecture = Lens.lens (\RegisterImage' {architecture} -> architecture) (\s@RegisterImage' {} a -> s {architecture = a} :: RegisterImage)

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

-- | The block device mapping entries.
--
-- If you specify an EBS volume using the ID of an EBS snapshot, you can\'t
-- specify the encryption state of the volume.
--
-- If you create an AMI on an Outpost, then all backing snapshots must be
-- on the same Outpost or in the Region of that Outpost. AMIs on an Outpost
-- that include local snapshots can be used to launch instances on the same
-- Outpost only. For more information,
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/snapshots-outposts.html#ami Amazon EBS local snapshots on Outposts>
-- in the /Amazon Elastic Compute Cloud User Guide/.
registerImage_blockDeviceMappings :: Lens.Lens' RegisterImage (Prelude.Maybe [BlockDeviceMapping])
registerImage_blockDeviceMappings = Lens.lens (\RegisterImage' {blockDeviceMappings} -> blockDeviceMappings) (\s@RegisterImage' {} a -> s {blockDeviceMappings = a} :: RegisterImage) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the kernel.
registerImage_kernelId :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_kernelId = Lens.lens (\RegisterImage' {kernelId} -> kernelId) (\s@RegisterImage' {} a -> s {kernelId = a} :: RegisterImage)

-- | A description for your AMI.
registerImage_description :: Lens.Lens' RegisterImage (Prelude.Maybe Prelude.Text)
registerImage_description = Lens.lens (\RegisterImage' {description} -> description) (\s@RegisterImage' {} a -> s {description = a} :: RegisterImage)

-- | The billing product codes. Your account must be authorized to specify
-- billing product codes. Otherwise, you can use the AWS Marketplace to
-- bill for the use of an AMI.
registerImage_billingProducts :: Lens.Lens' RegisterImage (Prelude.Maybe [Prelude.Text])
registerImage_billingProducts = Lens.lens (\RegisterImage' {billingProducts} -> billingProducts) (\s@RegisterImage' {} a -> s {billingProducts = a} :: RegisterImage) Prelude.. Lens.mapping Prelude._Coerce

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

-- | A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
registerImage_name :: Lens.Lens' RegisterImage Prelude.Text
registerImage_name = Lens.lens (\RegisterImage' {name} -> name) (\s@RegisterImage' {} a -> s {name = a} :: RegisterImage)

instance Prelude.AWSRequest RegisterImage where
  type Rs RegisterImage = RegisterImageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterImageResponse'
            Prelude.<$> (x Prelude..@? "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegisterImage

instance Prelude.NFData RegisterImage

instance Prelude.ToHeaders RegisterImage where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath RegisterImage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RegisterImage where
  toQuery RegisterImage' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("RegisterImage" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "VirtualizationType" Prelude.=: virtualizationType,
        "RootDeviceName" Prelude.=: rootDeviceName,
        "DryRun" Prelude.=: dryRun,
        "RamdiskId" Prelude.=: ramdiskId,
        "Architecture" Prelude.=: architecture,
        "SriovNetSupport" Prelude.=: sriovNetSupport,
        Prelude.toQuery
          ( Prelude.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "KernelId" Prelude.=: kernelId,
        "Description" Prelude.=: description,
        Prelude.toQuery
          ( Prelude.toQueryList "BillingProduct"
              Prelude.<$> billingProducts
          ),
        "EnaSupport" Prelude.=: enaSupport,
        "ImageLocation" Prelude.=: imageLocation,
        "Name" Prelude.=: name
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData RegisterImageResponse
