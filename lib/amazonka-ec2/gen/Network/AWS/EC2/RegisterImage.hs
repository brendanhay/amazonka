{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.RegisterImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AMI. When you're creating an AMI, this is the final step you must complete before you can launch an instance from the AMI. For more information about creating AMIs, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami.html Creating your own AMIs> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can also use @RegisterImage@ to create an Amazon EBS-backed Linux AMI from a snapshot of a root device volume. You specify the snapshot using the block device mapping. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-launch-snapshot.html Launching a Linux instance from a backup> in the /Amazon Elastic Compute Cloud User Guide/ .
-- If any snapshots have AWS Marketplace product codes, they are copied to the new AMI.
-- Windows and some Linux distributions, such as Red Hat Enterprise Linux (RHEL) and SUSE Linux Enterprise Server (SLES), use the EC2 billing product code associated with an AMI to verify the subscription status for package updates. To create a new AMI for operating systems that require a billing product code, instead of registering the AMI, do the following to preserve the billing product code association:
--
--     * Launch an instance from an existing AMI with that billing product code.
--
--
--     * Customize the instance.
--
--
--     * Create an AMI from the instance using 'CreateImage' .
--
--
-- If you purchase a Reserved Instance to apply to an On-Demand Instance that was launched from an AMI with a billing product code, make sure that the Reserved Instance has the matching billing product code. If you purchase a Reserved Instance without the matching billing product code, the Reserved Instance will not be applied to the On-Demand Instance. For information about how to obtain the platform details and billing information of an AMI, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ami-billing-info.html Obtaining billing information> in the /Amazon Elastic Compute Cloud User Guide/ .
-- If needed, you can deregister an AMI at any time. Any modifications you make to an AMI backed by an instance store volume invalidates its registration. If you make changes to an image, deregister the previous image and register the new image.
module Network.AWS.EC2.RegisterImage
  ( -- * Creating a request
    RegisterImage (..),
    mkRegisterImage,

    -- ** Request lenses
    riVirtualizationType,
    riImageLocation,
    riEnaSupport,
    riBillingProducts,
    riRAMDiskId,
    riKernelId,
    riRootDeviceName,
    riSRIOVNetSupport,
    riArchitecture,
    riDescription,
    riBlockDeviceMappings,
    riDryRun,
    riName,

    -- * Destructuring the response
    RegisterImageResponse (..),
    mkRegisterImageResponse,

    -- ** Response lenses
    rirsImageId,
    rirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for RegisterImage.
--
-- /See:/ 'mkRegisterImage' smart constructor.
data RegisterImage = RegisterImage'
  { virtualizationType ::
      Lude.Maybe Lude.Text,
    imageLocation :: Lude.Maybe Lude.Text,
    enaSupport :: Lude.Maybe Lude.Bool,
    billingProducts :: Lude.Maybe [Lude.Text],
    ramdiskId :: Lude.Maybe Lude.Text,
    kernelId :: Lude.Maybe Lude.Text,
    rootDeviceName :: Lude.Maybe Lude.Text,
    sriovNetSupport :: Lude.Maybe Lude.Text,
    architecture :: Lude.Maybe ArchitectureValues,
    description :: Lude.Maybe Lude.Text,
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping],
    dryRun :: Lude.Maybe Lude.Bool,
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterImage' with the minimum fields required to make a request.
--
-- * 'architecture' - The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
-- * 'billingProducts' - The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
-- * 'blockDeviceMappings' - The block device mapping entries.
-- * 'description' - A description for your AMI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'enaSupport' - Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI.
--
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
-- * 'imageLocation' - The full path to your AMI manifest in Amazon S3 storage. The specified bucket must have the @aws-exec-read@ canned access control list (ACL) to ensure that it can be accessed by Amazon EC2. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs> in the /Amazon S3 Service Developer Guide/ .
-- * 'kernelId' - The ID of the kernel.
-- * 'name' - A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
-- * 'ramdiskId' - The ID of the RAM disk.
-- * 'rootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
-- * 'sriovNetSupport' - Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI.
--
-- There is no way to disable @sriovNetSupport@ at this time.
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
-- * 'virtualizationType' - The type of virtualization (@hvm@ | @paravirtual@ ).
--
-- Default: @paravirtual@
mkRegisterImage ::
  -- | 'name'
  Lude.Text ->
  RegisterImage
mkRegisterImage pName_ =
  RegisterImage'
    { virtualizationType = Lude.Nothing,
      imageLocation = Lude.Nothing,
      enaSupport = Lude.Nothing,
      billingProducts = Lude.Nothing,
      ramdiskId = Lude.Nothing,
      kernelId = Lude.Nothing,
      rootDeviceName = Lude.Nothing,
      sriovNetSupport = Lude.Nothing,
      architecture = Lude.Nothing,
      description = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      dryRun = Lude.Nothing,
      name = pName_
    }

-- | The type of virtualization (@hvm@ | @paravirtual@ ).
--
-- Default: @paravirtual@
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riVirtualizationType :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Text)
riVirtualizationType = Lens.lens (virtualizationType :: RegisterImage -> Lude.Maybe Lude.Text) (\s a -> s {virtualizationType = a} :: RegisterImage)
{-# DEPRECATED riVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

-- | The full path to your AMI manifest in Amazon S3 storage. The specified bucket must have the @aws-exec-read@ canned access control list (ACL) to ensure that it can be accessed by Amazon EC2. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'imageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riImageLocation :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Text)
riImageLocation = Lens.lens (imageLocation :: RegisterImage -> Lude.Maybe Lude.Text) (\s a -> s {imageLocation = a} :: RegisterImage)
{-# DEPRECATED riImageLocation "Use generic-lens or generic-optics with 'imageLocation' instead." #-}

-- | Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI.
--
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEnaSupport :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Bool)
riEnaSupport = Lens.lens (enaSupport :: RegisterImage -> Lude.Maybe Lude.Bool) (\s a -> s {enaSupport = a} :: RegisterImage)
{-# DEPRECATED riEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
--
-- /Note:/ Consider using 'billingProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riBillingProducts :: Lens.Lens' RegisterImage (Lude.Maybe [Lude.Text])
riBillingProducts = Lens.lens (billingProducts :: RegisterImage -> Lude.Maybe [Lude.Text]) (\s a -> s {billingProducts = a} :: RegisterImage)
{-# DEPRECATED riBillingProducts "Use generic-lens or generic-optics with 'billingProducts' instead." #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRAMDiskId :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Text)
riRAMDiskId = Lens.lens (ramdiskId :: RegisterImage -> Lude.Maybe Lude.Text) (\s a -> s {ramdiskId = a} :: RegisterImage)
{-# DEPRECATED riRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riKernelId :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Text)
riKernelId = Lens.lens (kernelId :: RegisterImage -> Lude.Maybe Lude.Text) (\s a -> s {kernelId = a} :: RegisterImage)
{-# DEPRECATED riKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRootDeviceName :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Text)
riRootDeviceName = Lens.lens (rootDeviceName :: RegisterImage -> Lude.Maybe Lude.Text) (\s a -> s {rootDeviceName = a} :: RegisterImage)
{-# DEPRECATED riRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI.
--
-- There is no way to disable @sriovNetSupport@ at this time.
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riSRIOVNetSupport :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Text)
riSRIOVNetSupport = Lens.lens (sriovNetSupport :: RegisterImage -> Lude.Maybe Lude.Text) (\s a -> s {sriovNetSupport = a} :: RegisterImage)
{-# DEPRECATED riSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riArchitecture :: Lens.Lens' RegisterImage (Lude.Maybe ArchitectureValues)
riArchitecture = Lens.lens (architecture :: RegisterImage -> Lude.Maybe ArchitectureValues) (\s a -> s {architecture = a} :: RegisterImage)
{-# DEPRECATED riArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | A description for your AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDescription :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Text)
riDescription = Lens.lens (description :: RegisterImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RegisterImage)
{-# DEPRECATED riDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riBlockDeviceMappings :: Lens.Lens' RegisterImage (Lude.Maybe [BlockDeviceMapping])
riBlockDeviceMappings = Lens.lens (blockDeviceMappings :: RegisterImage -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: RegisterImage)
{-# DEPRECATED riBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDryRun :: Lens.Lens' RegisterImage (Lude.Maybe Lude.Bool)
riDryRun = Lens.lens (dryRun :: RegisterImage -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: RegisterImage)
{-# DEPRECATED riDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riName :: Lens.Lens' RegisterImage Lude.Text
riName = Lens.lens (name :: RegisterImage -> Lude.Text) (\s a -> s {name = a} :: RegisterImage)
{-# DEPRECATED riName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest RegisterImage where
  type Rs RegisterImage = RegisterImageResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          RegisterImageResponse'
            Lude.<$> (x Lude..@? "imageId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterImage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterImage where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterImage where
  toQuery RegisterImage' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RegisterImage" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "VirtualizationType" Lude.=: virtualizationType,
        "ImageLocation" Lude.=: imageLocation,
        "EnaSupport" Lude.=: enaSupport,
        Lude.toQuery
          (Lude.toQueryList "BillingProduct" Lude.<$> billingProducts),
        "RamdiskId" Lude.=: ramdiskId,
        "KernelId" Lude.=: kernelId,
        "RootDeviceName" Lude.=: rootDeviceName,
        "SriovNetSupport" Lude.=: sriovNetSupport,
        "Architecture" Lude.=: architecture,
        "Description" Lude.=: description,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "DryRun" Lude.=: dryRun,
        "Name" Lude.=: name
      ]

-- | Contains the output of RegisterImage.
--
-- /See:/ 'mkRegisterImageResponse' smart constructor.
data RegisterImageResponse = RegisterImageResponse'
  { imageId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterImageResponse' with the minimum fields required to make a request.
--
-- * 'imageId' - The ID of the newly registered AMI.
-- * 'responseStatus' - The response status code.
mkRegisterImageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterImageResponse
mkRegisterImageResponse pResponseStatus_ =
  RegisterImageResponse'
    { imageId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the newly registered AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirsImageId :: Lens.Lens' RegisterImageResponse (Lude.Maybe Lude.Text)
rirsImageId = Lens.lens (imageId :: RegisterImageResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: RegisterImageResponse)
{-# DEPRECATED rirsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirsResponseStatus :: Lens.Lens' RegisterImageResponse Lude.Int
rirsResponseStatus = Lens.lens (responseStatus :: RegisterImageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterImageResponse)
{-# DEPRECATED rirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
