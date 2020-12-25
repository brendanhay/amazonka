{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    riName,
    riArchitecture,
    riBillingProducts,
    riBlockDeviceMappings,
    riDescription,
    riDryRun,
    riEnaSupport,
    riImageLocation,
    riKernelId,
    riRamdiskId,
    riRootDeviceName,
    riSriovNetSupport,
    riVirtualizationType,

    -- * Destructuring the response
    RegisterImageResponse (..),
    mkRegisterImageResponse,

    -- ** Response lenses
    rirrsImageId,
    rirrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RegisterImage.
--
-- /See:/ 'mkRegisterImage' smart constructor.
data RegisterImage = RegisterImage'
  { -- | A name for your AMI.
    --
    -- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
    name :: Types.Name,
    -- | The architecture of the AMI.
    --
    -- Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
    architecture :: Core.Maybe Types.ArchitectureValues,
    -- | The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
    billingProducts :: Core.Maybe [Types.String],
    -- | The block device mapping entries.
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | A description for your AMI.
    description :: Core.Maybe Types.Description,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI.
    --
    -- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
    enaSupport :: Core.Maybe Core.Bool,
    -- | The full path to your AMI manifest in Amazon S3 storage. The specified bucket must have the @aws-exec-read@ canned access control list (ACL) to ensure that it can be accessed by Amazon EC2. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs> in the /Amazon S3 Service Developer Guide/ .
    imageLocation :: Core.Maybe Types.ImageLocation,
    -- | The ID of the kernel.
    kernelId :: Core.Maybe Types.KernelId,
    -- | The ID of the RAM disk.
    ramdiskId :: Core.Maybe Types.RamdiskId,
    -- | The device name of the root device volume (for example, @/dev/sda1@ ).
    rootDeviceName :: Core.Maybe Types.RootDeviceName,
    -- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI.
    --
    -- There is no way to disable @sriovNetSupport@ at this time.
    -- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
    sriovNetSupport :: Core.Maybe Types.SriovNetSupport,
    -- | The type of virtualization (@hvm@ | @paravirtual@ ).
    --
    -- Default: @paravirtual@
    virtualizationType :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterImage' value with any optional fields omitted.
mkRegisterImage ::
  -- | 'name'
  Types.Name ->
  RegisterImage
mkRegisterImage name =
  RegisterImage'
    { name,
      architecture = Core.Nothing,
      billingProducts = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      description = Core.Nothing,
      dryRun = Core.Nothing,
      enaSupport = Core.Nothing,
      imageLocation = Core.Nothing,
      kernelId = Core.Nothing,
      ramdiskId = Core.Nothing,
      rootDeviceName = Core.Nothing,
      sriovNetSupport = Core.Nothing,
      virtualizationType = Core.Nothing
    }

-- | A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riName :: Lens.Lens' RegisterImage Types.Name
riName = Lens.field @"name"
{-# DEPRECATED riName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riArchitecture :: Lens.Lens' RegisterImage (Core.Maybe Types.ArchitectureValues)
riArchitecture = Lens.field @"architecture"
{-# DEPRECATED riArchitecture "Use generic-lens or generic-optics with 'architecture' instead." #-}

-- | The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
--
-- /Note:/ Consider using 'billingProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riBillingProducts :: Lens.Lens' RegisterImage (Core.Maybe [Types.String])
riBillingProducts = Lens.field @"billingProducts"
{-# DEPRECATED riBillingProducts "Use generic-lens or generic-optics with 'billingProducts' instead." #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riBlockDeviceMappings :: Lens.Lens' RegisterImage (Core.Maybe [Types.BlockDeviceMapping])
riBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED riBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | A description for your AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDescription :: Lens.Lens' RegisterImage (Core.Maybe Types.Description)
riDescription = Lens.field @"description"
{-# DEPRECATED riDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDryRun :: Lens.Lens' RegisterImage (Core.Maybe Core.Bool)
riDryRun = Lens.field @"dryRun"
{-# DEPRECATED riDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI.
--
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEnaSupport :: Lens.Lens' RegisterImage (Core.Maybe Core.Bool)
riEnaSupport = Lens.field @"enaSupport"
{-# DEPRECATED riEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | The full path to your AMI manifest in Amazon S3 storage. The specified bucket must have the @aws-exec-read@ canned access control list (ACL) to ensure that it can be accessed by Amazon EC2. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'imageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riImageLocation :: Lens.Lens' RegisterImage (Core.Maybe Types.ImageLocation)
riImageLocation = Lens.field @"imageLocation"
{-# DEPRECATED riImageLocation "Use generic-lens or generic-optics with 'imageLocation' instead." #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riKernelId :: Lens.Lens' RegisterImage (Core.Maybe Types.KernelId)
riKernelId = Lens.field @"kernelId"
{-# DEPRECATED riKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRamdiskId :: Lens.Lens' RegisterImage (Core.Maybe Types.RamdiskId)
riRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED riRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRootDeviceName :: Lens.Lens' RegisterImage (Core.Maybe Types.RootDeviceName)
riRootDeviceName = Lens.field @"rootDeviceName"
{-# DEPRECATED riRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI.
--
-- There is no way to disable @sriovNetSupport@ at this time.
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riSriovNetSupport :: Lens.Lens' RegisterImage (Core.Maybe Types.SriovNetSupport)
riSriovNetSupport = Lens.field @"sriovNetSupport"
{-# DEPRECATED riSriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The type of virtualization (@hvm@ | @paravirtual@ ).
--
-- Default: @paravirtual@
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riVirtualizationType :: Lens.Lens' RegisterImage (Core.Maybe Types.String)
riVirtualizationType = Lens.field @"virtualizationType"
{-# DEPRECATED riVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

instance Core.AWSRequest RegisterImage where
  type Rs RegisterImage = RegisterImageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "RegisterImage")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Name" name)
                Core.<> (Core.toQueryValue "Architecture" Core.<$> architecture)
                Core.<> (Core.toQueryList "BillingProduct" Core.<$> billingProducts)
                Core.<> ( Core.toQueryList "BlockDeviceMapping"
                            Core.<$> blockDeviceMappings
                        )
                Core.<> (Core.toQueryValue "Description" Core.<$> description)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryValue "EnaSupport" Core.<$> enaSupport)
                Core.<> (Core.toQueryValue "ImageLocation" Core.<$> imageLocation)
                Core.<> (Core.toQueryValue "KernelId" Core.<$> kernelId)
                Core.<> (Core.toQueryValue "RamdiskId" Core.<$> ramdiskId)
                Core.<> (Core.toQueryValue "RootDeviceName" Core.<$> rootDeviceName)
                Core.<> (Core.toQueryValue "SriovNetSupport" Core.<$> sriovNetSupport)
                Core.<> ( Core.toQueryValue "VirtualizationType"
                            Core.<$> virtualizationType
                        )
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterImageResponse'
            Core.<$> (x Core..@? "imageId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of RegisterImage.
--
-- /See:/ 'mkRegisterImageResponse' smart constructor.
data RegisterImageResponse = RegisterImageResponse'
  { -- | The ID of the newly registered AMI.
    imageId :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterImageResponse' value with any optional fields omitted.
mkRegisterImageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RegisterImageResponse
mkRegisterImageResponse responseStatus =
  RegisterImageResponse' {imageId = Core.Nothing, responseStatus}

-- | The ID of the newly registered AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsImageId :: Lens.Lens' RegisterImageResponse (Core.Maybe Types.String)
rirrsImageId = Lens.field @"imageId"
{-# DEPRECATED rirrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsResponseStatus :: Lens.Lens' RegisterImageResponse Core.Int
rirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
