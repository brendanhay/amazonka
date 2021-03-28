{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RegisterImage (..)
    , mkRegisterImage
    -- ** Request lenses
    , riName
    , riArchitecture
    , riBillingProducts
    , riBlockDeviceMappings
    , riDescription
    , riDryRun
    , riEnaSupport
    , riImageLocation
    , riKernelId
    , riRamdiskId
    , riRootDeviceName
    , riSriovNetSupport
    , riVirtualizationType

    -- * Destructuring the response
    , RegisterImageResponse (..)
    , mkRegisterImageResponse
    -- ** Response lenses
    , rirrsImageId
    , rirrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for RegisterImage.
--
-- /See:/ 'mkRegisterImage' smart constructor.
data RegisterImage = RegisterImage'
  { name :: Core.Text
    -- ^ A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
  , architecture :: Core.Maybe Types.ArchitectureValues
    -- ^ The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
  , billingProducts :: Core.Maybe [Core.Text]
    -- ^ The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
  , blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ The block device mapping entries.
  , description :: Core.Maybe Core.Text
    -- ^ A description for your AMI.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , enaSupport :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI.
--
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
  , imageLocation :: Core.Maybe Core.Text
    -- ^ The full path to your AMI manifest in Amazon S3 storage. The specified bucket must have the @aws-exec-read@ canned access control list (ACL) to ensure that it can be accessed by Amazon EC2. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs> in the /Amazon S3 Service Developer Guide/ .
  , kernelId :: Core.Maybe Types.KernelId
    -- ^ The ID of the kernel.
  , ramdiskId :: Core.Maybe Types.RamdiskId
    -- ^ The ID of the RAM disk.
  , rootDeviceName :: Core.Maybe Core.Text
    -- ^ The device name of the root device volume (for example, @/dev/sda1@ ).
  , sriovNetSupport :: Core.Maybe Core.Text
    -- ^ Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI.
--
-- There is no way to disable @sriovNetSupport@ at this time.
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
  , virtualizationType :: Core.Maybe Core.Text
    -- ^ The type of virtualization (@hvm@ | @paravirtual@ ).
--
-- Default: @paravirtual@ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterImage' value with any optional fields omitted.
mkRegisterImage
    :: Core.Text -- ^ 'name'
    -> RegisterImage
mkRegisterImage name
  = RegisterImage'{name, architecture = Core.Nothing,
                   billingProducts = Core.Nothing, blockDeviceMappings = Core.Nothing,
                   description = Core.Nothing, dryRun = Core.Nothing,
                   enaSupport = Core.Nothing, imageLocation = Core.Nothing,
                   kernelId = Core.Nothing, ramdiskId = Core.Nothing,
                   rootDeviceName = Core.Nothing, sriovNetSupport = Core.Nothing,
                   virtualizationType = Core.Nothing}

-- | A name for your AMI.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square brackets ([]), spaces ( ), periods (.), slashes (/), dashes (-), single quotes ('), at-signs (@), or underscores(_)
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riName :: Lens.Lens' RegisterImage Core.Text
riName = Lens.field @"name"
{-# INLINEABLE riName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The architecture of the AMI.
--
-- Default: For Amazon EBS-backed AMIs, @i386@ . For instance store-backed AMIs, the architecture specified in the manifest file.
--
-- /Note:/ Consider using 'architecture' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riArchitecture :: Lens.Lens' RegisterImage (Core.Maybe Types.ArchitectureValues)
riArchitecture = Lens.field @"architecture"
{-# INLINEABLE riArchitecture #-}
{-# DEPRECATED architecture "Use generic-lens or generic-optics with 'architecture' instead"  #-}

-- | The billing product codes. Your account must be authorized to specify billing product codes. Otherwise, you can use the AWS Marketplace to bill for the use of an AMI.
--
-- /Note:/ Consider using 'billingProducts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riBillingProducts :: Lens.Lens' RegisterImage (Core.Maybe [Core.Text])
riBillingProducts = Lens.field @"billingProducts"
{-# INLINEABLE riBillingProducts #-}
{-# DEPRECATED billingProducts "Use generic-lens or generic-optics with 'billingProducts' instead"  #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riBlockDeviceMappings :: Lens.Lens' RegisterImage (Core.Maybe [Types.BlockDeviceMapping])
riBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE riBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | A description for your AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDescription :: Lens.Lens' RegisterImage (Core.Maybe Core.Text)
riDescription = Lens.field @"description"
{-# INLINEABLE riDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riDryRun :: Lens.Lens' RegisterImage (Core.Maybe Core.Bool)
riDryRun = Lens.field @"dryRun"
{-# INLINEABLE riDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Set to @true@ to enable enhanced networking with ENA for the AMI and any instances that you launch from the AMI.
--
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riEnaSupport :: Lens.Lens' RegisterImage (Core.Maybe Core.Bool)
riEnaSupport = Lens.field @"enaSupport"
{-# INLINEABLE riEnaSupport #-}
{-# DEPRECATED enaSupport "Use generic-lens or generic-optics with 'enaSupport' instead"  #-}

-- | The full path to your AMI manifest in Amazon S3 storage. The specified bucket must have the @aws-exec-read@ canned access control list (ACL) to ensure that it can be accessed by Amazon EC2. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl Canned ACLs> in the /Amazon S3 Service Developer Guide/ .
--
-- /Note:/ Consider using 'imageLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riImageLocation :: Lens.Lens' RegisterImage (Core.Maybe Core.Text)
riImageLocation = Lens.field @"imageLocation"
{-# INLINEABLE riImageLocation #-}
{-# DEPRECATED imageLocation "Use generic-lens or generic-optics with 'imageLocation' instead"  #-}

-- | The ID of the kernel.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riKernelId :: Lens.Lens' RegisterImage (Core.Maybe Types.KernelId)
riKernelId = Lens.field @"kernelId"
{-# INLINEABLE riKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The ID of the RAM disk.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRamdiskId :: Lens.Lens' RegisterImage (Core.Maybe Types.RamdiskId)
riRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE riRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riRootDeviceName :: Lens.Lens' RegisterImage (Core.Maybe Core.Text)
riRootDeviceName = Lens.field @"rootDeviceName"
{-# INLINEABLE riRootDeviceName #-}
{-# DEPRECATED rootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead"  #-}

-- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the AMI and any instances that you launch from the AMI.
--
-- There is no way to disable @sriovNetSupport@ at this time.
-- This option is supported only for HVM AMIs. Specifying this option with a PV AMI can make instances launched from the AMI unreachable.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riSriovNetSupport :: Lens.Lens' RegisterImage (Core.Maybe Core.Text)
riSriovNetSupport = Lens.field @"sriovNetSupport"
{-# INLINEABLE riSriovNetSupport #-}
{-# DEPRECATED sriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead"  #-}

-- | The type of virtualization (@hvm@ | @paravirtual@ ).
--
-- Default: @paravirtual@ 
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
riVirtualizationType :: Lens.Lens' RegisterImage (Core.Maybe Core.Text)
riVirtualizationType = Lens.field @"virtualizationType"
{-# INLINEABLE riVirtualizationType #-}
{-# DEPRECATED virtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead"  #-}

instance Core.ToQuery RegisterImage where
        toQuery RegisterImage{..}
          = Core.toQueryPair "Action" ("RegisterImage" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Name" name
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Architecture")
                architecture
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "BillingProduct")
                billingProducts
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "BlockDeviceMapping")
                blockDeviceMappings
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnaSupport") enaSupport
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "ImageLocation")
                imageLocation
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "KernelId") kernelId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RamdiskId") ramdiskId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "RootDeviceName")
                rootDeviceName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SriovNetSupport")
                sriovNetSupport
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "VirtualizationType")
                virtualizationType

instance Core.ToHeaders RegisterImage where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest RegisterImage where
        type Rs RegisterImage = RegisterImageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 RegisterImageResponse' Core.<$>
                   (x Core..@? "imageId") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Contains the output of RegisterImage.
--
-- /See:/ 'mkRegisterImageResponse' smart constructor.
data RegisterImageResponse = RegisterImageResponse'
  { imageId :: Core.Maybe Core.Text
    -- ^ The ID of the newly registered AMI.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RegisterImageResponse' value with any optional fields omitted.
mkRegisterImageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RegisterImageResponse
mkRegisterImageResponse responseStatus
  = RegisterImageResponse'{imageId = Core.Nothing, responseStatus}

-- | The ID of the newly registered AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsImageId :: Lens.Lens' RegisterImageResponse (Core.Maybe Core.Text)
rirrsImageId = Lens.field @"imageId"
{-# INLINEABLE rirrsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirrsResponseStatus :: Lens.Lens' RegisterImageResponse Core.Int
rirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
