{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImageAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified AMI. You can specify only one attribute at a time.
module Network.AWS.EC2.DescribeImageAttribute
  ( -- * Creating a request
    DescribeImageAttribute (..),
    mkDescribeImageAttribute,

    -- ** Request lenses
    diafAttribute,
    diafImageId,
    diafDryRun,

    -- * Destructuring the response
    DescribeImageAttributeResponse (..),
    mkDescribeImageAttributeResponse,

    -- ** Response lenses
    diarfrsBlockDeviceMappings,
    diarfrsDescription,
    diarfrsImageId,
    diarfrsKernelId,
    diarfrsLaunchPermissions,
    diarfrsProductCodes,
    diarfrsRamdiskId,
    diarfrsSriovNetSupport,
    diarfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeImageAttribute.
--
-- /See:/ 'mkDescribeImageAttribute' smart constructor.
data DescribeImageAttribute = DescribeImageAttribute'
  { -- | The AMI attribute.
    --
    -- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
    attribute :: Types.ImageAttributeName,
    -- | The ID of the AMI.
    imageId :: Types.ImageId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImageAttribute' value with any optional fields omitted.
mkDescribeImageAttribute ::
  -- | 'attribute'
  Types.ImageAttributeName ->
  -- | 'imageId'
  Types.ImageId ->
  DescribeImageAttribute
mkDescribeImageAttribute attribute imageId =
  DescribeImageAttribute'
    { attribute,
      imageId,
      dryRun = Core.Nothing
    }

-- | The AMI attribute.
--
-- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafAttribute :: Lens.Lens' DescribeImageAttribute Types.ImageAttributeName
diafAttribute = Lens.field @"attribute"
{-# DEPRECATED diafAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafImageId :: Lens.Lens' DescribeImageAttribute Types.ImageId
diafImageId = Lens.field @"imageId"
{-# DEPRECATED diafImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafDryRun :: Lens.Lens' DescribeImageAttribute (Core.Maybe Core.Bool)
diafDryRun = Lens.field @"dryRun"
{-# DEPRECATED diafDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeImageAttribute where
  type Rs DescribeImageAttribute = DescribeImageAttributeResponse
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
            ( Core.pure ("Action", "DescribeImageAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Attribute" attribute)
                Core.<> (Core.toQueryValue "ImageId" imageId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImageAttributeResponse'
            Core.<$> ( x Core..@? "blockDeviceMapping"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "description")
            Core.<*> (x Core..@? "imageId")
            Core.<*> (x Core..@? "kernel")
            Core.<*> (x Core..@? "launchPermission" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "ramdisk")
            Core.<*> (x Core..@? "sriovNetSupport")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Describes an image attribute.
--
-- /See:/ 'mkDescribeImageAttributeResponse' smart constructor.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
  { -- | The block device mapping entries.
    blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping],
    -- | A description for the AMI.
    description :: Core.Maybe Types.AttributeValue,
    -- | The ID of the AMI.
    imageId :: Core.Maybe Types.String,
    -- | The kernel ID.
    kernelId :: Core.Maybe Types.AttributeValue,
    -- | The launch permissions.
    launchPermissions :: Core.Maybe [Types.LaunchPermission],
    -- | The product codes.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | The RAM disk ID.
    ramdiskId :: Core.Maybe Types.AttributeValue,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
    sriovNetSupport :: Core.Maybe Types.AttributeValue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImageAttributeResponse' value with any optional fields omitted.
mkDescribeImageAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImageAttributeResponse
mkDescribeImageAttributeResponse responseStatus =
  DescribeImageAttributeResponse'
    { blockDeviceMappings =
        Core.Nothing,
      description = Core.Nothing,
      imageId = Core.Nothing,
      kernelId = Core.Nothing,
      launchPermissions = Core.Nothing,
      productCodes = Core.Nothing,
      ramdiskId = Core.Nothing,
      sriovNetSupport = Core.Nothing,
      responseStatus
    }

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsBlockDeviceMappings :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe [Types.BlockDeviceMapping])
diarfrsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED diarfrsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | A description for the AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsDescription :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsDescription = Lens.field @"description"
{-# DEPRECATED diarfrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsImageId :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.String)
diarfrsImageId = Lens.field @"imageId"
{-# DEPRECATED diarfrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The kernel ID.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsKernelId :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsKernelId = Lens.field @"kernelId"
{-# DEPRECATED diarfrsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The launch permissions.
--
-- /Note:/ Consider using 'launchPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsLaunchPermissions :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe [Types.LaunchPermission])
diarfrsLaunchPermissions = Lens.field @"launchPermissions"
{-# DEPRECATED diarfrsLaunchPermissions "Use generic-lens or generic-optics with 'launchPermissions' instead." #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsProductCodes :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe [Types.ProductCode])
diarfrsProductCodes = Lens.field @"productCodes"
{-# DEPRECATED diarfrsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The RAM disk ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsRamdiskId :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED diarfrsRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsSriovNetSupport :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsSriovNetSupport = Lens.field @"sriovNetSupport"
{-# DEPRECATED diarfrsSriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsResponseStatus :: Lens.Lens' DescribeImageAttributeResponse Core.Int
diarfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diarfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
