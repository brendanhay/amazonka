{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeImageAttribute (..)
    , mkDescribeImageAttribute
    -- ** Request lenses
    , diafAttribute
    , diafImageId
    , diafDryRun

    -- * Destructuring the response
    , DescribeImageAttributeResponse (..)
    , mkDescribeImageAttributeResponse
    -- ** Response lenses
    , diarfrsBlockDeviceMappings
    , diarfrsDescription
    , diarfrsImageId
    , diarfrsKernelId
    , diarfrsLaunchPermissions
    , diarfrsProductCodes
    , diarfrsRamdiskId
    , diarfrsSriovNetSupport
    , diarfrsResponseStatus
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeImageAttribute.
--
-- /See:/ 'mkDescribeImageAttribute' smart constructor.
data DescribeImageAttribute = DescribeImageAttribute'
  { attribute :: Types.ImageAttributeName
    -- ^ The AMI attribute.
--
-- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
  , imageId :: Types.ImageId
    -- ^ The ID of the AMI.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImageAttribute' value with any optional fields omitted.
mkDescribeImageAttribute
    :: Types.ImageAttributeName -- ^ 'attribute'
    -> Types.ImageId -- ^ 'imageId'
    -> DescribeImageAttribute
mkDescribeImageAttribute attribute imageId
  = DescribeImageAttribute'{attribute, imageId,
                            dryRun = Core.Nothing}

-- | The AMI attribute.
--
-- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafAttribute :: Lens.Lens' DescribeImageAttribute Types.ImageAttributeName
diafAttribute = Lens.field @"attribute"
{-# INLINEABLE diafAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafImageId :: Lens.Lens' DescribeImageAttribute Types.ImageId
diafImageId = Lens.field @"imageId"
{-# INLINEABLE diafImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafDryRun :: Lens.Lens' DescribeImageAttribute (Core.Maybe Core.Bool)
diafDryRun = Lens.field @"dryRun"
{-# INLINEABLE diafDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

instance Core.ToQuery DescribeImageAttribute where
        toQuery DescribeImageAttribute{..}
          = Core.toQueryPair "Action" ("DescribeImageAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "Attribute" attribute
              Core.<> Core.toQueryPair "ImageId" imageId
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun

instance Core.ToHeaders DescribeImageAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeImageAttribute where
        type Rs DescribeImageAttribute = DescribeImageAttributeResponse
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
                 DescribeImageAttributeResponse' Core.<$>
                   (x Core..@? "blockDeviceMapping" Core..<@>
                      Core.parseXMLList "item")
                     Core.<*> x Core..@? "description"
                     Core.<*> x Core..@? "imageId"
                     Core.<*> x Core..@? "kernel"
                     Core.<*>
                     x Core..@? "launchPermission" Core..<@> Core.parseXMLList "item"
                     Core.<*>
                     x Core..@? "productCodes" Core..<@> Core.parseXMLList "item"
                     Core.<*> x Core..@? "ramdisk"
                     Core.<*> x Core..@? "sriovNetSupport"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Describes an image attribute.
--
-- /See:/ 'mkDescribeImageAttributeResponse' smart constructor.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
  { blockDeviceMappings :: Core.Maybe [Types.BlockDeviceMapping]
    -- ^ The block device mapping entries.
  , description :: Core.Maybe Types.AttributeValue
    -- ^ A description for the AMI.
  , imageId :: Core.Maybe Core.Text
    -- ^ The ID of the AMI.
  , kernelId :: Core.Maybe Types.AttributeValue
    -- ^ The kernel ID.
  , launchPermissions :: Core.Maybe [Types.LaunchPermission]
    -- ^ The launch permissions.
  , productCodes :: Core.Maybe [Types.ProductCode]
    -- ^ The product codes.
  , ramdiskId :: Core.Maybe Types.AttributeValue
    -- ^ The RAM disk ID.
  , sriovNetSupport :: Core.Maybe Types.AttributeValue
    -- ^ Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImageAttributeResponse' value with any optional fields omitted.
mkDescribeImageAttributeResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeImageAttributeResponse
mkDescribeImageAttributeResponse responseStatus
  = DescribeImageAttributeResponse'{blockDeviceMappings =
                                      Core.Nothing,
                                    description = Core.Nothing, imageId = Core.Nothing,
                                    kernelId = Core.Nothing, launchPermissions = Core.Nothing,
                                    productCodes = Core.Nothing, ramdiskId = Core.Nothing,
                                    sriovNetSupport = Core.Nothing, responseStatus}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsBlockDeviceMappings :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe [Types.BlockDeviceMapping])
diarfrsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE diarfrsBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | A description for the AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsDescription :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsDescription = Lens.field @"description"
{-# INLINEABLE diarfrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsImageId :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Core.Text)
diarfrsImageId = Lens.field @"imageId"
{-# INLINEABLE diarfrsImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The kernel ID.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsKernelId :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsKernelId = Lens.field @"kernelId"
{-# INLINEABLE diarfrsKernelId #-}
{-# DEPRECATED kernelId "Use generic-lens or generic-optics with 'kernelId' instead"  #-}

-- | The launch permissions.
--
-- /Note:/ Consider using 'launchPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsLaunchPermissions :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe [Types.LaunchPermission])
diarfrsLaunchPermissions = Lens.field @"launchPermissions"
{-# INLINEABLE diarfrsLaunchPermissions #-}
{-# DEPRECATED launchPermissions "Use generic-lens or generic-optics with 'launchPermissions' instead"  #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsProductCodes :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe [Types.ProductCode])
diarfrsProductCodes = Lens.field @"productCodes"
{-# INLINEABLE diarfrsProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | The RAM disk ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsRamdiskId :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsRamdiskId = Lens.field @"ramdiskId"
{-# INLINEABLE diarfrsRamdiskId #-}
{-# DEPRECATED ramdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead"  #-}

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsSriovNetSupport :: Lens.Lens' DescribeImageAttributeResponse (Core.Maybe Types.AttributeValue)
diarfrsSriovNetSupport = Lens.field @"sriovNetSupport"
{-# INLINEABLE diarfrsSriovNetSupport #-}
{-# DEPRECATED sriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarfrsResponseStatus :: Lens.Lens' DescribeImageAttributeResponse Core.Int
diarfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE diarfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
