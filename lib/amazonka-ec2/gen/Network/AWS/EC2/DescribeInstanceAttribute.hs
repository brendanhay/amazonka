{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified instance. You can specify only one attribute at a time. Valid attribute values are: @instanceType@ | @kernel@ | @ramdisk@ | @userData@ | @disableApiTermination@ | @instanceInitiatedShutdownBehavior@ | @rootDeviceName@ | @blockDeviceMapping@ | @productCodes@ | @sourceDestCheck@ | @groupSet@ | @ebsOptimized@ | @sriovNetSupport@
module Network.AWS.EC2.DescribeInstanceAttribute
  ( -- * Creating a request
    DescribeInstanceAttribute (..),
    mkDescribeInstanceAttribute,

    -- ** Request lenses
    diaAttribute,
    diaInstanceId,
    diaDryRun,

    -- * Destructuring the response
    DescribeInstanceAttributeResponse (..),
    mkDescribeInstanceAttributeResponse,

    -- ** Response lenses
    diarrsBlockDeviceMappings,
    diarrsDisableApiTermination,
    diarrsEbsOptimized,
    diarrsEnaSupport,
    diarrsEnclaveOptions,
    diarrsGroups,
    diarrsInstanceId,
    diarrsInstanceInitiatedShutdownBehavior,
    diarrsInstanceType,
    diarrsKernelId,
    diarrsProductCodes,
    diarrsRamdiskId,
    diarrsRootDeviceName,
    diarrsSourceDestCheck,
    diarrsSriovNetSupport,
    diarrsUserData,
    diarrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | The instance attribute.
    --
    -- Note: The @enaSupport@ attribute is not supported at this time.
    attribute :: Types.InstanceAttributeName,
    -- | The ID of the instance.
    instanceId :: Types.InstanceId,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstanceAttribute' value with any optional fields omitted.
mkDescribeInstanceAttribute ::
  -- | 'attribute'
  Types.InstanceAttributeName ->
  -- | 'instanceId'
  Types.InstanceId ->
  DescribeInstanceAttribute
mkDescribeInstanceAttribute attribute instanceId =
  DescribeInstanceAttribute'
    { attribute,
      instanceId,
      dryRun = Core.Nothing
    }

-- | The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaAttribute :: Lens.Lens' DescribeInstanceAttribute Types.InstanceAttributeName
diaAttribute = Lens.field @"attribute"
{-# DEPRECATED diaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaInstanceId :: Lens.Lens' DescribeInstanceAttribute Types.InstanceId
diaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED diaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaDryRun :: Lens.Lens' DescribeInstanceAttribute (Core.Maybe Core.Bool)
diaDryRun = Lens.field @"dryRun"
{-# DEPRECATED diaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Core.AWSRequest DescribeInstanceAttribute where
  type
    Rs DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
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
            ( Core.pure ("Action", "DescribeInstanceAttribute")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "Attribute" attribute)
                Core.<> (Core.toQueryValue "InstanceId" instanceId)
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            Core.<$> ( x Core..@? "blockDeviceMapping"
                         Core..<@> Core.parseXMLList "item"
                     )
            Core.<*> (x Core..@? "disableApiTermination")
            Core.<*> (x Core..@? "ebsOptimized")
            Core.<*> (x Core..@? "enaSupport")
            Core.<*> (x Core..@? "enclaveOptions")
            Core.<*> (x Core..@? "groupSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "instanceId")
            Core.<*> (x Core..@? "instanceInitiatedShutdownBehavior")
            Core.<*> (x Core..@? "instanceType")
            Core.<*> (x Core..@? "kernel")
            Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
            Core.<*> (x Core..@? "ramdisk")
            Core.<*> (x Core..@? "rootDeviceName")
            Core.<*> (x Core..@? "sourceDestCheck")
            Core.<*> (x Core..@? "sriovNetSupport")
            Core.<*> (x Core..@? "userData")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Describes an instance attribute.
--
-- /See:/ 'mkDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The block device mapping of the instance.
    blockDeviceMappings :: Core.Maybe [Types.InstanceBlockDeviceMapping],
    -- | If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
    disableApiTermination :: Core.Maybe Types.AttributeBooleanValue,
    -- | Indicates whether the instance is optimized for Amazon EBS I/O.
    ebsOptimized :: Core.Maybe Types.AttributeBooleanValue,
    -- | Indicates whether enhanced networking with ENA is enabled.
    enaSupport :: Core.Maybe Types.AttributeBooleanValue,
    -- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ ; otherwise, set it to @false@ .
    enclaveOptions :: Core.Maybe Types.EnclaveOptions,
    -- | The security groups associated with the instance.
    groups :: Core.Maybe [Types.GroupIdentifier],
    -- | The ID of the instance.
    instanceId :: Core.Maybe Types.String,
    -- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
    instanceInitiatedShutdownBehavior :: Core.Maybe Types.AttributeValue,
    -- | The instance type.
    instanceType :: Core.Maybe Types.AttributeValue,
    -- | The kernel ID.
    kernelId :: Core.Maybe Types.AttributeValue,
    -- | A list of product codes.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | The RAM disk ID.
    ramdiskId :: Core.Maybe Types.AttributeValue,
    -- | The device name of the root device volume (for example, @/dev/sda1@ ).
    rootDeviceName :: Core.Maybe Types.AttributeValue,
    -- | Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
    sourceDestCheck :: Core.Maybe Types.AttributeBooleanValue,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
    sriovNetSupport :: Core.Maybe Types.AttributeValue,
    -- | The user data.
    userData :: Core.Maybe Types.AttributeValue,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeInstanceAttributeResponse' value with any optional fields omitted.
mkDescribeInstanceAttributeResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeInstanceAttributeResponse
mkDescribeInstanceAttributeResponse responseStatus =
  DescribeInstanceAttributeResponse'
    { blockDeviceMappings =
        Core.Nothing,
      disableApiTermination = Core.Nothing,
      ebsOptimized = Core.Nothing,
      enaSupport = Core.Nothing,
      enclaveOptions = Core.Nothing,
      groups = Core.Nothing,
      instanceId = Core.Nothing,
      instanceInitiatedShutdownBehavior = Core.Nothing,
      instanceType = Core.Nothing,
      kernelId = Core.Nothing,
      productCodes = Core.Nothing,
      ramdiskId = Core.Nothing,
      rootDeviceName = Core.Nothing,
      sourceDestCheck = Core.Nothing,
      sriovNetSupport = Core.Nothing,
      userData = Core.Nothing,
      responseStatus
    }

-- | The block device mapping of the instance.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsBlockDeviceMappings :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe [Types.InstanceBlockDeviceMapping])
diarrsBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# DEPRECATED diarrsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
--
-- /Note:/ Consider using 'disableApiTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsDisableApiTermination :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
diarrsDisableApiTermination = Lens.field @"disableApiTermination"
{-# DEPRECATED diarrsDisableApiTermination "Use generic-lens or generic-optics with 'disableApiTermination' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsEbsOptimized :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
diarrsEbsOptimized = Lens.field @"ebsOptimized"
{-# DEPRECATED diarrsEbsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | Indicates whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsEnaSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
diarrsEnaSupport = Lens.field @"enaSupport"
{-# DEPRECATED diarrsEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ ; otherwise, set it to @false@ .
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsEnclaveOptions :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.EnclaveOptions)
diarrsEnclaveOptions = Lens.field @"enclaveOptions"
{-# DEPRECATED diarrsEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The security groups associated with the instance.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsGroups :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe [Types.GroupIdentifier])
diarrsGroups = Lens.field @"groups"
{-# DEPRECATED diarrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsInstanceId :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.String)
diarrsInstanceId = Lens.field @"instanceId"
{-# DEPRECATED diarrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsInstanceInitiatedShutdownBehavior :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeValue)
diarrsInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# DEPRECATED diarrsInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsInstanceType :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeValue)
diarrsInstanceType = Lens.field @"instanceType"
{-# DEPRECATED diarrsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | The kernel ID.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsKernelId :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeValue)
diarrsKernelId = Lens.field @"kernelId"
{-# DEPRECATED diarrsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | A list of product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsProductCodes :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe [Types.ProductCode])
diarrsProductCodes = Lens.field @"productCodes"
{-# DEPRECATED diarrsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The RAM disk ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsRamdiskId :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeValue)
diarrsRamdiskId = Lens.field @"ramdiskId"
{-# DEPRECATED diarrsRamdiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsRootDeviceName :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeValue)
diarrsRootDeviceName = Lens.field @"rootDeviceName"
{-# DEPRECATED diarrsRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsSourceDestCheck :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeBooleanValue)
diarrsSourceDestCheck = Lens.field @"sourceDestCheck"
{-# DEPRECATED diarrsSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsSriovNetSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeValue)
diarrsSriovNetSupport = Lens.field @"sriovNetSupport"
{-# DEPRECATED diarrsSriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The user data.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsUserData :: Lens.Lens' DescribeInstanceAttributeResponse (Core.Maybe Types.AttributeValue)
diarrsUserData = Lens.field @"userData"
{-# DEPRECATED diarrsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarrsResponseStatus :: Lens.Lens' DescribeInstanceAttributeResponse Core.Int
diarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED diarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
