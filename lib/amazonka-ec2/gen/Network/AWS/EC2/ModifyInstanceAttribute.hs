{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attribute of the specified instance. You can specify only one attribute at a time.
--
-- __Note: __ Using this action to change the security groups associated with an elastic network interface (ENI) attached to an instance in a VPC can result in an error if the instance has more than one ENI. To change the security groups associated with an ENI attached to an instance that has multiple ENIs, we recommend that you use the 'ModifyNetworkInterfaceAttribute' action.
-- To modify some attributes, the instance must be stopped. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Using_ChangingAttributesWhileInstanceStopped.html Modifying attributes of a stopped instance> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.ModifyInstanceAttribute
    (
    -- * Creating a request
      ModifyInstanceAttribute (..)
    , mkModifyInstanceAttribute
    -- ** Request lenses
    , mInstanceId
    , mAttribute
    , mBlockDeviceMappings
    , mDisableApiTermination
    , mDryRun
    , mEbsOptimized
    , mEnaSupport
    , mGroups
    , mInstanceInitiatedShutdownBehavior
    , mInstanceType
    , mKernel
    , mRamdisk
    , mSourceDestCheck
    , mSriovNetSupport
    , mUserData
    , mValue

    -- * Destructuring the response
    , ModifyInstanceAttributeResponse (..)
    , mkModifyInstanceAttributeResponse
    ) where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkModifyInstanceAttribute' smart constructor.
data ModifyInstanceAttribute = ModifyInstanceAttribute'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , attribute :: Core.Maybe Types.InstanceAttributeName
    -- ^ The name of the attribute.
  , blockDeviceMappings :: Core.Maybe [Types.InstanceBlockDeviceMappingSpecification]
    -- ^ Modifies the @DeleteOnTermination@ attribute for volumes that are currently attached. The volume must be owned by the caller. If no value is specified for @DeleteOnTermination@ , the default is @true@ and the volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must add them when you launch the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance> in the /Amazon Elastic Compute Cloud User Guide/ .
  , disableApiTermination :: Core.Maybe Types.AttributeBooleanValue
    -- ^ If the value is @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this parameter for Spot Instances.
  , dryRun :: Core.Maybe Core.Bool
    -- ^ Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
  , ebsOptimized :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Specifies whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
  , enaSupport :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
  , groups :: Core.Maybe [Core.Text]
    -- ^ [EC2-VPC] Changes the security groups of the instance. You must specify at least one security group, even if it's just the default security group for the VPC. You must specify the security group ID, not the security group name.
  , instanceInitiatedShutdownBehavior :: Core.Maybe Types.AttributeValue
    -- ^ Specifies whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
  , instanceType :: Core.Maybe Types.AttributeValue
    -- ^ Changes the instance type to the specified value. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> . If the instance type is not valid, the error returned is @InvalidInstanceAttributeValue@ .
  , kernel :: Core.Maybe Types.AttributeValue
    -- ^ Changes the instance's kernel to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
  , ramdisk :: Core.Maybe Types.AttributeValue
    -- ^ Changes the instance's RAM disk to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
  , sourceDestCheck :: Core.Maybe Types.AttributeBooleanValue
    -- ^ Specifies whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
  , sriovNetSupport :: Core.Maybe Types.AttributeValue
    -- ^ Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the instance.
--
-- There is no way to disable enhanced networking with the Intel 82599 Virtual Function interface at this time.
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
  , userData :: Core.Maybe Types.BlobAttributeValue
    -- ^ Changes the instance's user data to the specified value. If you are using an AWS SDK or command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
  , value :: Core.Maybe Core.Text
    -- ^ A new value for the attribute. Use only with the @kernel@ , @ramdisk@ , @userData@ , @disableApiTermination@ , or @instanceInitiatedShutdownBehavior@ attribute.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceAttribute' value with any optional fields omitted.
mkModifyInstanceAttribute
    :: Types.InstanceId -- ^ 'instanceId'
    -> ModifyInstanceAttribute
mkModifyInstanceAttribute instanceId
  = ModifyInstanceAttribute'{instanceId, attribute = Core.Nothing,
                             blockDeviceMappings = Core.Nothing,
                             disableApiTermination = Core.Nothing, dryRun = Core.Nothing,
                             ebsOptimized = Core.Nothing, enaSupport = Core.Nothing,
                             groups = Core.Nothing,
                             instanceInitiatedShutdownBehavior = Core.Nothing,
                             instanceType = Core.Nothing, kernel = Core.Nothing,
                             ramdisk = Core.Nothing, sourceDestCheck = Core.Nothing,
                             sriovNetSupport = Core.Nothing, userData = Core.Nothing,
                             value = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInstanceId :: Lens.Lens' ModifyInstanceAttribute Types.InstanceId
mInstanceId = Lens.field @"instanceId"
{-# INLINEABLE mInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAttribute :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.InstanceAttributeName)
mAttribute = Lens.field @"attribute"
{-# INLINEABLE mAttribute #-}
{-# DEPRECATED attribute "Use generic-lens or generic-optics with 'attribute' instead"  #-}

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are currently attached. The volume must be owned by the caller. If no value is specified for @DeleteOnTermination@ , the default is @true@ and the volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must add them when you launch the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBlockDeviceMappings :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe [Types.InstanceBlockDeviceMappingSpecification])
mBlockDeviceMappings = Lens.field @"blockDeviceMappings"
{-# INLINEABLE mBlockDeviceMappings #-}
{-# DEPRECATED blockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead"  #-}

-- | If the value is @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this parameter for Spot Instances.
--
-- /Note:/ Consider using 'disableApiTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDisableApiTermination :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeBooleanValue)
mDisableApiTermination = Lens.field @"disableApiTermination"
{-# INLINEABLE mDisableApiTermination #-}
{-# DEPRECATED disableApiTermination "Use generic-lens or generic-optics with 'disableApiTermination' instead"  #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDryRun :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Core.Bool)
mDryRun = Lens.field @"dryRun"
{-# INLINEABLE mDryRun #-}
{-# DEPRECATED dryRun "Use generic-lens or generic-optics with 'dryRun' instead"  #-}

-- | Specifies whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEbsOptimized :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeBooleanValue)
mEbsOptimized = Lens.field @"ebsOptimized"
{-# INLINEABLE mEbsOptimized #-}
{-# DEPRECATED ebsOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead"  #-}

-- | Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEnaSupport :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeBooleanValue)
mEnaSupport = Lens.field @"enaSupport"
{-# INLINEABLE mEnaSupport #-}
{-# DEPRECATED enaSupport "Use generic-lens or generic-optics with 'enaSupport' instead"  #-}

-- | [EC2-VPC] Changes the security groups of the instance. You must specify at least one security group, even if it's just the default security group for the VPC. You must specify the security group ID, not the security group name.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mGroups :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe [Core.Text])
mGroups = Lens.field @"groups"
{-# INLINEABLE mGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | Specifies whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInstanceInitiatedShutdownBehavior :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeValue)
mInstanceInitiatedShutdownBehavior = Lens.field @"instanceInitiatedShutdownBehavior"
{-# INLINEABLE mInstanceInitiatedShutdownBehavior #-}
{-# DEPRECATED instanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead"  #-}

-- | Changes the instance type to the specified value. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> . If the instance type is not valid, the error returned is @InvalidInstanceAttributeValue@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInstanceType :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeValue)
mInstanceType = Lens.field @"instanceType"
{-# INLINEABLE mInstanceType #-}
{-# DEPRECATED instanceType "Use generic-lens or generic-optics with 'instanceType' instead"  #-}

-- | Changes the instance's kernel to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
--
-- /Note:/ Consider using 'kernel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mKernel :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeValue)
mKernel = Lens.field @"kernel"
{-# INLINEABLE mKernel #-}
{-# DEPRECATED kernel "Use generic-lens or generic-optics with 'kernel' instead"  #-}

-- | Changes the instance's RAM disk to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
--
-- /Note:/ Consider using 'ramdisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRamdisk :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeValue)
mRamdisk = Lens.field @"ramdisk"
{-# INLINEABLE mRamdisk #-}
{-# DEPRECATED ramdisk "Use generic-lens or generic-optics with 'ramdisk' instead"  #-}

-- | Specifies whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSourceDestCheck :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeBooleanValue)
mSourceDestCheck = Lens.field @"sourceDestCheck"
{-# INLINEABLE mSourceDestCheck #-}
{-# DEPRECATED sourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead"  #-}

-- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the instance.
--
-- There is no way to disable enhanced networking with the Intel 82599 Virtual Function interface at this time.
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSriovNetSupport :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.AttributeValue)
mSriovNetSupport = Lens.field @"sriovNetSupport"
{-# INLINEABLE mSriovNetSupport #-}
{-# DEPRECATED sriovNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead"  #-}

-- | Changes the instance's user data to the specified value. If you are using an AWS SDK or command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mUserData :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Types.BlobAttributeValue)
mUserData = Lens.field @"userData"
{-# INLINEABLE mUserData #-}
{-# DEPRECATED userData "Use generic-lens or generic-optics with 'userData' instead"  #-}

-- | A new value for the attribute. Use only with the @kernel@ , @ramdisk@ , @userData@ , @disableApiTermination@ , or @instanceInitiatedShutdownBehavior@ attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mValue :: Lens.Lens' ModifyInstanceAttribute (Core.Maybe Core.Text)
mValue = Lens.field @"value"
{-# INLINEABLE mValue #-}
{-# DEPRECATED value "Use generic-lens or generic-optics with 'value' instead"  #-}

instance Core.ToQuery ModifyInstanceAttribute where
        toQuery ModifyInstanceAttribute{..}
          = Core.toQueryPair "Action"
              ("ModifyInstanceAttribute" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2016-11-15" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Attribute") attribute
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "BlockDeviceMapping")
                blockDeviceMappings
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DisableApiTermination")
                disableApiTermination
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DryRun") dryRun
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EbsOptimized")
                ebsOptimized
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "EnaSupport") enaSupport
              Core.<> Core.maybe Core.mempty (Core.toQueryList "GroupId") groups
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "InstanceInitiatedShutdownBehavior")
                instanceInitiatedShutdownBehavior
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InstanceType")
                instanceType
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Kernel") kernel
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Ramdisk") ramdisk
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SourceDestCheck")
                sourceDestCheck
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SriovNetSupport")
                sriovNetSupport
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "UserData") userData
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Value") value

instance Core.ToHeaders ModifyInstanceAttribute where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ModifyInstanceAttribute where
        type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse
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
          = Response.receiveNull ModifyInstanceAttributeResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkModifyInstanceAttributeResponse' smart constructor.
data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ModifyInstanceAttributeResponse' value with any optional fields omitted.
mkModifyInstanceAttributeResponse
    :: ModifyInstanceAttributeResponse
mkModifyInstanceAttributeResponse
  = ModifyInstanceAttributeResponse'
