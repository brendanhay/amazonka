{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ModifyInstanceAttribute (..),
    mkModifyInstanceAttribute,

    -- ** Request lenses
    mGroups,
    mAttribute,
    mEnaSupport,
    mSourceDestCheck,
    mDisableAPITermination,
    mKernel,
    mRAMDisk,
    mValue,
    mInstanceType,
    mSRIOVNetSupport,
    mEBSOptimized,
    mUserData,
    mInstanceInitiatedShutdownBehavior,
    mBlockDeviceMappings,
    mDryRun,
    mInstanceId,

    -- * Destructuring the response
    ModifyInstanceAttributeResponse (..),
    mkModifyInstanceAttributeResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyInstanceAttribute' smart constructor.
data ModifyInstanceAttribute = ModifyInstanceAttribute'
  { groups ::
      Lude.Maybe [Lude.Text],
    attribute ::
      Lude.Maybe InstanceAttributeName,
    enaSupport ::
      Lude.Maybe AttributeBooleanValue,
    sourceDestCheck ::
      Lude.Maybe AttributeBooleanValue,
    disableAPITermination ::
      Lude.Maybe AttributeBooleanValue,
    kernel :: Lude.Maybe AttributeValue,
    ramdisk :: Lude.Maybe AttributeValue,
    value :: Lude.Maybe Lude.Text,
    instanceType :: Lude.Maybe AttributeValue,
    sriovNetSupport ::
      Lude.Maybe AttributeValue,
    ebsOptimized ::
      Lude.Maybe AttributeBooleanValue,
    userData :: Lude.Maybe BlobAttributeValue,
    instanceInitiatedShutdownBehavior ::
      Lude.Maybe AttributeValue,
    blockDeviceMappings ::
      Lude.Maybe
        [InstanceBlockDeviceMappingSpecification],
    dryRun :: Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The name of the attribute.
-- * 'blockDeviceMappings' - Modifies the @DeleteOnTermination@ attribute for volumes that are currently attached. The volume must be owned by the caller. If no value is specified for @DeleteOnTermination@ , the default is @true@ and the volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must add them when you launch the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance> in the /Amazon Elastic Compute Cloud User Guide/ .
-- * 'disableAPITermination' - If the value is @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this parameter for Spot Instances.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'ebsOptimized' - Specifies whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
-- * 'enaSupport' - Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
-- * 'groups' - [EC2-VPC] Changes the security groups of the instance. You must specify at least one security group, even if it's just the default security group for the VPC. You must specify the security group ID, not the security group name.
-- * 'instanceId' - The ID of the instance.
-- * 'instanceInitiatedShutdownBehavior' - Specifies whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
-- * 'instanceType' - Changes the instance type to the specified value. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> . If the instance type is not valid, the error returned is @InvalidInstanceAttributeValue@ .
-- * 'kernel' - Changes the instance's kernel to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
-- * 'ramdisk' - Changes the instance's RAM disk to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
-- * 'sourceDestCheck' - Specifies whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
-- * 'sriovNetSupport' - Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the instance.
--
-- There is no way to disable enhanced networking with the Intel 82599 Virtual Function interface at this time.
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
-- * 'userData' - Changes the instance's user data to the specified value. If you are using an AWS SDK or command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
-- * 'value' - A new value for the attribute. Use only with the @kernel@ , @ramdisk@ , @userData@ , @disableApiTermination@ , or @instanceInitiatedShutdownBehavior@ attribute.
mkModifyInstanceAttribute ::
  -- | 'instanceId'
  Lude.Text ->
  ModifyInstanceAttribute
mkModifyInstanceAttribute pInstanceId_ =
  ModifyInstanceAttribute'
    { groups = Lude.Nothing,
      attribute = Lude.Nothing,
      enaSupport = Lude.Nothing,
      sourceDestCheck = Lude.Nothing,
      disableAPITermination = Lude.Nothing,
      kernel = Lude.Nothing,
      ramdisk = Lude.Nothing,
      value = Lude.Nothing,
      instanceType = Lude.Nothing,
      sriovNetSupport = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      instanceInitiatedShutdownBehavior = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      dryRun = Lude.Nothing,
      instanceId = pInstanceId_
    }

-- | [EC2-VPC] Changes the security groups of the instance. You must specify at least one security group, even if it's just the default security group for the VPC. You must specify the security group ID, not the security group name.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mGroups :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe [Lude.Text])
mGroups = Lens.lens (groups :: ModifyInstanceAttribute -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAttribute :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe InstanceAttributeName)
mAttribute = Lens.lens (attribute :: ModifyInstanceAttribute -> Lude.Maybe InstanceAttributeName) (\s a -> s {attribute = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | Set to @true@ to enable enhanced networking with ENA for the instance.
--
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEnaSupport :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeBooleanValue)
mEnaSupport = Lens.lens (enaSupport :: ModifyInstanceAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {enaSupport = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | Specifies whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSourceDestCheck :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeBooleanValue)
mSourceDestCheck = Lens.lens (sourceDestCheck :: ModifyInstanceAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {sourceDestCheck = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | If the value is @true@ , you can't terminate the instance using the Amazon EC2 console, CLI, or API; otherwise, you can. You cannot use this parameter for Spot Instances.
--
-- /Note:/ Consider using 'disableAPITermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDisableAPITermination :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeBooleanValue)
mDisableAPITermination = Lens.lens (disableAPITermination :: ModifyInstanceAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {disableAPITermination = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mDisableAPITermination "Use generic-lens or generic-optics with 'disableAPITermination' instead." #-}

-- | Changes the instance's kernel to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
--
-- /Note:/ Consider using 'kernel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mKernel :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeValue)
mKernel = Lens.lens (kernel :: ModifyInstanceAttribute -> Lude.Maybe AttributeValue) (\s a -> s {kernel = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mKernel "Use generic-lens or generic-optics with 'kernel' instead." #-}

-- | Changes the instance's RAM disk to the specified value. We recommend that you use PV-GRUB instead of kernels and RAM disks. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UserProvidedKernels.html PV-GRUB> .
--
-- /Note:/ Consider using 'ramdisk' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRAMDisk :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeValue)
mRAMDisk = Lens.lens (ramdisk :: ModifyInstanceAttribute -> Lude.Maybe AttributeValue) (\s a -> s {ramdisk = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mRAMDisk "Use generic-lens or generic-optics with 'ramdisk' instead." #-}

-- | A new value for the attribute. Use only with the @kernel@ , @ramdisk@ , @userData@ , @disableApiTermination@ , or @instanceInitiatedShutdownBehavior@ attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mValue :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe Lude.Text)
mValue = Lens.lens (value :: ModifyInstanceAttribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | Changes the instance type to the specified value. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/instance-types.html Instance types> . If the instance type is not valid, the error returned is @InvalidInstanceAttributeValue@ .
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInstanceType :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeValue)
mInstanceType = Lens.lens (instanceType :: ModifyInstanceAttribute -> Lude.Maybe AttributeValue) (\s a -> s {instanceType = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Set to @simple@ to enable enhanced networking with the Intel 82599 Virtual Function interface for the instance.
--
-- There is no way to disable enhanced networking with the Intel 82599 Virtual Function interface at this time.
-- This option is supported only for HVM instances. Specifying this option with a PV instance can make it unreachable.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mSRIOVNetSupport :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeValue)
mSRIOVNetSupport = Lens.lens (sriovNetSupport :: ModifyInstanceAttribute -> Lude.Maybe AttributeValue) (\s a -> s {sriovNetSupport = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | Specifies whether the instance is optimized for Amazon EBS I/O. This optimization provides dedicated throughput to Amazon EBS and an optimized configuration stack to provide optimal EBS I/O performance. This optimization isn't available with all instance types. Additional usage charges apply when using an EBS Optimized instance.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEBSOptimized :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeBooleanValue)
mEBSOptimized = Lens.lens (ebsOptimized :: ModifyInstanceAttribute -> Lude.Maybe AttributeBooleanValue) (\s a -> s {ebsOptimized = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | Changes the instance's user data to the specified value. If you are using an AWS SDK or command line tool, base64-encoding is performed for you, and you can load the text from a file. Otherwise, you must provide base64-encoded text.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mUserData :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe BlobAttributeValue)
mUserData = Lens.lens (userData :: ModifyInstanceAttribute -> Lude.Maybe BlobAttributeValue) (\s a -> s {userData = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Specifies whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInstanceInitiatedShutdownBehavior :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe AttributeValue)
mInstanceInitiatedShutdownBehavior = Lens.lens (instanceInitiatedShutdownBehavior :: ModifyInstanceAttribute -> Lude.Maybe AttributeValue) (\s a -> s {instanceInitiatedShutdownBehavior = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | Modifies the @DeleteOnTermination@ attribute for volumes that are currently attached. The volume must be owned by the caller. If no value is specified for @DeleteOnTermination@ , the default is @true@ and the volume is deleted when the instance is terminated.
--
-- To add instance store volumes to an Amazon EBS-backed instance, you must add them when you launch the instance. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/block-device-mapping-concepts.html#Using_OverridingAMIBDM Updating the block device mapping when launching an instance> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mBlockDeviceMappings :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe [InstanceBlockDeviceMappingSpecification])
mBlockDeviceMappings = Lens.lens (blockDeviceMappings :: ModifyInstanceAttribute -> Lude.Maybe [InstanceBlockDeviceMappingSpecification]) (\s a -> s {blockDeviceMappings = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDryRun :: Lens.Lens' ModifyInstanceAttribute (Lude.Maybe Lude.Bool)
mDryRun = Lens.lens (dryRun :: ModifyInstanceAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInstanceId :: Lens.Lens' ModifyInstanceAttribute Lude.Text
mInstanceId = Lens.lens (instanceId :: ModifyInstanceAttribute -> Lude.Text) (\s a -> s {instanceId = a} :: ModifyInstanceAttribute)
{-# DEPRECATED mInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest ModifyInstanceAttribute where
  type Rs ModifyInstanceAttribute = ModifyInstanceAttributeResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull ModifyInstanceAttributeResponse'

instance Lude.ToHeaders ModifyInstanceAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyInstanceAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstanceAttribute where
  toQuery ModifyInstanceAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ModifyInstanceAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "GroupId" Lude.<$> groups),
        "Attribute" Lude.=: attribute,
        "EnaSupport" Lude.=: enaSupport,
        "SourceDestCheck" Lude.=: sourceDestCheck,
        "DisableApiTermination" Lude.=: disableAPITermination,
        "Kernel" Lude.=: kernel,
        "Ramdisk" Lude.=: ramdisk,
        "Value" Lude.=: value,
        "InstanceType" Lude.=: instanceType,
        "SriovNetSupport" Lude.=: sriovNetSupport,
        "EbsOptimized" Lude.=: ebsOptimized,
        "UserData" Lude.=: userData,
        "InstanceInitiatedShutdownBehavior"
          Lude.=: instanceInitiatedShutdownBehavior,
        Lude.toQuery
          ( Lude.toQueryList "BlockDeviceMapping"
              Lude.<$> blockDeviceMappings
          ),
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId
      ]

-- | /See:/ 'mkModifyInstanceAttributeResponse' smart constructor.
data ModifyInstanceAttributeResponse = ModifyInstanceAttributeResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceAttributeResponse' with the minimum fields required to make a request.
mkModifyInstanceAttributeResponse ::
  ModifyInstanceAttributeResponse
mkModifyInstanceAttributeResponse =
  ModifyInstanceAttributeResponse'
