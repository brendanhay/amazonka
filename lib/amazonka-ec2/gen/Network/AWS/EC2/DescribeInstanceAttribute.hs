{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    diaDryRun,
    diaAttribute,
    diaInstanceId,

    -- * Destructuring the response
    DescribeInstanceAttributeResponse (..),
    mkDescribeInstanceAttributeResponse,

    -- ** Response lenses
    desrsInstanceId,
    desrsGroups,
    desrsEnaSupport,
    desrsSourceDestCheck,
    desrsDisableAPITermination,
    desrsEnclaveOptions,
    desrsRAMDiskId,
    desrsKernelId,
    desrsRootDeviceName,
    desrsInstanceType,
    desrsSRIOVNetSupport,
    desrsEBSOptimized,
    desrsUserData,
    desrsInstanceInitiatedShutdownBehavior,
    desrsProductCodes,
    desrsBlockDeviceMappings,
    desrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    attribute :: InstanceAttributeName,
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

-- | Creates a value of 'DescribeInstanceAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceId' - The ID of the instance.
mkDescribeInstanceAttribute ::
  -- | 'attribute'
  InstanceAttributeName ->
  -- | 'instanceId'
  Lude.Text ->
  DescribeInstanceAttribute
mkDescribeInstanceAttribute pAttribute_ pInstanceId_ =
  DescribeInstanceAttribute'
    { dryRun = Lude.Nothing,
      attribute = pAttribute_,
      instanceId = pInstanceId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaDryRun :: Lens.Lens' DescribeInstanceAttribute (Lude.Maybe Lude.Bool)
diaDryRun = Lens.lens (dryRun :: DescribeInstanceAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaAttribute :: Lens.Lens' DescribeInstanceAttribute InstanceAttributeName
diaAttribute = Lens.lens (attribute :: DescribeInstanceAttribute -> InstanceAttributeName) (\s a -> s {attribute = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaInstanceId :: Lens.Lens' DescribeInstanceAttribute Lude.Text
diaInstanceId = Lens.lens (instanceId :: DescribeInstanceAttribute -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

instance Lude.AWSRequest DescribeInstanceAttribute where
  type
    Rs DescribeInstanceAttribute =
      DescribeInstanceAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeInstanceAttributeResponse'
            Lude.<$> (x Lude..@? "instanceId")
            Lude.<*> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "enaSupport")
            Lude.<*> (x Lude..@? "sourceDestCheck")
            Lude.<*> (x Lude..@? "disableApiTermination")
            Lude.<*> (x Lude..@? "enclaveOptions")
            Lude.<*> (x Lude..@? "ramdisk")
            Lude.<*> (x Lude..@? "kernel")
            Lude.<*> (x Lude..@? "rootDeviceName")
            Lude.<*> (x Lude..@? "instanceType")
            Lude.<*> (x Lude..@? "sriovNetSupport")
            Lude.<*> (x Lude..@? "ebsOptimized")
            Lude.<*> (x Lude..@? "userData")
            Lude.<*> (x Lude..@? "instanceInitiatedShutdownBehavior")
            Lude.<*> ( x Lude..@? "productCodes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> ( x Lude..@? "blockDeviceMapping" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeInstanceAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeInstanceAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeInstanceAttribute where
  toQuery DescribeInstanceAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeInstanceAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "Attribute" Lude.=: attribute,
        "InstanceId" Lude.=: instanceId
      ]

-- | Describes an instance attribute.
--
-- /See:/ 'mkDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { instanceId ::
      Lude.Maybe Lude.Text,
    groups ::
      Lude.Maybe
        [GroupIdentifier],
    enaSupport ::
      Lude.Maybe
        AttributeBooleanValue,
    sourceDestCheck ::
      Lude.Maybe
        AttributeBooleanValue,
    disableAPITermination ::
      Lude.Maybe
        AttributeBooleanValue,
    enclaveOptions ::
      Lude.Maybe
        EnclaveOptions,
    ramdiskId ::
      Lude.Maybe
        AttributeValue,
    kernelId ::
      Lude.Maybe
        AttributeValue,
    rootDeviceName ::
      Lude.Maybe
        AttributeValue,
    instanceType ::
      Lude.Maybe
        AttributeValue,
    sriovNetSupport ::
      Lude.Maybe
        AttributeValue,
    ebsOptimized ::
      Lude.Maybe
        AttributeBooleanValue,
    userData ::
      Lude.Maybe
        AttributeValue,
    instanceInitiatedShutdownBehavior ::
      Lude.Maybe
        AttributeValue,
    productCodes ::
      Lude.Maybe
        [ProductCode],
    blockDeviceMappings ::
      Lude.Maybe
        [InstanceBlockDeviceMapping],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceAttributeResponse' with the minimum fields required to make a request.
--
-- * 'blockDeviceMappings' - The block device mapping of the instance.
-- * 'disableAPITermination' - If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O.
-- * 'enaSupport' - Indicates whether enhanced networking with ENA is enabled.
-- * 'enclaveOptions' - To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ ; otherwise, set it to @false@ .
-- * 'groups' - The security groups associated with the instance.
-- * 'instanceId' - The ID of the instance.
-- * 'instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
-- * 'instanceType' - The instance type.
-- * 'kernelId' - The kernel ID.
-- * 'productCodes' - A list of product codes.
-- * 'ramdiskId' - The RAM disk ID.
-- * 'responseStatus' - The response status code.
-- * 'rootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
-- * 'sourceDestCheck' - Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
-- * 'sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
-- * 'userData' - The user data.
mkDescribeInstanceAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInstanceAttributeResponse
mkDescribeInstanceAttributeResponse pResponseStatus_ =
  DescribeInstanceAttributeResponse'
    { instanceId = Lude.Nothing,
      groups = Lude.Nothing,
      enaSupport = Lude.Nothing,
      sourceDestCheck = Lude.Nothing,
      disableAPITermination = Lude.Nothing,
      enclaveOptions = Lude.Nothing,
      ramdiskId = Lude.Nothing,
      kernelId = Lude.Nothing,
      rootDeviceName = Lude.Nothing,
      instanceType = Lude.Nothing,
      sriovNetSupport = Lude.Nothing,
      ebsOptimized = Lude.Nothing,
      userData = Lude.Nothing,
      instanceInitiatedShutdownBehavior = Lude.Nothing,
      productCodes = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsInstanceId :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe Lude.Text)
desrsInstanceId = Lens.lens (instanceId :: DescribeInstanceAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The security groups associated with the instance.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsGroups :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe [GroupIdentifier])
desrsGroups = Lens.lens (groups :: DescribeInstanceAttributeResponse -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groups = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Indicates whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEnaSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
desrsEnaSupport = Lens.lens (enaSupport :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {enaSupport = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSourceDestCheck :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
desrsSourceDestCheck = Lens.lens (sourceDestCheck :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {sourceDestCheck = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
--
-- /Note:/ Consider using 'disableAPITermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsDisableAPITermination :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
desrsDisableAPITermination = Lens.lens (disableAPITermination :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {disableAPITermination = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsDisableAPITermination "Use generic-lens or generic-optics with 'disableAPITermination' instead." #-}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ ; otherwise, set it to @false@ .
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEnclaveOptions :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe EnclaveOptions)
desrsEnclaveOptions = Lens.lens (enclaveOptions :: DescribeInstanceAttributeResponse -> Lude.Maybe EnclaveOptions) (\s a -> s {enclaveOptions = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The RAM disk ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsRAMDiskId :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
desrsRAMDiskId = Lens.lens (ramdiskId :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {ramdiskId = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The kernel ID.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsKernelId :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
desrsKernelId = Lens.lens (kernelId :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {kernelId = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsRootDeviceName :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
desrsRootDeviceName = Lens.lens (rootDeviceName :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {rootDeviceName = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsInstanceType :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
desrsInstanceType = Lens.lens (instanceType :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {instanceType = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsSRIOVNetSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
desrsSRIOVNetSupport = Lens.lens (sriovNetSupport :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {sriovNetSupport = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsEBSOptimized :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
desrsEBSOptimized = Lens.lens (ebsOptimized :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {ebsOptimized = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The user data.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsUserData :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
desrsUserData = Lens.lens (userData :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {userData = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsInstanceInitiatedShutdownBehavior :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
desrsInstanceInitiatedShutdownBehavior = Lens.lens (instanceInitiatedShutdownBehavior :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {instanceInitiatedShutdownBehavior = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | A list of product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsProductCodes :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe [ProductCode])
desrsProductCodes = Lens.lens (productCodes :: DescribeInstanceAttributeResponse -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The block device mapping of the instance.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsBlockDeviceMappings :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe [InstanceBlockDeviceMapping])
desrsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: DescribeInstanceAttributeResponse -> Lude.Maybe [InstanceBlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeInstanceAttributeResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
