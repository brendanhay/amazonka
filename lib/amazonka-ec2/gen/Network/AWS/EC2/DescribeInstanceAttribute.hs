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
    diafInstanceId,
    diafAttribute,
    diafDryRun,

    -- * Destructuring the response
    DescribeInstanceAttributeResponse (..),
    mkDescribeInstanceAttributeResponse,

    -- ** Response lenses
    diarsInstanceId,
    diarsGroups,
    diarsEnaSupport,
    diarsSourceDestCheck,
    diarsDisableAPITermination,
    diarsEnclaveOptions,
    diarsRAMDiskId,
    diarsKernelId,
    diarsRootDeviceName,
    diarsInstanceType,
    diarsSRIOVNetSupport,
    diarsEBSOptimized,
    diarsUserData,
    diarsInstanceInitiatedShutdownBehavior,
    diarsProductCodes,
    diarsBlockDeviceMappings,
    diarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeInstanceAttribute' smart constructor.
data DescribeInstanceAttribute = DescribeInstanceAttribute'
  { -- | The ID of the instance.
    instanceId :: Lude.Text,
    -- | The instance attribute.
    --
    -- Note: The @enaSupport@ attribute is not supported at this time.
    attribute :: InstanceAttributeName,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceAttribute' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'attribute' - The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeInstanceAttribute ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'attribute'
  InstanceAttributeName ->
  DescribeInstanceAttribute
mkDescribeInstanceAttribute pInstanceId_ pAttribute_ =
  DescribeInstanceAttribute'
    { instanceId = pInstanceId_,
      attribute = pAttribute_,
      dryRun = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafInstanceId :: Lens.Lens' DescribeInstanceAttribute Lude.Text
diafInstanceId = Lens.lens (instanceId :: DescribeInstanceAttribute -> Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diafInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The instance attribute.
--
-- Note: The @enaSupport@ attribute is not supported at this time.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafAttribute :: Lens.Lens' DescribeInstanceAttribute InstanceAttributeName
diafAttribute = Lens.lens (attribute :: DescribeInstanceAttribute -> InstanceAttributeName) (\s a -> s {attribute = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diafAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafDryRun :: Lens.Lens' DescribeInstanceAttribute (Lude.Maybe Lude.Bool)
diafDryRun = Lens.lens (dryRun :: DescribeInstanceAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeInstanceAttribute)
{-# DEPRECATED diafDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

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
        "InstanceId" Lude.=: instanceId,
        "Attribute" Lude.=: attribute,
        "DryRun" Lude.=: dryRun
      ]

-- | Describes an instance attribute.
--
-- /See:/ 'mkDescribeInstanceAttributeResponse' smart constructor.
data DescribeInstanceAttributeResponse = DescribeInstanceAttributeResponse'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The security groups associated with the instance.
    groups :: Lude.Maybe [GroupIdentifier],
    -- | Indicates whether enhanced networking with ENA is enabled.
    enaSupport :: Lude.Maybe AttributeBooleanValue,
    -- | Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
    sourceDestCheck :: Lude.Maybe AttributeBooleanValue,
    -- | If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
    disableAPITermination :: Lude.Maybe AttributeBooleanValue,
    -- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ ; otherwise, set it to @false@ .
    enclaveOptions :: Lude.Maybe EnclaveOptions,
    -- | The RAM disk ID.
    ramdiskId :: Lude.Maybe AttributeValue,
    -- | The kernel ID.
    kernelId :: Lude.Maybe AttributeValue,
    -- | The device name of the root device volume (for example, @/dev/sda1@ ).
    rootDeviceName :: Lude.Maybe AttributeValue,
    -- | The instance type.
    instanceType :: Lude.Maybe AttributeValue,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
    sriovNetSupport :: Lude.Maybe AttributeValue,
    -- | Indicates whether the instance is optimized for Amazon EBS I/O.
    ebsOptimized :: Lude.Maybe AttributeBooleanValue,
    -- | The user data.
    userData :: Lude.Maybe AttributeValue,
    -- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
    instanceInitiatedShutdownBehavior :: Lude.Maybe AttributeValue,
    -- | A list of product codes.
    productCodes :: Lude.Maybe [ProductCode],
    -- | The block device mapping of the instance.
    blockDeviceMappings :: Lude.Maybe [InstanceBlockDeviceMapping],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInstanceAttributeResponse' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'groups' - The security groups associated with the instance.
-- * 'enaSupport' - Indicates whether enhanced networking with ENA is enabled.
-- * 'sourceDestCheck' - Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
-- * 'disableAPITermination' - If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
-- * 'enclaveOptions' - To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ ; otherwise, set it to @false@ .
-- * 'ramdiskId' - The RAM disk ID.
-- * 'kernelId' - The kernel ID.
-- * 'rootDeviceName' - The device name of the root device volume (for example, @/dev/sda1@ ).
-- * 'instanceType' - The instance type.
-- * 'sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
-- * 'ebsOptimized' - Indicates whether the instance is optimized for Amazon EBS I/O.
-- * 'userData' - The user data.
-- * 'instanceInitiatedShutdownBehavior' - Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
-- * 'productCodes' - A list of product codes.
-- * 'blockDeviceMappings' - The block device mapping of the instance.
-- * 'responseStatus' - The response status code.
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
diarsInstanceId :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe Lude.Text)
diarsInstanceId = Lens.lens (instanceId :: DescribeInstanceAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The security groups associated with the instance.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsGroups :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe [GroupIdentifier])
diarsGroups = Lens.lens (groups :: DescribeInstanceAttributeResponse -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groups = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Indicates whether enhanced networking with ENA is enabled.
--
-- /Note:/ Consider using 'enaSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsEnaSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
diarsEnaSupport = Lens.lens (enaSupport :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {enaSupport = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsEnaSupport "Use generic-lens or generic-optics with 'enaSupport' instead." #-}

-- | Indicates whether source/destination checking is enabled. A value of @true@ means that checking is enabled, and @false@ means that checking is disabled. This value must be @false@ for a NAT instance to perform NAT.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsSourceDestCheck :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
diarsSourceDestCheck = Lens.lens (sourceDestCheck :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {sourceDestCheck = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | If the value is @true@ , you can't terminate the instance through the Amazon EC2 console, CLI, or API; otherwise, you can.
--
-- /Note:/ Consider using 'disableAPITermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsDisableAPITermination :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
diarsDisableAPITermination = Lens.lens (disableAPITermination :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {disableAPITermination = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsDisableAPITermination "Use generic-lens or generic-optics with 'disableAPITermination' instead." #-}

-- | To enable the instance for AWS Nitro Enclaves, set this parameter to @true@ ; otherwise, set it to @false@ .
--
-- /Note:/ Consider using 'enclaveOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsEnclaveOptions :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe EnclaveOptions)
diarsEnclaveOptions = Lens.lens (enclaveOptions :: DescribeInstanceAttributeResponse -> Lude.Maybe EnclaveOptions) (\s a -> s {enclaveOptions = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsEnclaveOptions "Use generic-lens or generic-optics with 'enclaveOptions' instead." #-}

-- | The RAM disk ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsRAMDiskId :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
diarsRAMDiskId = Lens.lens (ramdiskId :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {ramdiskId = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The kernel ID.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsKernelId :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
diarsKernelId = Lens.lens (kernelId :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {kernelId = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | The device name of the root device volume (for example, @/dev/sda1@ ).
--
-- /Note:/ Consider using 'rootDeviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsRootDeviceName :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
diarsRootDeviceName = Lens.lens (rootDeviceName :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {rootDeviceName = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsRootDeviceName "Use generic-lens or generic-optics with 'rootDeviceName' instead." #-}

-- | The instance type.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsInstanceType :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
diarsInstanceType = Lens.lens (instanceType :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {instanceType = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsSRIOVNetSupport :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
diarsSRIOVNetSupport = Lens.lens (sriovNetSupport :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {sriovNetSupport = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | Indicates whether the instance is optimized for Amazon EBS I/O.
--
-- /Note:/ Consider using 'ebsOptimized' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsEBSOptimized :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeBooleanValue)
diarsEBSOptimized = Lens.lens (ebsOptimized :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {ebsOptimized = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsEBSOptimized "Use generic-lens or generic-optics with 'ebsOptimized' instead." #-}

-- | The user data.
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsUserData :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
diarsUserData = Lens.lens (userData :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {userData = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | Indicates whether an instance stops or terminates when you initiate shutdown from the instance (using the operating system command for system shutdown).
--
-- /Note:/ Consider using 'instanceInitiatedShutdownBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsInstanceInitiatedShutdownBehavior :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe AttributeValue)
diarsInstanceInitiatedShutdownBehavior = Lens.lens (instanceInitiatedShutdownBehavior :: DescribeInstanceAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {instanceInitiatedShutdownBehavior = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsInstanceInitiatedShutdownBehavior "Use generic-lens or generic-optics with 'instanceInitiatedShutdownBehavior' instead." #-}

-- | A list of product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsProductCodes :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe [ProductCode])
diarsProductCodes = Lens.lens (productCodes :: DescribeInstanceAttributeResponse -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The block device mapping of the instance.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsBlockDeviceMappings :: Lens.Lens' DescribeInstanceAttributeResponse (Lude.Maybe [InstanceBlockDeviceMapping])
diarsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: DescribeInstanceAttributeResponse -> Lude.Maybe [InstanceBlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsResponseStatus :: Lens.Lens' DescribeInstanceAttributeResponse Lude.Int
diarsResponseStatus = Lens.lens (responseStatus :: DescribeInstanceAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInstanceAttributeResponse)
{-# DEPRECATED diarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
