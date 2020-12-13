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
    diaAttribute,
    diaImageId,
    diaDryRun,

    -- * Destructuring the response
    DescribeImageAttributeResponse (..),
    mkDescribeImageAttributeResponse,

    -- ** Response lenses
    diafrsLaunchPermissions,
    diafrsRAMDiskId,
    diafrsKernelId,
    diafrsSRIOVNetSupport,
    diafrsImageId,
    diafrsProductCodes,
    diafrsDescription,
    diafrsBlockDeviceMappings,
    diafrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for DescribeImageAttribute.
--
-- /See:/ 'mkDescribeImageAttribute' smart constructor.
data DescribeImageAttribute = DescribeImageAttribute'
  { -- | The AMI attribute.
    --
    -- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
    attribute :: ImageAttributeName,
    -- | The ID of the AMI.
    imageId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The AMI attribute.
--
-- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
-- * 'imageId' - The ID of the AMI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeImageAttribute ::
  -- | 'attribute'
  ImageAttributeName ->
  -- | 'imageId'
  Lude.Text ->
  DescribeImageAttribute
mkDescribeImageAttribute pAttribute_ pImageId_ =
  DescribeImageAttribute'
    { attribute = pAttribute_,
      imageId = pImageId_,
      dryRun = Lude.Nothing
    }

-- | The AMI attribute.
--
-- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaAttribute :: Lens.Lens' DescribeImageAttribute ImageAttributeName
diaAttribute = Lens.lens (attribute :: DescribeImageAttribute -> ImageAttributeName) (\s a -> s {attribute = a} :: DescribeImageAttribute)
{-# DEPRECATED diaAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaImageId :: Lens.Lens' DescribeImageAttribute Lude.Text
diaImageId = Lens.lens (imageId :: DescribeImageAttribute -> Lude.Text) (\s a -> s {imageId = a} :: DescribeImageAttribute)
{-# DEPRECATED diaImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaDryRun :: Lens.Lens' DescribeImageAttribute (Lude.Maybe Lude.Bool)
diaDryRun = Lens.lens (dryRun :: DescribeImageAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeImageAttribute)
{-# DEPRECATED diaDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeImageAttribute where
  type Rs DescribeImageAttribute = DescribeImageAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeImageAttributeResponse'
            Lude.<$> ( x Lude..@? "launchPermission" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "ramdisk")
            Lude.<*> (x Lude..@? "kernel")
            Lude.<*> (x Lude..@? "sriovNetSupport")
            Lude.<*> (x Lude..@? "imageId")
            Lude.<*> ( x Lude..@? "productCodes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "description")
            Lude.<*> ( x Lude..@? "blockDeviceMapping" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImageAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeImageAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImageAttribute where
  toQuery DescribeImageAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeImageAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        "ImageId" Lude.=: imageId,
        "DryRun" Lude.=: dryRun
      ]

-- | Describes an image attribute.
--
-- /See:/ 'mkDescribeImageAttributeResponse' smart constructor.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
  { -- | The launch permissions.
    launchPermissions :: Lude.Maybe [LaunchPermission],
    -- | The RAM disk ID.
    ramdiskId :: Lude.Maybe AttributeValue,
    -- | The kernel ID.
    kernelId :: Lude.Maybe AttributeValue,
    -- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
    sriovNetSupport :: Lude.Maybe AttributeValue,
    -- | The ID of the AMI.
    imageId :: Lude.Maybe Lude.Text,
    -- | The product codes.
    productCodes :: Lude.Maybe [ProductCode],
    -- | A description for the AMI.
    description :: Lude.Maybe AttributeValue,
    -- | The block device mapping entries.
    blockDeviceMappings :: Lude.Maybe [BlockDeviceMapping],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageAttributeResponse' with the minimum fields required to make a request.
--
-- * 'launchPermissions' - The launch permissions.
-- * 'ramdiskId' - The RAM disk ID.
-- * 'kernelId' - The kernel ID.
-- * 'sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
-- * 'imageId' - The ID of the AMI.
-- * 'productCodes' - The product codes.
-- * 'description' - A description for the AMI.
-- * 'blockDeviceMappings' - The block device mapping entries.
-- * 'responseStatus' - The response status code.
mkDescribeImageAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImageAttributeResponse
mkDescribeImageAttributeResponse pResponseStatus_ =
  DescribeImageAttributeResponse'
    { launchPermissions = Lude.Nothing,
      ramdiskId = Lude.Nothing,
      kernelId = Lude.Nothing,
      sriovNetSupport = Lude.Nothing,
      imageId = Lude.Nothing,
      productCodes = Lude.Nothing,
      description = Lude.Nothing,
      blockDeviceMappings = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The launch permissions.
--
-- /Note:/ Consider using 'launchPermissions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsLaunchPermissions :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe [LaunchPermission])
diafrsLaunchPermissions = Lens.lens (launchPermissions :: DescribeImageAttributeResponse -> Lude.Maybe [LaunchPermission]) (\s a -> s {launchPermissions = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsLaunchPermissions "Use generic-lens or generic-optics with 'launchPermissions' instead." #-}

-- | The RAM disk ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsRAMDiskId :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diafrsRAMDiskId = Lens.lens (ramdiskId :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {ramdiskId = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The kernel ID.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsKernelId :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diafrsKernelId = Lens.lens (kernelId :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {kernelId = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsSRIOVNetSupport :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diafrsSRIOVNetSupport = Lens.lens (sriovNetSupport :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {sriovNetSupport = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsImageId :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe Lude.Text)
diafrsImageId = Lens.lens (imageId :: DescribeImageAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsProductCodes :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe [ProductCode])
diafrsProductCodes = Lens.lens (productCodes :: DescribeImageAttributeResponse -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | A description for the AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsDescription :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diafrsDescription = Lens.lens (description :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {description = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsBlockDeviceMappings :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe [BlockDeviceMapping])
diafrsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: DescribeImageAttributeResponse -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diafrsResponseStatus :: Lens.Lens' DescribeImageAttributeResponse Lude.Int
diafrsResponseStatus = Lens.lens (responseStatus :: DescribeImageAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
