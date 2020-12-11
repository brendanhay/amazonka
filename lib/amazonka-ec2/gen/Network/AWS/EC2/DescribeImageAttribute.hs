{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    diaiDryRun,
    diaiAttribute,
    diaiImageId,

    -- * Destructuring the response
    DescribeImageAttributeResponse (..),
    mkDescribeImageAttributeResponse,

    -- ** Response lenses
    diarsLaunchPermissions,
    diarsRAMDiskId,
    diarsKernelId,
    diarsSRIOVNetSupport,
    diarsImageId,
    diarsProductCodes,
    diarsDescription,
    diarsBlockDeviceMappings,
    diarsResponseStatus,
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
  { dryRun ::
      Lude.Maybe Lude.Bool,
    attribute :: ImageAttributeName,
    imageId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The AMI attribute.
--
-- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'imageId' - The ID of the AMI.
mkDescribeImageAttribute ::
  -- | 'attribute'
  ImageAttributeName ->
  -- | 'imageId'
  Lude.Text ->
  DescribeImageAttribute
mkDescribeImageAttribute pAttribute_ pImageId_ =
  DescribeImageAttribute'
    { dryRun = Lude.Nothing,
      attribute = pAttribute_,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaiDryRun :: Lens.Lens' DescribeImageAttribute (Lude.Maybe Lude.Bool)
diaiDryRun = Lens.lens (dryRun :: DescribeImageAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeImageAttribute)
{-# DEPRECATED diaiDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The AMI attribute.
--
-- __Note__ : Depending on your account privileges, the @blockDeviceMapping@ attribute may return a @Client.AuthFailure@ error. If this happens, use 'DescribeImages' to get information about the block device mapping for the AMI.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaiAttribute :: Lens.Lens' DescribeImageAttribute ImageAttributeName
diaiAttribute = Lens.lens (attribute :: DescribeImageAttribute -> ImageAttributeName) (\s a -> s {attribute = a} :: DescribeImageAttribute)
{-# DEPRECATED diaiAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diaiImageId :: Lens.Lens' DescribeImageAttribute Lude.Text
diaiImageId = Lens.lens (imageId :: DescribeImageAttribute -> Lude.Text) (\s a -> s {imageId = a} :: DescribeImageAttribute)
{-# DEPRECATED diaiImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

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
        "DryRun" Lude.=: dryRun,
        "Attribute" Lude.=: attribute,
        "ImageId" Lude.=: imageId
      ]

-- | Describes an image attribute.
--
-- /See:/ 'mkDescribeImageAttributeResponse' smart constructor.
data DescribeImageAttributeResponse = DescribeImageAttributeResponse'
  { launchPermissions ::
      Lude.Maybe [LaunchPermission],
    ramdiskId ::
      Lude.Maybe AttributeValue,
    kernelId ::
      Lude.Maybe AttributeValue,
    sriovNetSupport ::
      Lude.Maybe AttributeValue,
    imageId ::
      Lude.Maybe Lude.Text,
    productCodes ::
      Lude.Maybe [ProductCode],
    description ::
      Lude.Maybe AttributeValue,
    blockDeviceMappings ::
      Lude.Maybe
        [BlockDeviceMapping],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImageAttributeResponse' with the minimum fields required to make a request.
--
-- * 'blockDeviceMappings' - The block device mapping entries.
-- * 'description' - A description for the AMI.
-- * 'imageId' - The ID of the AMI.
-- * 'kernelId' - The kernel ID.
-- * 'launchPermissions' - The launch permissions.
-- * 'productCodes' - The product codes.
-- * 'ramdiskId' - The RAM disk ID.
-- * 'responseStatus' - The response status code.
-- * 'sriovNetSupport' - Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
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
diarsLaunchPermissions :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe [LaunchPermission])
diarsLaunchPermissions = Lens.lens (launchPermissions :: DescribeImageAttributeResponse -> Lude.Maybe [LaunchPermission]) (\s a -> s {launchPermissions = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsLaunchPermissions "Use generic-lens or generic-optics with 'launchPermissions' instead." #-}

-- | The RAM disk ID.
--
-- /Note:/ Consider using 'ramdiskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsRAMDiskId :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diarsRAMDiskId = Lens.lens (ramdiskId :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {ramdiskId = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsRAMDiskId "Use generic-lens or generic-optics with 'ramdiskId' instead." #-}

-- | The kernel ID.
--
-- /Note:/ Consider using 'kernelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsKernelId :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diarsKernelId = Lens.lens (kernelId :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {kernelId = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsKernelId "Use generic-lens or generic-optics with 'kernelId' instead." #-}

-- | Indicates whether enhanced networking with the Intel 82599 Virtual Function interface is enabled.
--
-- /Note:/ Consider using 'sriovNetSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsSRIOVNetSupport :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diarsSRIOVNetSupport = Lens.lens (sriovNetSupport :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {sriovNetSupport = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsSRIOVNetSupport "Use generic-lens or generic-optics with 'sriovNetSupport' instead." #-}

-- | The ID of the AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsImageId :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe Lude.Text)
diarsImageId = Lens.lens (imageId :: DescribeImageAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

-- | The product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsProductCodes :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe [ProductCode])
diarsProductCodes = Lens.lens (productCodes :: DescribeImageAttributeResponse -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | A description for the AMI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsDescription :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe AttributeValue)
diarsDescription = Lens.lens (description :: DescribeImageAttributeResponse -> Lude.Maybe AttributeValue) (\s a -> s {description = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The block device mapping entries.
--
-- /Note:/ Consider using 'blockDeviceMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsBlockDeviceMappings :: Lens.Lens' DescribeImageAttributeResponse (Lude.Maybe [BlockDeviceMapping])
diarsBlockDeviceMappings = Lens.lens (blockDeviceMappings :: DescribeImageAttributeResponse -> Lude.Maybe [BlockDeviceMapping]) (\s a -> s {blockDeviceMappings = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsBlockDeviceMappings "Use generic-lens or generic-optics with 'blockDeviceMappings' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diarsResponseStatus :: Lens.Lens' DescribeImageAttributeResponse Lude.Int
diarsResponseStatus = Lens.lens (responseStatus :: DescribeImageAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImageAttributeResponse)
{-# DEPRECATED diarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
