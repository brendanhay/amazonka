{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified attribute of the specified volume. You can specify only one attribute at a time.
--
-- For more information about EBS volumes, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumes.html Amazon EBS Volumes> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DescribeVolumeAttribute
  ( -- * Creating a request
    DescribeVolumeAttribute (..),
    mkDescribeVolumeAttribute,

    -- ** Request lenses
    dvafAttribute,
    dvafVolumeId,
    dvafDryRun,

    -- * Destructuring the response
    DescribeVolumeAttributeResponse (..),
    mkDescribeVolumeAttributeResponse,

    -- ** Response lenses
    dvarsVolumeId,
    dvarsProductCodes,
    dvarsAutoEnableIO,
    dvarsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeVolumeAttribute' smart constructor.
data DescribeVolumeAttribute = DescribeVolumeAttribute'
  { -- | The attribute of the volume. This parameter is required.
    attribute :: VolumeAttributeName,
    -- | The ID of the volume.
    volumeId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumeAttribute' with the minimum fields required to make a request.
--
-- * 'attribute' - The attribute of the volume. This parameter is required.
-- * 'volumeId' - The ID of the volume.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDescribeVolumeAttribute ::
  -- | 'attribute'
  VolumeAttributeName ->
  -- | 'volumeId'
  Lude.Text ->
  DescribeVolumeAttribute
mkDescribeVolumeAttribute pAttribute_ pVolumeId_ =
  DescribeVolumeAttribute'
    { attribute = pAttribute_,
      volumeId = pVolumeId_,
      dryRun = Lude.Nothing
    }

-- | The attribute of the volume. This parameter is required.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvafAttribute :: Lens.Lens' DescribeVolumeAttribute VolumeAttributeName
dvafAttribute = Lens.lens (attribute :: DescribeVolumeAttribute -> VolumeAttributeName) (\s a -> s {attribute = a} :: DescribeVolumeAttribute)
{-# DEPRECATED dvafAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvafVolumeId :: Lens.Lens' DescribeVolumeAttribute Lude.Text
dvafVolumeId = Lens.lens (volumeId :: DescribeVolumeAttribute -> Lude.Text) (\s a -> s {volumeId = a} :: DescribeVolumeAttribute)
{-# DEPRECATED dvafVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvafDryRun :: Lens.Lens' DescribeVolumeAttribute (Lude.Maybe Lude.Bool)
dvafDryRun = Lens.lens (dryRun :: DescribeVolumeAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeVolumeAttribute)
{-# DEPRECATED dvafDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeVolumeAttribute where
  type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeVolumeAttributeResponse'
            Lude.<$> (x Lude..@? "volumeId")
            Lude.<*> ( x Lude..@? "productCodes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (x Lude..@? "autoEnableIO")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVolumeAttribute where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeVolumeAttribute where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVolumeAttribute where
  toQuery DescribeVolumeAttribute' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeVolumeAttribute" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "Attribute" Lude.=: attribute,
        "VolumeId" Lude.=: volumeId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeVolumeAttributeResponse' smart constructor.
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse'
  { -- | The ID of the volume.
    volumeId :: Lude.Maybe Lude.Text,
    -- | A list of product codes.
    productCodes :: Lude.Maybe [ProductCode],
    -- | The state of @autoEnableIO@ attribute.
    autoEnableIO :: Lude.Maybe AttributeBooleanValue,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVolumeAttributeResponse' with the minimum fields required to make a request.
--
-- * 'volumeId' - The ID of the volume.
-- * 'productCodes' - A list of product codes.
-- * 'autoEnableIO' - The state of @autoEnableIO@ attribute.
-- * 'responseStatus' - The response status code.
mkDescribeVolumeAttributeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVolumeAttributeResponse
mkDescribeVolumeAttributeResponse pResponseStatus_ =
  DescribeVolumeAttributeResponse'
    { volumeId = Lude.Nothing,
      productCodes = Lude.Nothing,
      autoEnableIO = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the volume.
--
-- /Note:/ Consider using 'volumeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarsVolumeId :: Lens.Lens' DescribeVolumeAttributeResponse (Lude.Maybe Lude.Text)
dvarsVolumeId = Lens.lens (volumeId :: DescribeVolumeAttributeResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeId = a} :: DescribeVolumeAttributeResponse)
{-# DEPRECATED dvarsVolumeId "Use generic-lens or generic-optics with 'volumeId' instead." #-}

-- | A list of product codes.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarsProductCodes :: Lens.Lens' DescribeVolumeAttributeResponse (Lude.Maybe [ProductCode])
dvarsProductCodes = Lens.lens (productCodes :: DescribeVolumeAttributeResponse -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: DescribeVolumeAttributeResponse)
{-# DEPRECATED dvarsProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The state of @autoEnableIO@ attribute.
--
-- /Note:/ Consider using 'autoEnableIO' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarsAutoEnableIO :: Lens.Lens' DescribeVolumeAttributeResponse (Lude.Maybe AttributeBooleanValue)
dvarsAutoEnableIO = Lens.lens (autoEnableIO :: DescribeVolumeAttributeResponse -> Lude.Maybe AttributeBooleanValue) (\s a -> s {autoEnableIO = a} :: DescribeVolumeAttributeResponse)
{-# DEPRECATED dvarsAutoEnableIO "Use generic-lens or generic-optics with 'autoEnableIO' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvarsResponseStatus :: Lens.Lens' DescribeVolumeAttributeResponse Lude.Int
dvarsResponseStatus = Lens.lens (responseStatus :: DescribeVolumeAttributeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVolumeAttributeResponse)
{-# DEPRECATED dvarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
