{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified images (AMIs, AKIs, and ARIs) available to you or all of the images available to you.
--
-- The images available to you include public images, private images that you own, and private images owned by other AWS accounts for which you have explicit launch permissions.
-- Recently deregistered images appear in the returned results for a short interval and then return empty results. After all instances that reference a deregistered AMI are terminated, specifying the ID of the image results in an error indicating that the AMI ID cannot be found.
module Network.AWS.EC2.DescribeImages
  ( -- * Creating a request
    DescribeImages (..),
    mkDescribeImages,

    -- ** Request lenses
    deseOwners,
    deseExecutableUsers,
    deseFilters,
    deseImageIds,
    deseDryRun,

    -- * Destructuring the response
    DescribeImagesResponse (..),
    mkDescribeImagesResponse,

    -- ** Response lenses
    diirsImages,
    diirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { owners ::
      Lude.Maybe [Lude.Text],
    executableUsers :: Lude.Maybe [Lude.Text],
    filters :: Lude.Maybe [Filter],
    imageIds :: Lude.Maybe [Lude.Text],
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'executableUsers' - Scopes the images by users with explicit launch permissions. Specify an AWS account ID, @self@ (the sender of the request), or @all@ (public AMIs).
-- * 'filters' - The filters.
--
--
--     * @architecture@ - The image architecture (@i386@ | @x86_64@ | @arm64@ ).
--
--
--     * @block-device-mapping.delete-on-termination@ - A Boolean value that indicates whether the Amazon EBS volume is deleted on instance termination.
--
--
--     * @block-device-mapping.device-name@ - The device name specified in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).
--
--
--     * @block-device-mapping.snapshot-id@ - The ID of the snapshot used for the EBS volume.
--
--
--     * @block-device-mapping.volume-size@ - The volume size of the EBS volume, in GiB.
--
--
--     * @block-device-mapping.volume-type@ - The volume type of the EBS volume (@gp2@ | @io1@ | @io2@ | @st1 @ | @sc1@ | @standard@ ).
--
--
--     * @block-device-mapping.encrypted@ - A Boolean that indicates whether the EBS volume is encrypted.
--
--
--     * @description@ - The description of the image (provided during image creation).
--
--
--     * @ena-support@ - A Boolean that indicates whether enhanced networking with ENA is enabled.
--
--
--     * @hypervisor@ - The hypervisor type (@ovm@ | @xen@ ).
--
--
--     * @image-id@ - The ID of the image.
--
--
--     * @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@ ).
--
--
--     * @is-public@ - A Boolean that indicates whether the image is public.
--
--
--     * @kernel-id@ - The kernel ID.
--
--
--     * @manifest-location@ - The location of the image manifest.
--
--
--     * @name@ - The name of the AMI (provided during image creation).
--
--
--     * @owner-alias@ - The owner alias, from an Amazon-maintained list (@amazon@ | @aws-marketplace@ ). This is not the user-configured AWS account alias set using the IAM console. We recommend that you use the related parameter instead of this filter.
--
--
--     * @owner-id@ - The AWS account ID of the owner. We recommend that you use the related parameter instead of this filter.
--
--
--     * @platform@ - The platform. To only list Windows-based AMIs, use @windows@ .
--
--
--     * @product-code@ - The product code.
--
--
--     * @product-code.type@ - The type of the product code (@devpay@ | @marketplace@ ).
--
--
--     * @ramdisk-id@ - The RAM disk ID.
--
--
--     * @root-device-name@ - The device name of the root device volume (for example, @/dev/sda1@ ).
--
--
--     * @root-device-type@ - The type of the root device volume (@ebs@ | @instance-store@ ).
--
--
--     * @state@ - The state of the image (@available@ | @pending@ | @failed@ ).
--
--
--     * @state-reason-code@ - The reason code for the state change.
--
--
--     * @state-reason-message@ - The message for the state change.
--
--
--     * @sriov-net-support@ - A value of @simple@ indicates that enhanced networking with the Intel 82599 VF interface is enabled.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @virtualization-type@ - The virtualization type (@paravirtual@ | @hvm@ ).
--
--
-- * 'imageIds' - The image IDs.
--
-- Default: Describes all images available to you.
-- * 'owners' - Scopes the results to images with the specified owners. You can specify a combination of AWS account IDs, @self@ , @amazon@ , and @aws-marketplace@ . If you omit this parameter, the results include all images for which you have launch permissions, regardless of ownership.
mkDescribeImages ::
  DescribeImages
mkDescribeImages =
  DescribeImages'
    { owners = Lude.Nothing,
      executableUsers = Lude.Nothing,
      filters = Lude.Nothing,
      imageIds = Lude.Nothing,
      dryRun = Lude.Nothing
    }

-- | Scopes the results to images with the specified owners. You can specify a combination of AWS account IDs, @self@ , @amazon@ , and @aws-marketplace@ . If you omit this parameter, the results include all images for which you have launch permissions, regardless of ownership.
--
-- /Note:/ Consider using 'owners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseOwners :: Lens.Lens' DescribeImages (Lude.Maybe [Lude.Text])
deseOwners = Lens.lens (owners :: DescribeImages -> Lude.Maybe [Lude.Text]) (\s a -> s {owners = a} :: DescribeImages)
{-# DEPRECATED deseOwners "Use generic-lens or generic-optics with 'owners' instead." #-}

-- | Scopes the images by users with explicit launch permissions. Specify an AWS account ID, @self@ (the sender of the request), or @all@ (public AMIs).
--
-- /Note:/ Consider using 'executableUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseExecutableUsers :: Lens.Lens' DescribeImages (Lude.Maybe [Lude.Text])
deseExecutableUsers = Lens.lens (executableUsers :: DescribeImages -> Lude.Maybe [Lude.Text]) (\s a -> s {executableUsers = a} :: DescribeImages)
{-# DEPRECATED deseExecutableUsers "Use generic-lens or generic-optics with 'executableUsers' instead." #-}

-- | The filters.
--
--
--     * @architecture@ - The image architecture (@i386@ | @x86_64@ | @arm64@ ).
--
--
--     * @block-device-mapping.delete-on-termination@ - A Boolean value that indicates whether the Amazon EBS volume is deleted on instance termination.
--
--
--     * @block-device-mapping.device-name@ - The device name specified in the block device mapping (for example, @/dev/sdh@ or @xvdh@ ).
--
--
--     * @block-device-mapping.snapshot-id@ - The ID of the snapshot used for the EBS volume.
--
--
--     * @block-device-mapping.volume-size@ - The volume size of the EBS volume, in GiB.
--
--
--     * @block-device-mapping.volume-type@ - The volume type of the EBS volume (@gp2@ | @io1@ | @io2@ | @st1 @ | @sc1@ | @standard@ ).
--
--
--     * @block-device-mapping.encrypted@ - A Boolean that indicates whether the EBS volume is encrypted.
--
--
--     * @description@ - The description of the image (provided during image creation).
--
--
--     * @ena-support@ - A Boolean that indicates whether enhanced networking with ENA is enabled.
--
--
--     * @hypervisor@ - The hypervisor type (@ovm@ | @xen@ ).
--
--
--     * @image-id@ - The ID of the image.
--
--
--     * @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@ ).
--
--
--     * @is-public@ - A Boolean that indicates whether the image is public.
--
--
--     * @kernel-id@ - The kernel ID.
--
--
--     * @manifest-location@ - The location of the image manifest.
--
--
--     * @name@ - The name of the AMI (provided during image creation).
--
--
--     * @owner-alias@ - The owner alias, from an Amazon-maintained list (@amazon@ | @aws-marketplace@ ). This is not the user-configured AWS account alias set using the IAM console. We recommend that you use the related parameter instead of this filter.
--
--
--     * @owner-id@ - The AWS account ID of the owner. We recommend that you use the related parameter instead of this filter.
--
--
--     * @platform@ - The platform. To only list Windows-based AMIs, use @windows@ .
--
--
--     * @product-code@ - The product code.
--
--
--     * @product-code.type@ - The type of the product code (@devpay@ | @marketplace@ ).
--
--
--     * @ramdisk-id@ - The RAM disk ID.
--
--
--     * @root-device-name@ - The device name of the root device volume (for example, @/dev/sda1@ ).
--
--
--     * @root-device-type@ - The type of the root device volume (@ebs@ | @instance-store@ ).
--
--
--     * @state@ - The state of the image (@available@ | @pending@ | @failed@ ).
--
--
--     * @state-reason-code@ - The reason code for the state change.
--
--
--     * @state-reason-message@ - The message for the state change.
--
--
--     * @sriov-net-support@ - A value of @simple@ indicates that enhanced networking with the Intel 82599 VF interface is enabled.
--
--
--     * @tag@ :<key> - The key/value combination of a tag assigned to the resource. Use the tag key in the filter name and the tag value as the filter value. For example, to find all resources that have a tag with the key @Owner@ and the value @TeamA@ , specify @tag:Owner@ for the filter name and @TeamA@ for the filter value.
--
--
--     * @tag-key@ - The key of a tag assigned to the resource. Use this filter to find all resources assigned a tag with a specific key, regardless of the tag value.
--
--
--     * @virtualization-type@ - The virtualization type (@paravirtual@ | @hvm@ ).
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseFilters :: Lens.Lens' DescribeImages (Lude.Maybe [Filter])
deseFilters = Lens.lens (filters :: DescribeImages -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeImages)
{-# DEPRECATED deseFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The image IDs.
--
-- Default: Describes all images available to you.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseImageIds :: Lens.Lens' DescribeImages (Lude.Maybe [Lude.Text])
deseImageIds = Lens.lens (imageIds :: DescribeImages -> Lude.Maybe [Lude.Text]) (\s a -> s {imageIds = a} :: DescribeImages)
{-# DEPRECATED deseImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deseDryRun :: Lens.Lens' DescribeImages (Lude.Maybe Lude.Bool)
deseDryRun = Lens.lens (dryRun :: DescribeImages -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DescribeImages)
{-# DEPRECATED deseDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DescribeImages where
  type Rs DescribeImages = DescribeImagesResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DescribeImagesResponse'
            Lude.<$> ( x Lude..@? "imagesSet" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "item")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeImages where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeImages where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeImages where
  toQuery DescribeImages' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeImages" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        Lude.toQuery (Lude.toQueryList "Owner" Lude.<$> owners),
        Lude.toQuery
          (Lude.toQueryList "ExecutableBy" Lude.<$> executableUsers),
        Lude.toQuery (Lude.toQueryList "Filter" Lude.<$> filters),
        Lude.toQuery (Lude.toQueryList "ImageId" Lude.<$> imageIds),
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { images ::
      Lude.Maybe [Image],
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

-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- * 'images' - Information about the images.
-- * 'responseStatus' - The response status code.
mkDescribeImagesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeImagesResponse
mkDescribeImagesResponse pResponseStatus_ =
  DescribeImagesResponse'
    { images = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the images.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsImages :: Lens.Lens' DescribeImagesResponse (Lude.Maybe [Image])
diirsImages = Lens.lens (images :: DescribeImagesResponse -> Lude.Maybe [Image]) (\s a -> s {images = a} :: DescribeImagesResponse)
{-# DEPRECATED diirsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diirsResponseStatus :: Lens.Lens' DescribeImagesResponse Lude.Int
diirsResponseStatus = Lens.lens (responseStatus :: DescribeImagesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeImagesResponse)
{-# DEPRECATED diirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
