{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dihDryRun,
    dihExecutableUsers,
    dihFilters,
    dihImageIds,
    dihOwners,

    -- * Destructuring the response
    DescribeImagesResponse (..),
    mkDescribeImagesResponse,

    -- ** Response lenses
    dirfrsImages,
    dirfrsResponseStatus,
  )
where

import qualified Network.AWS.EC2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Core.Maybe Core.Bool,
    -- | Scopes the images by users with explicit launch permissions. Specify an AWS account ID, @self@ (the sender of the request), or @all@ (public AMIs).
    executableUsers :: Core.Maybe [Types.String],
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
    filters :: Core.Maybe [Types.Filter],
    -- | The image IDs.
    --
    -- Default: Describes all images available to you.
    imageIds :: Core.Maybe [Types.ImageId],
    -- | Scopes the results to images with the specified owners. You can specify a combination of AWS account IDs, @self@ , @amazon@ , and @aws-marketplace@ . If you omit this parameter, the results include all images for which you have launch permissions, regardless of ownership.
    owners :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImages' value with any optional fields omitted.
mkDescribeImages ::
  DescribeImages
mkDescribeImages =
  DescribeImages'
    { dryRun = Core.Nothing,
      executableUsers = Core.Nothing,
      filters = Core.Nothing,
      imageIds = Core.Nothing,
      owners = Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihDryRun :: Lens.Lens' DescribeImages (Core.Maybe Core.Bool)
dihDryRun = Lens.field @"dryRun"
{-# DEPRECATED dihDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | Scopes the images by users with explicit launch permissions. Specify an AWS account ID, @self@ (the sender of the request), or @all@ (public AMIs).
--
-- /Note:/ Consider using 'executableUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihExecutableUsers :: Lens.Lens' DescribeImages (Core.Maybe [Types.String])
dihExecutableUsers = Lens.field @"executableUsers"
{-# DEPRECATED dihExecutableUsers "Use generic-lens or generic-optics with 'executableUsers' instead." #-}

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
dihFilters :: Lens.Lens' DescribeImages (Core.Maybe [Types.Filter])
dihFilters = Lens.field @"filters"
{-# DEPRECATED dihFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The image IDs.
--
-- Default: Describes all images available to you.
--
-- /Note:/ Consider using 'imageIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihImageIds :: Lens.Lens' DescribeImages (Core.Maybe [Types.ImageId])
dihImageIds = Lens.field @"imageIds"
{-# DEPRECATED dihImageIds "Use generic-lens or generic-optics with 'imageIds' instead." #-}

-- | Scopes the results to images with the specified owners. You can specify a combination of AWS account IDs, @self@ , @amazon@ , and @aws-marketplace@ . If you omit this parameter, the results include all images for which you have launch permissions, regardless of ownership.
--
-- /Note:/ Consider using 'owners' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dihOwners :: Lens.Lens' DescribeImages (Core.Maybe [Types.String])
dihOwners = Lens.field @"owners"
{-# DEPRECATED dihOwners "Use generic-lens or generic-optics with 'owners' instead." #-}

instance Core.AWSRequest DescribeImages where
  type Rs DescribeImages = DescribeImagesResponse
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
            ( Core.pure ("Action", "DescribeImages")
                Core.<> (Core.pure ("Version", "2016-11-15"))
                Core.<> (Core.toQueryValue "DryRun" Core.<$> dryRun)
                Core.<> (Core.toQueryList "ExecutableBy" Core.<$> executableUsers)
                Core.<> (Core.toQueryList "Filter" Core.<$> filters)
                Core.<> (Core.toQueryList "ImageId" Core.<$> imageIds)
                Core.<> (Core.toQueryList "Owner" Core.<$> owners)
            )
      }
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImagesResponse'
            Core.<$> (x Core..@? "imagesSet" Core..<@> Core.parseXMLList "item")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | Information about the images.
    images :: Core.Maybe [Types.Image],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImagesResponse' value with any optional fields omitted.
mkDescribeImagesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeImagesResponse
mkDescribeImagesResponse responseStatus =
  DescribeImagesResponse' {images = Core.Nothing, responseStatus}

-- | Information about the images.
--
-- /Note:/ Consider using 'images' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsImages :: Lens.Lens' DescribeImagesResponse (Core.Maybe [Types.Image])
dirfrsImages = Lens.field @"images"
{-# DEPRECATED dirfrsImages "Use generic-lens or generic-optics with 'images' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirfrsResponseStatus :: Lens.Lens' DescribeImagesResponse Core.Int
dirfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dirfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
