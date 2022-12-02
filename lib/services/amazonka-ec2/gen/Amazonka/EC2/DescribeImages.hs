{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.EC2.DescribeImages
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified images (AMIs, AKIs, and ARIs) available to you
-- or all of the images available to you.
--
-- The images available to you include public images, private images that
-- you own, and private images owned by other Amazon Web Services accounts
-- for which you have explicit launch permissions.
--
-- Recently deregistered images appear in the returned results for a short
-- interval and then return empty results. After all instances that
-- reference a deregistered AMI are terminated, specifying the ID of the
-- image will eventually return an error indicating that the AMI ID cannot
-- be found.
module Amazonka.EC2.DescribeImages
  ( -- * Creating a Request
    DescribeImages (..),
    newDescribeImages,

    -- * Request Lenses
    describeImages_imageIds,
    describeImages_filters,
    describeImages_owners,
    describeImages_dryRun,
    describeImages_includeDeprecated,
    describeImages_executableUsers,

    -- * Destructuring the Response
    DescribeImagesResponse (..),
    newDescribeImagesResponse,

    -- * Response Lenses
    describeImagesResponse_images,
    describeImagesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeImages' smart constructor.
data DescribeImages = DescribeImages'
  { -- | The image IDs.
    --
    -- Default: Describes all images available to you.
    imageIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @architecture@ - The image architecture (@i386@ | @x86_64@ |
    --     @arm64@).
    --
    -- -   @block-device-mapping.delete-on-termination@ - A Boolean value that
    --     indicates whether the Amazon EBS volume is deleted on instance
    --     termination.
    --
    -- -   @block-device-mapping.device-name@ - The device name specified in
    --     the block device mapping (for example, @\/dev\/sdh@ or @xvdh@).
    --
    -- -   @block-device-mapping.snapshot-id@ - The ID of the snapshot used for
    --     the Amazon EBS volume.
    --
    -- -   @block-device-mapping.volume-size@ - The volume size of the Amazon
    --     EBS volume, in GiB.
    --
    -- -   @block-device-mapping.volume-type@ - The volume type of the Amazon
    --     EBS volume (@io1@ | @io2@ | @gp2@ | @gp3@ | @sc1 @| @st1@ |
    --     @standard@).
    --
    -- -   @block-device-mapping.encrypted@ - A Boolean that indicates whether
    --     the Amazon EBS volume is encrypted.
    --
    -- -   @creation-date@ - The time when the image was created, in the ISO
    --     8601 format in the UTC time zone (YYYY-MM-DDThh:mm:ss.sssZ), for
    --     example, @2021-09-29T11:04:43.305Z@. You can use a wildcard (@*@),
    --     for example, @2021-09-29T*@, which matches an entire day.
    --
    -- -   @description@ - The description of the image (provided during image
    --     creation).
    --
    -- -   @ena-support@ - A Boolean that indicates whether enhanced networking
    --     with ENA is enabled.
    --
    -- -   @hypervisor@ - The hypervisor type (@ovm@ | @xen@).
    --
    -- -   @image-id@ - The ID of the image.
    --
    -- -   @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@).
    --
    -- -   @is-public@ - A Boolean that indicates whether the image is public.
    --
    -- -   @kernel-id@ - The kernel ID.
    --
    -- -   @manifest-location@ - The location of the image manifest.
    --
    -- -   @name@ - The name of the AMI (provided during image creation).
    --
    -- -   @owner-alias@ - The owner alias (@amazon@ | @aws-marketplace@). The
    --     valid aliases are defined in an Amazon-maintained list. This is not
    --     the Amazon Web Services account alias that can be set using the IAM
    --     console. We recommend that you use the __Owner__ request parameter
    --     instead of this filter.
    --
    -- -   @owner-id@ - The Amazon Web Services account ID of the owner. We
    --     recommend that you use the __Owner__ request parameter instead of
    --     this filter.
    --
    -- -   @platform@ - The platform. The only supported value is @windows@.
    --
    -- -   @product-code@ - The product code.
    --
    -- -   @product-code.type@ - The type of the product code (@marketplace@).
    --
    -- -   @ramdisk-id@ - The RAM disk ID.
    --
    -- -   @root-device-name@ - The device name of the root device volume (for
    --     example, @\/dev\/sda1@).
    --
    -- -   @root-device-type@ - The type of the root device volume (@ebs@ |
    --     @instance-store@).
    --
    -- -   @state@ - The state of the image (@available@ | @pending@ |
    --     @failed@).
    --
    -- -   @state-reason-code@ - The reason code for the state change.
    --
    -- -   @state-reason-message@ - The message for the state change.
    --
    -- -   @sriov-net-support@ - A value of @simple@ indicates that enhanced
    --     networking with the Intel 82599 VF interface is enabled.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @virtualization-type@ - The virtualization type (@paravirtual@ |
    --     @hvm@).
    filters :: Prelude.Maybe [Filter],
    -- | Scopes the results to images with the specified owners. You can specify
    -- a combination of Amazon Web Services account IDs, @self@, @amazon@, and
    -- @aws-marketplace@. If you omit this parameter, the results include all
    -- images for which you have launch permissions, regardless of ownership.
    owners :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to include deprecated AMIs.
    --
    -- Default: No deprecated AMIs are included in the response.
    --
    -- If you are the AMI owner, all deprecated AMIs appear in the response
    -- regardless of what you specify for this parameter.
    includeDeprecated :: Prelude.Maybe Prelude.Bool,
    -- | Scopes the images by users with explicit launch permissions. Specify an
    -- Amazon Web Services account ID, @self@ (the sender of the request), or
    -- @all@ (public AMIs).
    --
    -- -   If you specify an Amazon Web Services account ID that is not your
    --     own, only AMIs shared with that specific Amazon Web Services account
    --     ID are returned. However, AMIs that are shared with the account’s
    --     organization or organizational unit (OU) are not returned.
    --
    -- -   If you specify @self@ or your own Amazon Web Services account ID,
    --     AMIs shared with your account are returned. In addition, AMIs that
    --     are shared with the organization or OU of which you are member are
    --     also returned.
    --
    -- -   If you specify @all@, all public AMIs are returned.
    executableUsers :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageIds', 'describeImages_imageIds' - The image IDs.
--
-- Default: Describes all images available to you.
--
-- 'filters', 'describeImages_filters' - The filters.
--
-- -   @architecture@ - The image architecture (@i386@ | @x86_64@ |
--     @arm64@).
--
-- -   @block-device-mapping.delete-on-termination@ - A Boolean value that
--     indicates whether the Amazon EBS volume is deleted on instance
--     termination.
--
-- -   @block-device-mapping.device-name@ - The device name specified in
--     the block device mapping (for example, @\/dev\/sdh@ or @xvdh@).
--
-- -   @block-device-mapping.snapshot-id@ - The ID of the snapshot used for
--     the Amazon EBS volume.
--
-- -   @block-device-mapping.volume-size@ - The volume size of the Amazon
--     EBS volume, in GiB.
--
-- -   @block-device-mapping.volume-type@ - The volume type of the Amazon
--     EBS volume (@io1@ | @io2@ | @gp2@ | @gp3@ | @sc1 @| @st1@ |
--     @standard@).
--
-- -   @block-device-mapping.encrypted@ - A Boolean that indicates whether
--     the Amazon EBS volume is encrypted.
--
-- -   @creation-date@ - The time when the image was created, in the ISO
--     8601 format in the UTC time zone (YYYY-MM-DDThh:mm:ss.sssZ), for
--     example, @2021-09-29T11:04:43.305Z@. You can use a wildcard (@*@),
--     for example, @2021-09-29T*@, which matches an entire day.
--
-- -   @description@ - The description of the image (provided during image
--     creation).
--
-- -   @ena-support@ - A Boolean that indicates whether enhanced networking
--     with ENA is enabled.
--
-- -   @hypervisor@ - The hypervisor type (@ovm@ | @xen@).
--
-- -   @image-id@ - The ID of the image.
--
-- -   @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@).
--
-- -   @is-public@ - A Boolean that indicates whether the image is public.
--
-- -   @kernel-id@ - The kernel ID.
--
-- -   @manifest-location@ - The location of the image manifest.
--
-- -   @name@ - The name of the AMI (provided during image creation).
--
-- -   @owner-alias@ - The owner alias (@amazon@ | @aws-marketplace@). The
--     valid aliases are defined in an Amazon-maintained list. This is not
--     the Amazon Web Services account alias that can be set using the IAM
--     console. We recommend that you use the __Owner__ request parameter
--     instead of this filter.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the owner. We
--     recommend that you use the __Owner__ request parameter instead of
--     this filter.
--
-- -   @platform@ - The platform. The only supported value is @windows@.
--
-- -   @product-code@ - The product code.
--
-- -   @product-code.type@ - The type of the product code (@marketplace@).
--
-- -   @ramdisk-id@ - The RAM disk ID.
--
-- -   @root-device-name@ - The device name of the root device volume (for
--     example, @\/dev\/sda1@).
--
-- -   @root-device-type@ - The type of the root device volume (@ebs@ |
--     @instance-store@).
--
-- -   @state@ - The state of the image (@available@ | @pending@ |
--     @failed@).
--
-- -   @state-reason-code@ - The reason code for the state change.
--
-- -   @state-reason-message@ - The message for the state change.
--
-- -   @sriov-net-support@ - A value of @simple@ indicates that enhanced
--     networking with the Intel 82599 VF interface is enabled.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @virtualization-type@ - The virtualization type (@paravirtual@ |
--     @hvm@).
--
-- 'owners', 'describeImages_owners' - Scopes the results to images with the specified owners. You can specify
-- a combination of Amazon Web Services account IDs, @self@, @amazon@, and
-- @aws-marketplace@. If you omit this parameter, the results include all
-- images for which you have launch permissions, regardless of ownership.
--
-- 'dryRun', 'describeImages_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'includeDeprecated', 'describeImages_includeDeprecated' - Specifies whether to include deprecated AMIs.
--
-- Default: No deprecated AMIs are included in the response.
--
-- If you are the AMI owner, all deprecated AMIs appear in the response
-- regardless of what you specify for this parameter.
--
-- 'executableUsers', 'describeImages_executableUsers' - Scopes the images by users with explicit launch permissions. Specify an
-- Amazon Web Services account ID, @self@ (the sender of the request), or
-- @all@ (public AMIs).
--
-- -   If you specify an Amazon Web Services account ID that is not your
--     own, only AMIs shared with that specific Amazon Web Services account
--     ID are returned. However, AMIs that are shared with the account’s
--     organization or organizational unit (OU) are not returned.
--
-- -   If you specify @self@ or your own Amazon Web Services account ID,
--     AMIs shared with your account are returned. In addition, AMIs that
--     are shared with the organization or OU of which you are member are
--     also returned.
--
-- -   If you specify @all@, all public AMIs are returned.
newDescribeImages ::
  DescribeImages
newDescribeImages =
  DescribeImages'
    { imageIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      owners = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      includeDeprecated = Prelude.Nothing,
      executableUsers = Prelude.Nothing
    }

-- | The image IDs.
--
-- Default: Describes all images available to you.
describeImages_imageIds :: Lens.Lens' DescribeImages (Prelude.Maybe [Prelude.Text])
describeImages_imageIds = Lens.lens (\DescribeImages' {imageIds} -> imageIds) (\s@DescribeImages' {} a -> s {imageIds = a} :: DescribeImages) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
--
-- -   @architecture@ - The image architecture (@i386@ | @x86_64@ |
--     @arm64@).
--
-- -   @block-device-mapping.delete-on-termination@ - A Boolean value that
--     indicates whether the Amazon EBS volume is deleted on instance
--     termination.
--
-- -   @block-device-mapping.device-name@ - The device name specified in
--     the block device mapping (for example, @\/dev\/sdh@ or @xvdh@).
--
-- -   @block-device-mapping.snapshot-id@ - The ID of the snapshot used for
--     the Amazon EBS volume.
--
-- -   @block-device-mapping.volume-size@ - The volume size of the Amazon
--     EBS volume, in GiB.
--
-- -   @block-device-mapping.volume-type@ - The volume type of the Amazon
--     EBS volume (@io1@ | @io2@ | @gp2@ | @gp3@ | @sc1 @| @st1@ |
--     @standard@).
--
-- -   @block-device-mapping.encrypted@ - A Boolean that indicates whether
--     the Amazon EBS volume is encrypted.
--
-- -   @creation-date@ - The time when the image was created, in the ISO
--     8601 format in the UTC time zone (YYYY-MM-DDThh:mm:ss.sssZ), for
--     example, @2021-09-29T11:04:43.305Z@. You can use a wildcard (@*@),
--     for example, @2021-09-29T*@, which matches an entire day.
--
-- -   @description@ - The description of the image (provided during image
--     creation).
--
-- -   @ena-support@ - A Boolean that indicates whether enhanced networking
--     with ENA is enabled.
--
-- -   @hypervisor@ - The hypervisor type (@ovm@ | @xen@).
--
-- -   @image-id@ - The ID of the image.
--
-- -   @image-type@ - The image type (@machine@ | @kernel@ | @ramdisk@).
--
-- -   @is-public@ - A Boolean that indicates whether the image is public.
--
-- -   @kernel-id@ - The kernel ID.
--
-- -   @manifest-location@ - The location of the image manifest.
--
-- -   @name@ - The name of the AMI (provided during image creation).
--
-- -   @owner-alias@ - The owner alias (@amazon@ | @aws-marketplace@). The
--     valid aliases are defined in an Amazon-maintained list. This is not
--     the Amazon Web Services account alias that can be set using the IAM
--     console. We recommend that you use the __Owner__ request parameter
--     instead of this filter.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the owner. We
--     recommend that you use the __Owner__ request parameter instead of
--     this filter.
--
-- -   @platform@ - The platform. The only supported value is @windows@.
--
-- -   @product-code@ - The product code.
--
-- -   @product-code.type@ - The type of the product code (@marketplace@).
--
-- -   @ramdisk-id@ - The RAM disk ID.
--
-- -   @root-device-name@ - The device name of the root device volume (for
--     example, @\/dev\/sda1@).
--
-- -   @root-device-type@ - The type of the root device volume (@ebs@ |
--     @instance-store@).
--
-- -   @state@ - The state of the image (@available@ | @pending@ |
--     @failed@).
--
-- -   @state-reason-code@ - The reason code for the state change.
--
-- -   @state-reason-message@ - The message for the state change.
--
-- -   @sriov-net-support@ - A value of @simple@ indicates that enhanced
--     networking with the Intel 82599 VF interface is enabled.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @virtualization-type@ - The virtualization type (@paravirtual@ |
--     @hvm@).
describeImages_filters :: Lens.Lens' DescribeImages (Prelude.Maybe [Filter])
describeImages_filters = Lens.lens (\DescribeImages' {filters} -> filters) (\s@DescribeImages' {} a -> s {filters = a} :: DescribeImages) Prelude.. Lens.mapping Lens.coerced

-- | Scopes the results to images with the specified owners. You can specify
-- a combination of Amazon Web Services account IDs, @self@, @amazon@, and
-- @aws-marketplace@. If you omit this parameter, the results include all
-- images for which you have launch permissions, regardless of ownership.
describeImages_owners :: Lens.Lens' DescribeImages (Prelude.Maybe [Prelude.Text])
describeImages_owners = Lens.lens (\DescribeImages' {owners} -> owners) (\s@DescribeImages' {} a -> s {owners = a} :: DescribeImages) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeImages_dryRun :: Lens.Lens' DescribeImages (Prelude.Maybe Prelude.Bool)
describeImages_dryRun = Lens.lens (\DescribeImages' {dryRun} -> dryRun) (\s@DescribeImages' {} a -> s {dryRun = a} :: DescribeImages)

-- | Specifies whether to include deprecated AMIs.
--
-- Default: No deprecated AMIs are included in the response.
--
-- If you are the AMI owner, all deprecated AMIs appear in the response
-- regardless of what you specify for this parameter.
describeImages_includeDeprecated :: Lens.Lens' DescribeImages (Prelude.Maybe Prelude.Bool)
describeImages_includeDeprecated = Lens.lens (\DescribeImages' {includeDeprecated} -> includeDeprecated) (\s@DescribeImages' {} a -> s {includeDeprecated = a} :: DescribeImages)

-- | Scopes the images by users with explicit launch permissions. Specify an
-- Amazon Web Services account ID, @self@ (the sender of the request), or
-- @all@ (public AMIs).
--
-- -   If you specify an Amazon Web Services account ID that is not your
--     own, only AMIs shared with that specific Amazon Web Services account
--     ID are returned. However, AMIs that are shared with the account’s
--     organization or organizational unit (OU) are not returned.
--
-- -   If you specify @self@ or your own Amazon Web Services account ID,
--     AMIs shared with your account are returned. In addition, AMIs that
--     are shared with the organization or OU of which you are member are
--     also returned.
--
-- -   If you specify @all@, all public AMIs are returned.
describeImages_executableUsers :: Lens.Lens' DescribeImages (Prelude.Maybe [Prelude.Text])
describeImages_executableUsers = Lens.lens (\DescribeImages' {executableUsers} -> executableUsers) (\s@DescribeImages' {} a -> s {executableUsers = a} :: DescribeImages) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeImages where
  type
    AWSResponse DescribeImages =
      DescribeImagesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeImagesResponse'
            Prelude.<$> ( x Data..@? "imagesSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImages where
  hashWithSalt _salt DescribeImages' {..} =
    _salt `Prelude.hashWithSalt` imageIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` owners
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` includeDeprecated
      `Prelude.hashWithSalt` executableUsers

instance Prelude.NFData DescribeImages where
  rnf DescribeImages' {..} =
    Prelude.rnf imageIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf owners
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf includeDeprecated
      `Prelude.seq` Prelude.rnf executableUsers

instance Data.ToHeaders DescribeImages where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeImages where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImages where
  toQuery DescribeImages' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeImages" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        Data.toQuery
          (Data.toQueryList "ImageId" Prelude.<$> imageIds),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          (Data.toQueryList "Owner" Prelude.<$> owners),
        "DryRun" Data.=: dryRun,
        "IncludeDeprecated" Data.=: includeDeprecated,
        Data.toQuery
          ( Data.toQueryList "ExecutableBy"
              Prelude.<$> executableUsers
          )
      ]

-- | /See:/ 'newDescribeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { -- | Information about the images.
    images :: Prelude.Maybe [Image],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImagesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'images', 'describeImagesResponse_images' - Information about the images.
--
-- 'httpStatus', 'describeImagesResponse_httpStatus' - The response's http status code.
newDescribeImagesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImagesResponse
newDescribeImagesResponse pHttpStatus_ =
  DescribeImagesResponse'
    { images = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the images.
describeImagesResponse_images :: Lens.Lens' DescribeImagesResponse (Prelude.Maybe [Image])
describeImagesResponse_images = Lens.lens (\DescribeImagesResponse' {images} -> images) (\s@DescribeImagesResponse' {} a -> s {images = a} :: DescribeImagesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeImagesResponse_httpStatus :: Lens.Lens' DescribeImagesResponse Prelude.Int
describeImagesResponse_httpStatus = Lens.lens (\DescribeImagesResponse' {httpStatus} -> httpStatus) (\s@DescribeImagesResponse' {} a -> s {httpStatus = a} :: DescribeImagesResponse)

instance Prelude.NFData DescribeImagesResponse where
  rnf DescribeImagesResponse' {..} =
    Prelude.rnf images
      `Prelude.seq` Prelude.rnf httpStatus
