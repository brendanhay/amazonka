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
-- Module      : Network.AWS.EC2.CreateImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that
-- is either running or stopped.
--
-- If you customized your instance with instance store volumes or EBS
-- volumes in addition to the root device volume, the new AMI contains
-- block device mapping information for those volumes. When you launch an
-- instance from this new AMI, the instance automatically launches with
-- those additional volumes.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Creating Amazon EBS-Backed Linux AMIs>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateImage
  ( -- * Creating a Request
    CreateImage (..),
    newCreateImage,

    -- * Request Lenses
    createImage_tagSpecifications,
    createImage_dryRun,
    createImage_blockDeviceMappings,
    createImage_description,
    createImage_noReboot,
    createImage_instanceId,
    createImage_name,

    -- * Destructuring the Response
    CreateImageResponse (..),
    newCreateImageResponse,

    -- * Response Lenses
    createImageResponse_imageId,
    createImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateImage' smart constructor.
data CreateImage = CreateImage'
  { -- | The tags to apply to the AMI and snapshots on creation. You can tag the
    -- AMI, the snapshots, or both.
    --
    -- -   To tag the AMI, the value for @ResourceType@ must be @image@.
    --
    -- -   To tag the snapshots that are created of the root volume and of
    --     other EBS volumes that are attached to the instance, the value for
    --     @ResourceType@ must be @snapshot@. The same tag is applied to all of
    --     the snapshots that are created.
    --
    -- If you specify other values for @ResourceType@, the request fails.
    --
    -- To tag an AMI or snapshot after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The block device mappings. This parameter cannot be used to modify the
    -- encryption status of existing volumes or snapshots. To create an AMI
    -- with encrypted snapshots, use the CopyImage action.
    blockDeviceMappings :: Core.Maybe [BlockDeviceMapping],
    -- | A description for the new image.
    description :: Core.Maybe Core.Text,
    -- | By default, Amazon EC2 attempts to shut down and reboot the instance
    -- before creating the image. If the @No Reboot@ option is set, Amazon EC2
    -- doesn\'t shut down the instance before creating the image. When this
    -- option is used, file system integrity on the created image can\'t be
    -- guaranteed.
    noReboot :: Core.Maybe Core.Bool,
    -- | The ID of the instance.
    instanceId :: Core.Text,
    -- | A name for the new image.
    --
    -- Constraints: 3-128 alphanumeric characters, parentheses (()), square
    -- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
    -- quotes (\'), at-signs (\@), or underscores(_)
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createImage_tagSpecifications' - The tags to apply to the AMI and snapshots on creation. You can tag the
-- AMI, the snapshots, or both.
--
-- -   To tag the AMI, the value for @ResourceType@ must be @image@.
--
-- -   To tag the snapshots that are created of the root volume and of
--     other EBS volumes that are attached to the instance, the value for
--     @ResourceType@ must be @snapshot@. The same tag is applied to all of
--     the snapshots that are created.
--
-- If you specify other values for @ResourceType@, the request fails.
--
-- To tag an AMI or snapshot after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'dryRun', 'createImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'blockDeviceMappings', 'createImage_blockDeviceMappings' - The block device mappings. This parameter cannot be used to modify the
-- encryption status of existing volumes or snapshots. To create an AMI
-- with encrypted snapshots, use the CopyImage action.
--
-- 'description', 'createImage_description' - A description for the new image.
--
-- 'noReboot', 'createImage_noReboot' - By default, Amazon EC2 attempts to shut down and reboot the instance
-- before creating the image. If the @No Reboot@ option is set, Amazon EC2
-- doesn\'t shut down the instance before creating the image. When this
-- option is used, file system integrity on the created image can\'t be
-- guaranteed.
--
-- 'instanceId', 'createImage_instanceId' - The ID of the instance.
--
-- 'name', 'createImage_name' - A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
newCreateImage ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  CreateImage
newCreateImage pInstanceId_ pName_ =
  CreateImage'
    { tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      blockDeviceMappings = Core.Nothing,
      description = Core.Nothing,
      noReboot = Core.Nothing,
      instanceId = pInstanceId_,
      name = pName_
    }

-- | The tags to apply to the AMI and snapshots on creation. You can tag the
-- AMI, the snapshots, or both.
--
-- -   To tag the AMI, the value for @ResourceType@ must be @image@.
--
-- -   To tag the snapshots that are created of the root volume and of
--     other EBS volumes that are attached to the instance, the value for
--     @ResourceType@ must be @snapshot@. The same tag is applied to all of
--     the snapshots that are created.
--
-- If you specify other values for @ResourceType@, the request fails.
--
-- To tag an AMI or snapshot after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
createImage_tagSpecifications :: Lens.Lens' CreateImage (Core.Maybe [TagSpecification])
createImage_tagSpecifications = Lens.lens (\CreateImage' {tagSpecifications} -> tagSpecifications) (\s@CreateImage' {} a -> s {tagSpecifications = a} :: CreateImage) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createImage_dryRun :: Lens.Lens' CreateImage (Core.Maybe Core.Bool)
createImage_dryRun = Lens.lens (\CreateImage' {dryRun} -> dryRun) (\s@CreateImage' {} a -> s {dryRun = a} :: CreateImage)

-- | The block device mappings. This parameter cannot be used to modify the
-- encryption status of existing volumes or snapshots. To create an AMI
-- with encrypted snapshots, use the CopyImage action.
createImage_blockDeviceMappings :: Lens.Lens' CreateImage (Core.Maybe [BlockDeviceMapping])
createImage_blockDeviceMappings = Lens.lens (\CreateImage' {blockDeviceMappings} -> blockDeviceMappings) (\s@CreateImage' {} a -> s {blockDeviceMappings = a} :: CreateImage) Core.. Lens.mapping Lens._Coerce

-- | A description for the new image.
createImage_description :: Lens.Lens' CreateImage (Core.Maybe Core.Text)
createImage_description = Lens.lens (\CreateImage' {description} -> description) (\s@CreateImage' {} a -> s {description = a} :: CreateImage)

-- | By default, Amazon EC2 attempts to shut down and reboot the instance
-- before creating the image. If the @No Reboot@ option is set, Amazon EC2
-- doesn\'t shut down the instance before creating the image. When this
-- option is used, file system integrity on the created image can\'t be
-- guaranteed.
createImage_noReboot :: Lens.Lens' CreateImage (Core.Maybe Core.Bool)
createImage_noReboot = Lens.lens (\CreateImage' {noReboot} -> noReboot) (\s@CreateImage' {} a -> s {noReboot = a} :: CreateImage)

-- | The ID of the instance.
createImage_instanceId :: Lens.Lens' CreateImage Core.Text
createImage_instanceId = Lens.lens (\CreateImage' {instanceId} -> instanceId) (\s@CreateImage' {} a -> s {instanceId = a} :: CreateImage)

-- | A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
createImage_name :: Lens.Lens' CreateImage Core.Text
createImage_name = Lens.lens (\CreateImage' {name} -> name) (\s@CreateImage' {} a -> s {name = a} :: CreateImage)

instance Core.AWSRequest CreateImage where
  type AWSResponse CreateImage = CreateImageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateImageResponse'
            Core.<$> (x Core..@? "imageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateImage

instance Core.NFData CreateImage

instance Core.ToHeaders CreateImage where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateImage where
  toPath = Core.const "/"

instance Core.ToQuery CreateImage where
  toQuery CreateImage' {..} =
    Core.mconcat
      [ "Action" Core.=: ("CreateImage" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Core.<$> blockDeviceMappings
          ),
        "Description" Core.=: description,
        "NoReboot" Core.=: noReboot,
        "InstanceId" Core.=: instanceId,
        "Name" Core.=: name
      ]

-- | /See:/ 'newCreateImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { -- | The ID of the new AMI.
    imageId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageId', 'createImageResponse_imageId' - The ID of the new AMI.
--
-- 'httpStatus', 'createImageResponse_httpStatus' - The response's http status code.
newCreateImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateImageResponse
newCreateImageResponse pHttpStatus_ =
  CreateImageResponse'
    { imageId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the new AMI.
createImageResponse_imageId :: Lens.Lens' CreateImageResponse (Core.Maybe Core.Text)
createImageResponse_imageId = Lens.lens (\CreateImageResponse' {imageId} -> imageId) (\s@CreateImageResponse' {} a -> s {imageId = a} :: CreateImageResponse)

-- | The response's http status code.
createImageResponse_httpStatus :: Lens.Lens' CreateImageResponse Core.Int
createImageResponse_httpStatus = Lens.lens (\CreateImageResponse' {httpStatus} -> httpStatus) (\s@CreateImageResponse' {} a -> s {httpStatus = a} :: CreateImageResponse)

instance Core.NFData CreateImageResponse
