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
-- Module      : Amazonka.EC2.CreateImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon EBS-backed AMI from an Amazon EBS-backed instance that
-- is either running or stopped.
--
-- By default, when Amazon EC2 creates the new AMI, it reboots the instance
-- so that it can take snapshots of the attached volumes while data is at
-- rest, in order to ensure a consistent state. You can set the @NoReboot@
-- parameter to @true@ in the API request, or use the @--no-reboot@ option
-- in the CLI to prevent Amazon EC2 from shutting down and rebooting the
-- instance.
--
-- If you choose to bypass the shutdown and reboot process by setting the
-- @NoReboot@ parameter to @true@ in the API request, or by using the
-- @--no-reboot@ option in the CLI, we can\'t guarantee the file system
-- integrity of the created image.
--
-- If you customized your instance with instance store volumes or Amazon
-- EBS volumes in addition to the root device volume, the new AMI contains
-- block device mapping information for those volumes. When you launch an
-- instance from this new AMI, the instance automatically launches with
-- those additional volumes.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/creating-an-ami-ebs.html Create an Amazon EBS-backed Linux AMI>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateImage
  ( -- * Creating a Request
    CreateImage (..),
    newCreateImage,

    -- * Request Lenses
    createImage_blockDeviceMappings,
    createImage_description,
    createImage_dryRun,
    createImage_tagSpecifications,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateImage' smart constructor.
data CreateImage = CreateImage'
  { -- | The block device mappings. This parameter cannot be used to modify the
    -- encryption status of existing volumes or snapshots. To create an AMI
    -- with encrypted snapshots, use the CopyImage action.
    blockDeviceMappings :: Prelude.Maybe [BlockDeviceMapping],
    -- | A description for the new image.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The tags to apply to the AMI and snapshots on creation. You can tag the
    -- AMI, the snapshots, or both.
    --
    -- -   To tag the AMI, the value for @ResourceType@ must be @image@.
    --
    -- -   To tag the snapshots that are created of the root volume and of
    --     other Amazon EBS volumes that are attached to the instance, the
    --     value for @ResourceType@ must be @snapshot@. The same tag is applied
    --     to all of the snapshots that are created.
    --
    -- If you specify other values for @ResourceType@, the request fails.
    --
    -- To tag an AMI or snapshot after it has been created, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | By default, when Amazon EC2 creates the new AMI, it reboots the instance
    -- so that it can take snapshots of the attached volumes while data is at
    -- rest, in order to ensure a consistent state. You can set the @NoReboot@
    -- parameter to @true@ in the API request, or use the @--no-reboot@ option
    -- in the CLI to prevent Amazon EC2 from shutting down and rebooting the
    -- instance.
    --
    -- If you choose to bypass the shutdown and reboot process by setting the
    -- @NoReboot@ parameter to @true@ in the API request, or by using the
    -- @--no-reboot@ option in the CLI, we can\'t guarantee the file system
    -- integrity of the created image.
    --
    -- Default: @false@ (follow standard reboot process)
    noReboot :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the instance.
    instanceId :: Prelude.Text,
    -- | A name for the new image.
    --
    -- Constraints: 3-128 alphanumeric characters, parentheses (()), square
    -- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
    -- quotes (\'), at-signs (\@), or underscores(_)
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDeviceMappings', 'createImage_blockDeviceMappings' - The block device mappings. This parameter cannot be used to modify the
-- encryption status of existing volumes or snapshots. To create an AMI
-- with encrypted snapshots, use the CopyImage action.
--
-- 'description', 'createImage_description' - A description for the new image.
--
-- 'dryRun', 'createImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createImage_tagSpecifications' - The tags to apply to the AMI and snapshots on creation. You can tag the
-- AMI, the snapshots, or both.
--
-- -   To tag the AMI, the value for @ResourceType@ must be @image@.
--
-- -   To tag the snapshots that are created of the root volume and of
--     other Amazon EBS volumes that are attached to the instance, the
--     value for @ResourceType@ must be @snapshot@. The same tag is applied
--     to all of the snapshots that are created.
--
-- If you specify other values for @ResourceType@, the request fails.
--
-- To tag an AMI or snapshot after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
--
-- 'noReboot', 'createImage_noReboot' - By default, when Amazon EC2 creates the new AMI, it reboots the instance
-- so that it can take snapshots of the attached volumes while data is at
-- rest, in order to ensure a consistent state. You can set the @NoReboot@
-- parameter to @true@ in the API request, or use the @--no-reboot@ option
-- in the CLI to prevent Amazon EC2 from shutting down and rebooting the
-- instance.
--
-- If you choose to bypass the shutdown and reboot process by setting the
-- @NoReboot@ parameter to @true@ in the API request, or by using the
-- @--no-reboot@ option in the CLI, we can\'t guarantee the file system
-- integrity of the created image.
--
-- Default: @false@ (follow standard reboot process)
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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateImage
newCreateImage pInstanceId_ pName_ =
  CreateImage'
    { blockDeviceMappings = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      noReboot = Prelude.Nothing,
      instanceId = pInstanceId_,
      name = pName_
    }

-- | The block device mappings. This parameter cannot be used to modify the
-- encryption status of existing volumes or snapshots. To create an AMI
-- with encrypted snapshots, use the CopyImage action.
createImage_blockDeviceMappings :: Lens.Lens' CreateImage (Prelude.Maybe [BlockDeviceMapping])
createImage_blockDeviceMappings = Lens.lens (\CreateImage' {blockDeviceMappings} -> blockDeviceMappings) (\s@CreateImage' {} a -> s {blockDeviceMappings = a} :: CreateImage) Prelude.. Lens.mapping Lens.coerced

-- | A description for the new image.
createImage_description :: Lens.Lens' CreateImage (Prelude.Maybe Prelude.Text)
createImage_description = Lens.lens (\CreateImage' {description} -> description) (\s@CreateImage' {} a -> s {description = a} :: CreateImage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createImage_dryRun :: Lens.Lens' CreateImage (Prelude.Maybe Prelude.Bool)
createImage_dryRun = Lens.lens (\CreateImage' {dryRun} -> dryRun) (\s@CreateImage' {} a -> s {dryRun = a} :: CreateImage)

-- | The tags to apply to the AMI and snapshots on creation. You can tag the
-- AMI, the snapshots, or both.
--
-- -   To tag the AMI, the value for @ResourceType@ must be @image@.
--
-- -   To tag the snapshots that are created of the root volume and of
--     other Amazon EBS volumes that are attached to the instance, the
--     value for @ResourceType@ must be @snapshot@. The same tag is applied
--     to all of the snapshots that are created.
--
-- If you specify other values for @ResourceType@, the request fails.
--
-- To tag an AMI or snapshot after it has been created, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateTags.html CreateTags>.
createImage_tagSpecifications :: Lens.Lens' CreateImage (Prelude.Maybe [TagSpecification])
createImage_tagSpecifications = Lens.lens (\CreateImage' {tagSpecifications} -> tagSpecifications) (\s@CreateImage' {} a -> s {tagSpecifications = a} :: CreateImage) Prelude.. Lens.mapping Lens.coerced

-- | By default, when Amazon EC2 creates the new AMI, it reboots the instance
-- so that it can take snapshots of the attached volumes while data is at
-- rest, in order to ensure a consistent state. You can set the @NoReboot@
-- parameter to @true@ in the API request, or use the @--no-reboot@ option
-- in the CLI to prevent Amazon EC2 from shutting down and rebooting the
-- instance.
--
-- If you choose to bypass the shutdown and reboot process by setting the
-- @NoReboot@ parameter to @true@ in the API request, or by using the
-- @--no-reboot@ option in the CLI, we can\'t guarantee the file system
-- integrity of the created image.
--
-- Default: @false@ (follow standard reboot process)
createImage_noReboot :: Lens.Lens' CreateImage (Prelude.Maybe Prelude.Bool)
createImage_noReboot = Lens.lens (\CreateImage' {noReboot} -> noReboot) (\s@CreateImage' {} a -> s {noReboot = a} :: CreateImage)

-- | The ID of the instance.
createImage_instanceId :: Lens.Lens' CreateImage Prelude.Text
createImage_instanceId = Lens.lens (\CreateImage' {instanceId} -> instanceId) (\s@CreateImage' {} a -> s {instanceId = a} :: CreateImage)

-- | A name for the new image.
--
-- Constraints: 3-128 alphanumeric characters, parentheses (()), square
-- brackets ([]), spaces ( ), periods (.), slashes (\/), dashes (-), single
-- quotes (\'), at-signs (\@), or underscores(_)
createImage_name :: Lens.Lens' CreateImage Prelude.Text
createImage_name = Lens.lens (\CreateImage' {name} -> name) (\s@CreateImage' {} a -> s {name = a} :: CreateImage)

instance Core.AWSRequest CreateImage where
  type AWSResponse CreateImage = CreateImageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateImageResponse'
            Prelude.<$> (x Core..@? "imageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImage where
  hashWithSalt _salt CreateImage' {..} =
    _salt `Prelude.hashWithSalt` blockDeviceMappings
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` noReboot
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateImage where
  rnf CreateImage' {..} =
    Prelude.rnf blockDeviceMappings
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf noReboot
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateImage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateImage where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateImage where
  toQuery CreateImage' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateImage" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          ( Core.toQueryList "BlockDeviceMapping"
              Prelude.<$> blockDeviceMappings
          ),
        "Description" Core.=: description,
        "DryRun" Core.=: dryRun,
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "NoReboot" Core.=: noReboot,
        "InstanceId" Core.=: instanceId,
        "Name" Core.=: name
      ]

-- | /See:/ 'newCreateImageResponse' smart constructor.
data CreateImageResponse = CreateImageResponse'
  { -- | The ID of the new AMI.
    imageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateImageResponse
newCreateImageResponse pHttpStatus_ =
  CreateImageResponse'
    { imageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the new AMI.
createImageResponse_imageId :: Lens.Lens' CreateImageResponse (Prelude.Maybe Prelude.Text)
createImageResponse_imageId = Lens.lens (\CreateImageResponse' {imageId} -> imageId) (\s@CreateImageResponse' {} a -> s {imageId = a} :: CreateImageResponse)

-- | The response's http status code.
createImageResponse_httpStatus :: Lens.Lens' CreateImageResponse Prelude.Int
createImageResponse_httpStatus = Lens.lens (\CreateImageResponse' {httpStatus} -> httpStatus) (\s@CreateImageResponse' {} a -> s {httpStatus = a} :: CreateImageResponse)

instance Prelude.NFData CreateImageResponse where
  rnf CreateImageResponse' {..} =
    Prelude.rnf imageId
      `Prelude.seq` Prelude.rnf httpStatus
