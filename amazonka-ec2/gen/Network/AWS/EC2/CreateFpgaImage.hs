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
-- Module      : Network.AWS.EC2.CreateFpgaImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon FPGA Image (AFI) from the specified design checkpoint
-- (DCP).
--
-- The create operation is asynchronous. To verify that the AFI is ready
-- for use, check the output logs.
--
-- An AFI contains the FPGA bitstream that is ready to download to an FPGA.
-- You can securely deploy an AFI on multiple FPGA-accelerated instances.
-- For more information, see the
-- <https://github.com/aws/aws-fpga/ AWS FPGA Hardware Development Kit>.
module Network.AWS.EC2.CreateFpgaImage
  ( -- * Creating a Request
    CreateFpgaImage (..),
    newCreateFpgaImage,

    -- * Request Lenses
    createFpgaImage_tagSpecifications,
    createFpgaImage_dryRun,
    createFpgaImage_name,
    createFpgaImage_description,
    createFpgaImage_logsStorageLocation,
    createFpgaImage_clientToken,
    createFpgaImage_inputStorageLocation,

    -- * Destructuring the Response
    CreateFpgaImageResponse (..),
    newCreateFpgaImageResponse,

    -- * Response Lenses
    createFpgaImageResponse_fpgaImageGlobalId,
    createFpgaImageResponse_fpgaImageId,
    createFpgaImageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFpgaImage' smart constructor.
data CreateFpgaImage = CreateFpgaImage'
  { -- | The tags to apply to the FPGA image during creation.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | A name for the AFI.
    name :: Core.Maybe Core.Text,
    -- | A description for the AFI.
    description :: Core.Maybe Core.Text,
    -- | The location in Amazon S3 for the output logs.
    logsStorageLocation :: Core.Maybe StorageLocation,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text,
    -- | The location of the encrypted design checkpoint in Amazon S3. The input
    -- must be a tarball.
    inputStorageLocation :: StorageLocation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFpgaImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createFpgaImage_tagSpecifications' - The tags to apply to the FPGA image during creation.
--
-- 'dryRun', 'createFpgaImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'name', 'createFpgaImage_name' - A name for the AFI.
--
-- 'description', 'createFpgaImage_description' - A description for the AFI.
--
-- 'logsStorageLocation', 'createFpgaImage_logsStorageLocation' - The location in Amazon S3 for the output logs.
--
-- 'clientToken', 'createFpgaImage_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'inputStorageLocation', 'createFpgaImage_inputStorageLocation' - The location of the encrypted design checkpoint in Amazon S3. The input
-- must be a tarball.
newCreateFpgaImage ::
  -- | 'inputStorageLocation'
  StorageLocation ->
  CreateFpgaImage
newCreateFpgaImage pInputStorageLocation_ =
  CreateFpgaImage'
    { tagSpecifications = Core.Nothing,
      dryRun = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      logsStorageLocation = Core.Nothing,
      clientToken = Core.Nothing,
      inputStorageLocation = pInputStorageLocation_
    }

-- | The tags to apply to the FPGA image during creation.
createFpgaImage_tagSpecifications :: Lens.Lens' CreateFpgaImage (Core.Maybe [TagSpecification])
createFpgaImage_tagSpecifications = Lens.lens (\CreateFpgaImage' {tagSpecifications} -> tagSpecifications) (\s@CreateFpgaImage' {} a -> s {tagSpecifications = a} :: CreateFpgaImage) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createFpgaImage_dryRun :: Lens.Lens' CreateFpgaImage (Core.Maybe Core.Bool)
createFpgaImage_dryRun = Lens.lens (\CreateFpgaImage' {dryRun} -> dryRun) (\s@CreateFpgaImage' {} a -> s {dryRun = a} :: CreateFpgaImage)

-- | A name for the AFI.
createFpgaImage_name :: Lens.Lens' CreateFpgaImage (Core.Maybe Core.Text)
createFpgaImage_name = Lens.lens (\CreateFpgaImage' {name} -> name) (\s@CreateFpgaImage' {} a -> s {name = a} :: CreateFpgaImage)

-- | A description for the AFI.
createFpgaImage_description :: Lens.Lens' CreateFpgaImage (Core.Maybe Core.Text)
createFpgaImage_description = Lens.lens (\CreateFpgaImage' {description} -> description) (\s@CreateFpgaImage' {} a -> s {description = a} :: CreateFpgaImage)

-- | The location in Amazon S3 for the output logs.
createFpgaImage_logsStorageLocation :: Lens.Lens' CreateFpgaImage (Core.Maybe StorageLocation)
createFpgaImage_logsStorageLocation = Lens.lens (\CreateFpgaImage' {logsStorageLocation} -> logsStorageLocation) (\s@CreateFpgaImage' {} a -> s {logsStorageLocation = a} :: CreateFpgaImage)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency>.
createFpgaImage_clientToken :: Lens.Lens' CreateFpgaImage (Core.Maybe Core.Text)
createFpgaImage_clientToken = Lens.lens (\CreateFpgaImage' {clientToken} -> clientToken) (\s@CreateFpgaImage' {} a -> s {clientToken = a} :: CreateFpgaImage)

-- | The location of the encrypted design checkpoint in Amazon S3. The input
-- must be a tarball.
createFpgaImage_inputStorageLocation :: Lens.Lens' CreateFpgaImage StorageLocation
createFpgaImage_inputStorageLocation = Lens.lens (\CreateFpgaImage' {inputStorageLocation} -> inputStorageLocation) (\s@CreateFpgaImage' {} a -> s {inputStorageLocation = a} :: CreateFpgaImage)

instance Core.AWSRequest CreateFpgaImage where
  type
    AWSResponse CreateFpgaImage =
      CreateFpgaImageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFpgaImageResponse'
            Core.<$> (x Core..@? "fpgaImageGlobalId")
            Core.<*> (x Core..@? "fpgaImageId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateFpgaImage

instance Core.NFData CreateFpgaImage

instance Core.ToHeaders CreateFpgaImage where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateFpgaImage where
  toPath = Core.const "/"

instance Core.ToQuery CreateFpgaImage where
  toQuery CreateFpgaImage' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateFpgaImage" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun,
        "Name" Core.=: name,
        "Description" Core.=: description,
        "LogsStorageLocation" Core.=: logsStorageLocation,
        "ClientToken" Core.=: clientToken,
        "InputStorageLocation" Core.=: inputStorageLocation
      ]

-- | /See:/ 'newCreateFpgaImageResponse' smart constructor.
data CreateFpgaImageResponse = CreateFpgaImageResponse'
  { -- | The global FPGA image identifier (AGFI ID).
    fpgaImageGlobalId :: Core.Maybe Core.Text,
    -- | The FPGA image identifier (AFI ID).
    fpgaImageId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateFpgaImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fpgaImageGlobalId', 'createFpgaImageResponse_fpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
--
-- 'fpgaImageId', 'createFpgaImageResponse_fpgaImageId' - The FPGA image identifier (AFI ID).
--
-- 'httpStatus', 'createFpgaImageResponse_httpStatus' - The response's http status code.
newCreateFpgaImageResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateFpgaImageResponse
newCreateFpgaImageResponse pHttpStatus_ =
  CreateFpgaImageResponse'
    { fpgaImageGlobalId =
        Core.Nothing,
      fpgaImageId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The global FPGA image identifier (AGFI ID).
createFpgaImageResponse_fpgaImageGlobalId :: Lens.Lens' CreateFpgaImageResponse (Core.Maybe Core.Text)
createFpgaImageResponse_fpgaImageGlobalId = Lens.lens (\CreateFpgaImageResponse' {fpgaImageGlobalId} -> fpgaImageGlobalId) (\s@CreateFpgaImageResponse' {} a -> s {fpgaImageGlobalId = a} :: CreateFpgaImageResponse)

-- | The FPGA image identifier (AFI ID).
createFpgaImageResponse_fpgaImageId :: Lens.Lens' CreateFpgaImageResponse (Core.Maybe Core.Text)
createFpgaImageResponse_fpgaImageId = Lens.lens (\CreateFpgaImageResponse' {fpgaImageId} -> fpgaImageId) (\s@CreateFpgaImageResponse' {} a -> s {fpgaImageId = a} :: CreateFpgaImageResponse)

-- | The response's http status code.
createFpgaImageResponse_httpStatus :: Lens.Lens' CreateFpgaImageResponse Core.Int
createFpgaImageResponse_httpStatus = Lens.lens (\CreateFpgaImageResponse' {httpStatus} -> httpStatus) (\s@CreateFpgaImageResponse' {} a -> s {httpStatus = a} :: CreateFpgaImageResponse)

instance Core.NFData CreateFpgaImageResponse
