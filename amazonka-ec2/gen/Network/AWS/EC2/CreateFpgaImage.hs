{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateFpgaImage' smart constructor.
data CreateFpgaImage = CreateFpgaImage'
  { -- | The tags to apply to the FPGA image during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A name for the AFI.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description for the AFI.
    description :: Prelude.Maybe Prelude.Text,
    -- | The location in Amazon S3 for the output logs.
    logsStorageLocation :: Prelude.Maybe StorageLocation,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The location of the encrypted design checkpoint in Amazon S3. The input
    -- must be a tarball.
    inputStorageLocation :: StorageLocation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { tagSpecifications =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      logsStorageLocation = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      inputStorageLocation = pInputStorageLocation_
    }

-- | The tags to apply to the FPGA image during creation.
createFpgaImage_tagSpecifications :: Lens.Lens' CreateFpgaImage (Prelude.Maybe [TagSpecification])
createFpgaImage_tagSpecifications = Lens.lens (\CreateFpgaImage' {tagSpecifications} -> tagSpecifications) (\s@CreateFpgaImage' {} a -> s {tagSpecifications = a} :: CreateFpgaImage) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createFpgaImage_dryRun :: Lens.Lens' CreateFpgaImage (Prelude.Maybe Prelude.Bool)
createFpgaImage_dryRun = Lens.lens (\CreateFpgaImage' {dryRun} -> dryRun) (\s@CreateFpgaImage' {} a -> s {dryRun = a} :: CreateFpgaImage)

-- | A name for the AFI.
createFpgaImage_name :: Lens.Lens' CreateFpgaImage (Prelude.Maybe Prelude.Text)
createFpgaImage_name = Lens.lens (\CreateFpgaImage' {name} -> name) (\s@CreateFpgaImage' {} a -> s {name = a} :: CreateFpgaImage)

-- | A description for the AFI.
createFpgaImage_description :: Lens.Lens' CreateFpgaImage (Prelude.Maybe Prelude.Text)
createFpgaImage_description = Lens.lens (\CreateFpgaImage' {description} -> description) (\s@CreateFpgaImage' {} a -> s {description = a} :: CreateFpgaImage)

-- | The location in Amazon S3 for the output logs.
createFpgaImage_logsStorageLocation :: Lens.Lens' CreateFpgaImage (Prelude.Maybe StorageLocation)
createFpgaImage_logsStorageLocation = Lens.lens (\CreateFpgaImage' {logsStorageLocation} -> logsStorageLocation) (\s@CreateFpgaImage' {} a -> s {logsStorageLocation = a} :: CreateFpgaImage)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring Idempotency>.
createFpgaImage_clientToken :: Lens.Lens' CreateFpgaImage (Prelude.Maybe Prelude.Text)
createFpgaImage_clientToken = Lens.lens (\CreateFpgaImage' {clientToken} -> clientToken) (\s@CreateFpgaImage' {} a -> s {clientToken = a} :: CreateFpgaImage)

-- | The location of the encrypted design checkpoint in Amazon S3. The input
-- must be a tarball.
createFpgaImage_inputStorageLocation :: Lens.Lens' CreateFpgaImage StorageLocation
createFpgaImage_inputStorageLocation = Lens.lens (\CreateFpgaImage' {inputStorageLocation} -> inputStorageLocation) (\s@CreateFpgaImage' {} a -> s {inputStorageLocation = a} :: CreateFpgaImage)

instance Prelude.AWSRequest CreateFpgaImage where
  type Rs CreateFpgaImage = CreateFpgaImageResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateFpgaImageResponse'
            Prelude.<$> (x Prelude..@? "fpgaImageGlobalId")
            Prelude.<*> (x Prelude..@? "fpgaImageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFpgaImage

instance Prelude.NFData CreateFpgaImage

instance Prelude.ToHeaders CreateFpgaImage where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateFpgaImage where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateFpgaImage where
  toQuery CreateFpgaImage' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateFpgaImage" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "Name" Prelude.=: name,
        "Description" Prelude.=: description,
        "LogsStorageLocation" Prelude.=: logsStorageLocation,
        "ClientToken" Prelude.=: clientToken,
        "InputStorageLocation"
          Prelude.=: inputStorageLocation
      ]

-- | /See:/ 'newCreateFpgaImageResponse' smart constructor.
data CreateFpgaImageResponse = CreateFpgaImageResponse'
  { -- | The global FPGA image identifier (AGFI ID).
    fpgaImageGlobalId :: Prelude.Maybe Prelude.Text,
    -- | The FPGA image identifier (AFI ID).
    fpgaImageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateFpgaImageResponse
newCreateFpgaImageResponse pHttpStatus_ =
  CreateFpgaImageResponse'
    { fpgaImageGlobalId =
        Prelude.Nothing,
      fpgaImageId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The global FPGA image identifier (AGFI ID).
createFpgaImageResponse_fpgaImageGlobalId :: Lens.Lens' CreateFpgaImageResponse (Prelude.Maybe Prelude.Text)
createFpgaImageResponse_fpgaImageGlobalId = Lens.lens (\CreateFpgaImageResponse' {fpgaImageGlobalId} -> fpgaImageGlobalId) (\s@CreateFpgaImageResponse' {} a -> s {fpgaImageGlobalId = a} :: CreateFpgaImageResponse)

-- | The FPGA image identifier (AFI ID).
createFpgaImageResponse_fpgaImageId :: Lens.Lens' CreateFpgaImageResponse (Prelude.Maybe Prelude.Text)
createFpgaImageResponse_fpgaImageId = Lens.lens (\CreateFpgaImageResponse' {fpgaImageId} -> fpgaImageId) (\s@CreateFpgaImageResponse' {} a -> s {fpgaImageId = a} :: CreateFpgaImageResponse)

-- | The response's http status code.
createFpgaImageResponse_httpStatus :: Lens.Lens' CreateFpgaImageResponse Prelude.Int
createFpgaImageResponse_httpStatus = Lens.lens (\CreateFpgaImageResponse' {httpStatus} -> httpStatus) (\s@CreateFpgaImageResponse' {} a -> s {httpStatus = a} :: CreateFpgaImageResponse)

instance Prelude.NFData CreateFpgaImageResponse
