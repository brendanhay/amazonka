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
-- Module      : Amazonka.EC2.CopyFpgaImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified Amazon FPGA Image (AFI) to the current Region.
module Amazonka.EC2.CopyFpgaImage
  ( -- * Creating a Request
    CopyFpgaImage (..),
    newCopyFpgaImage,

    -- * Request Lenses
    copyFpgaImage_clientToken,
    copyFpgaImage_description,
    copyFpgaImage_dryRun,
    copyFpgaImage_name,
    copyFpgaImage_sourceFpgaImageId,
    copyFpgaImage_sourceRegion,

    -- * Destructuring the Response
    CopyFpgaImageResponse (..),
    newCopyFpgaImageResponse,

    -- * Response Lenses
    copyFpgaImageResponse_fpgaImageId,
    copyFpgaImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCopyFpgaImage' smart constructor.
data CopyFpgaImage = CopyFpgaImage'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description for the new AFI.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The name for the new AFI. The default is the name of the source AFI.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the source AFI.
    sourceFpgaImageId :: Prelude.Text,
    -- | The Region that contains the source AFI.
    sourceRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyFpgaImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'copyFpgaImage_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- 'description', 'copyFpgaImage_description' - The description for the new AFI.
--
-- 'dryRun', 'copyFpgaImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'name', 'copyFpgaImage_name' - The name for the new AFI. The default is the name of the source AFI.
--
-- 'sourceFpgaImageId', 'copyFpgaImage_sourceFpgaImageId' - The ID of the source AFI.
--
-- 'sourceRegion', 'copyFpgaImage_sourceRegion' - The Region that contains the source AFI.
newCopyFpgaImage ::
  -- | 'sourceFpgaImageId'
  Prelude.Text ->
  -- | 'sourceRegion'
  Prelude.Text ->
  CopyFpgaImage
newCopyFpgaImage pSourceFpgaImageId_ pSourceRegion_ =
  CopyFpgaImage'
    { clientToken = Prelude.Nothing,
      description = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceFpgaImageId = pSourceFpgaImageId_,
      sourceRegion = pSourceRegion_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html Ensuring idempotency>.
copyFpgaImage_clientToken :: Lens.Lens' CopyFpgaImage (Prelude.Maybe Prelude.Text)
copyFpgaImage_clientToken = Lens.lens (\CopyFpgaImage' {clientToken} -> clientToken) (\s@CopyFpgaImage' {} a -> s {clientToken = a} :: CopyFpgaImage)

-- | The description for the new AFI.
copyFpgaImage_description :: Lens.Lens' CopyFpgaImage (Prelude.Maybe Prelude.Text)
copyFpgaImage_description = Lens.lens (\CopyFpgaImage' {description} -> description) (\s@CopyFpgaImage' {} a -> s {description = a} :: CopyFpgaImage)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
copyFpgaImage_dryRun :: Lens.Lens' CopyFpgaImage (Prelude.Maybe Prelude.Bool)
copyFpgaImage_dryRun = Lens.lens (\CopyFpgaImage' {dryRun} -> dryRun) (\s@CopyFpgaImage' {} a -> s {dryRun = a} :: CopyFpgaImage)

-- | The name for the new AFI. The default is the name of the source AFI.
copyFpgaImage_name :: Lens.Lens' CopyFpgaImage (Prelude.Maybe Prelude.Text)
copyFpgaImage_name = Lens.lens (\CopyFpgaImage' {name} -> name) (\s@CopyFpgaImage' {} a -> s {name = a} :: CopyFpgaImage)

-- | The ID of the source AFI.
copyFpgaImage_sourceFpgaImageId :: Lens.Lens' CopyFpgaImage Prelude.Text
copyFpgaImage_sourceFpgaImageId = Lens.lens (\CopyFpgaImage' {sourceFpgaImageId} -> sourceFpgaImageId) (\s@CopyFpgaImage' {} a -> s {sourceFpgaImageId = a} :: CopyFpgaImage)

-- | The Region that contains the source AFI.
copyFpgaImage_sourceRegion :: Lens.Lens' CopyFpgaImage Prelude.Text
copyFpgaImage_sourceRegion = Lens.lens (\CopyFpgaImage' {sourceRegion} -> sourceRegion) (\s@CopyFpgaImage' {} a -> s {sourceRegion = a} :: CopyFpgaImage)

instance Core.AWSRequest CopyFpgaImage where
  type
    AWSResponse CopyFpgaImage =
      CopyFpgaImageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CopyFpgaImageResponse'
            Prelude.<$> (x Data..@? "fpgaImageId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopyFpgaImage where
  hashWithSalt _salt CopyFpgaImage' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sourceFpgaImageId
      `Prelude.hashWithSalt` sourceRegion

instance Prelude.NFData CopyFpgaImage where
  rnf CopyFpgaImage' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sourceFpgaImageId
      `Prelude.seq` Prelude.rnf sourceRegion

instance Data.ToHeaders CopyFpgaImage where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CopyFpgaImage where
  toPath = Prelude.const "/"

instance Data.ToQuery CopyFpgaImage where
  toQuery CopyFpgaImage' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CopyFpgaImage" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "Name" Data.=: name,
        "SourceFpgaImageId" Data.=: sourceFpgaImageId,
        "SourceRegion" Data.=: sourceRegion
      ]

-- | /See:/ 'newCopyFpgaImageResponse' smart constructor.
data CopyFpgaImageResponse = CopyFpgaImageResponse'
  { -- | The ID of the new AFI.
    fpgaImageId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CopyFpgaImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fpgaImageId', 'copyFpgaImageResponse_fpgaImageId' - The ID of the new AFI.
--
-- 'httpStatus', 'copyFpgaImageResponse_httpStatus' - The response's http status code.
newCopyFpgaImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CopyFpgaImageResponse
newCopyFpgaImageResponse pHttpStatus_ =
  CopyFpgaImageResponse'
    { fpgaImageId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the new AFI.
copyFpgaImageResponse_fpgaImageId :: Lens.Lens' CopyFpgaImageResponse (Prelude.Maybe Prelude.Text)
copyFpgaImageResponse_fpgaImageId = Lens.lens (\CopyFpgaImageResponse' {fpgaImageId} -> fpgaImageId) (\s@CopyFpgaImageResponse' {} a -> s {fpgaImageId = a} :: CopyFpgaImageResponse)

-- | The response's http status code.
copyFpgaImageResponse_httpStatus :: Lens.Lens' CopyFpgaImageResponse Prelude.Int
copyFpgaImageResponse_httpStatus = Lens.lens (\CopyFpgaImageResponse' {httpStatus} -> httpStatus) (\s@CopyFpgaImageResponse' {} a -> s {httpStatus = a} :: CopyFpgaImageResponse)

instance Prelude.NFData CopyFpgaImageResponse where
  rnf CopyFpgaImageResponse' {..} =
    Prelude.rnf fpgaImageId
      `Prelude.seq` Prelude.rnf httpStatus
