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
-- Module      : Amazonka.EC2.DeleteFpgaImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon FPGA Image (AFI).
module Amazonka.EC2.DeleteFpgaImage
  ( -- * Creating a Request
    DeleteFpgaImage (..),
    newDeleteFpgaImage,

    -- * Request Lenses
    deleteFpgaImage_dryRun,
    deleteFpgaImage_fpgaImageId,

    -- * Destructuring the Response
    DeleteFpgaImageResponse (..),
    newDeleteFpgaImageResponse,

    -- * Response Lenses
    deleteFpgaImageResponse_return,
    deleteFpgaImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteFpgaImage' smart constructor.
data DeleteFpgaImage = DeleteFpgaImage'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AFI.
    fpgaImageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFpgaImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteFpgaImage_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'fpgaImageId', 'deleteFpgaImage_fpgaImageId' - The ID of the AFI.
newDeleteFpgaImage ::
  -- | 'fpgaImageId'
  Prelude.Text ->
  DeleteFpgaImage
newDeleteFpgaImage pFpgaImageId_ =
  DeleteFpgaImage'
    { dryRun = Prelude.Nothing,
      fpgaImageId = pFpgaImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteFpgaImage_dryRun :: Lens.Lens' DeleteFpgaImage (Prelude.Maybe Prelude.Bool)
deleteFpgaImage_dryRun = Lens.lens (\DeleteFpgaImage' {dryRun} -> dryRun) (\s@DeleteFpgaImage' {} a -> s {dryRun = a} :: DeleteFpgaImage)

-- | The ID of the AFI.
deleteFpgaImage_fpgaImageId :: Lens.Lens' DeleteFpgaImage Prelude.Text
deleteFpgaImage_fpgaImageId = Lens.lens (\DeleteFpgaImage' {fpgaImageId} -> fpgaImageId) (\s@DeleteFpgaImage' {} a -> s {fpgaImageId = a} :: DeleteFpgaImage)

instance Core.AWSRequest DeleteFpgaImage where
  type
    AWSResponse DeleteFpgaImage =
      DeleteFpgaImageResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteFpgaImageResponse'
            Prelude.<$> (x Core..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteFpgaImage where
  hashWithSalt _salt DeleteFpgaImage' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` fpgaImageId

instance Prelude.NFData DeleteFpgaImage where
  rnf DeleteFpgaImage' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf fpgaImageId

instance Core.ToHeaders DeleteFpgaImage where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteFpgaImage where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteFpgaImage where
  toQuery DeleteFpgaImage' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteFpgaImage" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "FpgaImageId" Core.=: fpgaImageId
      ]

-- | /See:/ 'newDeleteFpgaImageResponse' smart constructor.
data DeleteFpgaImageResponse = DeleteFpgaImageResponse'
  { -- | Is @true@ if the request succeeds, and an error otherwise.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteFpgaImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'deleteFpgaImageResponse_return' - Is @true@ if the request succeeds, and an error otherwise.
--
-- 'httpStatus', 'deleteFpgaImageResponse_httpStatus' - The response's http status code.
newDeleteFpgaImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteFpgaImageResponse
newDeleteFpgaImageResponse pHttpStatus_ =
  DeleteFpgaImageResponse'
    { return' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Is @true@ if the request succeeds, and an error otherwise.
deleteFpgaImageResponse_return :: Lens.Lens' DeleteFpgaImageResponse (Prelude.Maybe Prelude.Bool)
deleteFpgaImageResponse_return = Lens.lens (\DeleteFpgaImageResponse' {return'} -> return') (\s@DeleteFpgaImageResponse' {} a -> s {return' = a} :: DeleteFpgaImageResponse)

-- | The response's http status code.
deleteFpgaImageResponse_httpStatus :: Lens.Lens' DeleteFpgaImageResponse Prelude.Int
deleteFpgaImageResponse_httpStatus = Lens.lens (\DeleteFpgaImageResponse' {httpStatus} -> httpStatus) (\s@DeleteFpgaImageResponse' {} a -> s {httpStatus = a} :: DeleteFpgaImageResponse)

instance Prelude.NFData DeleteFpgaImageResponse where
  rnf DeleteFpgaImageResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
