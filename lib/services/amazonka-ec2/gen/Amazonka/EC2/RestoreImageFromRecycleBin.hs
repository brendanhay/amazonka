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
-- Module      : Amazonka.EC2.RestoreImageFromRecycleBin
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an AMI from the Recycle Bin. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/recycle-bin.html Recycle Bin>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.RestoreImageFromRecycleBin
  ( -- * Creating a Request
    RestoreImageFromRecycleBin (..),
    newRestoreImageFromRecycleBin,

    -- * Request Lenses
    restoreImageFromRecycleBin_dryRun,
    restoreImageFromRecycleBin_imageId,

    -- * Destructuring the Response
    RestoreImageFromRecycleBinResponse (..),
    newRestoreImageFromRecycleBinResponse,

    -- * Response Lenses
    restoreImageFromRecycleBinResponse_return,
    restoreImageFromRecycleBinResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreImageFromRecycleBin' smart constructor.
data RestoreImageFromRecycleBin = RestoreImageFromRecycleBin'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AMI to restore.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreImageFromRecycleBin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'restoreImageFromRecycleBin_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'imageId', 'restoreImageFromRecycleBin_imageId' - The ID of the AMI to restore.
newRestoreImageFromRecycleBin ::
  -- | 'imageId'
  Prelude.Text ->
  RestoreImageFromRecycleBin
newRestoreImageFromRecycleBin pImageId_ =
  RestoreImageFromRecycleBin'
    { dryRun =
        Prelude.Nothing,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
restoreImageFromRecycleBin_dryRun :: Lens.Lens' RestoreImageFromRecycleBin (Prelude.Maybe Prelude.Bool)
restoreImageFromRecycleBin_dryRun = Lens.lens (\RestoreImageFromRecycleBin' {dryRun} -> dryRun) (\s@RestoreImageFromRecycleBin' {} a -> s {dryRun = a} :: RestoreImageFromRecycleBin)

-- | The ID of the AMI to restore.
restoreImageFromRecycleBin_imageId :: Lens.Lens' RestoreImageFromRecycleBin Prelude.Text
restoreImageFromRecycleBin_imageId = Lens.lens (\RestoreImageFromRecycleBin' {imageId} -> imageId) (\s@RestoreImageFromRecycleBin' {} a -> s {imageId = a} :: RestoreImageFromRecycleBin)

instance Core.AWSRequest RestoreImageFromRecycleBin where
  type
    AWSResponse RestoreImageFromRecycleBin =
      RestoreImageFromRecycleBinResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          RestoreImageFromRecycleBinResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreImageFromRecycleBin where
  hashWithSalt _salt RestoreImageFromRecycleBin' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData RestoreImageFromRecycleBin where
  rnf RestoreImageFromRecycleBin' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders RestoreImageFromRecycleBin where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreImageFromRecycleBin where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreImageFromRecycleBin where
  toQuery RestoreImageFromRecycleBin' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RestoreImageFromRecycleBin" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ImageId" Data.=: imageId
      ]

-- | /See:/ 'newRestoreImageFromRecycleBinResponse' smart constructor.
data RestoreImageFromRecycleBinResponse = RestoreImageFromRecycleBinResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreImageFromRecycleBinResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'restoreImageFromRecycleBinResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'restoreImageFromRecycleBinResponse_httpStatus' - The response's http status code.
newRestoreImageFromRecycleBinResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreImageFromRecycleBinResponse
newRestoreImageFromRecycleBinResponse pHttpStatus_ =
  RestoreImageFromRecycleBinResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
restoreImageFromRecycleBinResponse_return :: Lens.Lens' RestoreImageFromRecycleBinResponse (Prelude.Maybe Prelude.Bool)
restoreImageFromRecycleBinResponse_return = Lens.lens (\RestoreImageFromRecycleBinResponse' {return'} -> return') (\s@RestoreImageFromRecycleBinResponse' {} a -> s {return' = a} :: RestoreImageFromRecycleBinResponse)

-- | The response's http status code.
restoreImageFromRecycleBinResponse_httpStatus :: Lens.Lens' RestoreImageFromRecycleBinResponse Prelude.Int
restoreImageFromRecycleBinResponse_httpStatus = Lens.lens (\RestoreImageFromRecycleBinResponse' {httpStatus} -> httpStatus) (\s@RestoreImageFromRecycleBinResponse' {} a -> s {httpStatus = a} :: RestoreImageFromRecycleBinResponse)

instance
  Prelude.NFData
    RestoreImageFromRecycleBinResponse
  where
  rnf RestoreImageFromRecycleBinResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
