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
-- Module      : Amazonka.EC2.CancelImageLaunchPermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes your Amazon Web Services account from the launch permissions for
-- the specified AMI. For more information, see
-- <https://docs.aws.amazon.com/ Cancel having an AMI shared with your Amazon Web Services account>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CancelImageLaunchPermission
  ( -- * Creating a Request
    CancelImageLaunchPermission (..),
    newCancelImageLaunchPermission,

    -- * Request Lenses
    cancelImageLaunchPermission_dryRun,
    cancelImageLaunchPermission_imageId,

    -- * Destructuring the Response
    CancelImageLaunchPermissionResponse (..),
    newCancelImageLaunchPermissionResponse,

    -- * Response Lenses
    cancelImageLaunchPermissionResponse_return,
    cancelImageLaunchPermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelImageLaunchPermission' smart constructor.
data CancelImageLaunchPermission = CancelImageLaunchPermission'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the AMI that was shared with your Amazon Web Services account.
    imageId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelImageLaunchPermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelImageLaunchPermission_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'imageId', 'cancelImageLaunchPermission_imageId' - The ID of the AMI that was shared with your Amazon Web Services account.
newCancelImageLaunchPermission ::
  -- | 'imageId'
  Prelude.Text ->
  CancelImageLaunchPermission
newCancelImageLaunchPermission pImageId_ =
  CancelImageLaunchPermission'
    { dryRun =
        Prelude.Nothing,
      imageId = pImageId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelImageLaunchPermission_dryRun :: Lens.Lens' CancelImageLaunchPermission (Prelude.Maybe Prelude.Bool)
cancelImageLaunchPermission_dryRun = Lens.lens (\CancelImageLaunchPermission' {dryRun} -> dryRun) (\s@CancelImageLaunchPermission' {} a -> s {dryRun = a} :: CancelImageLaunchPermission)

-- | The ID of the AMI that was shared with your Amazon Web Services account.
cancelImageLaunchPermission_imageId :: Lens.Lens' CancelImageLaunchPermission Prelude.Text
cancelImageLaunchPermission_imageId = Lens.lens (\CancelImageLaunchPermission' {imageId} -> imageId) (\s@CancelImageLaunchPermission' {} a -> s {imageId = a} :: CancelImageLaunchPermission)

instance Core.AWSRequest CancelImageLaunchPermission where
  type
    AWSResponse CancelImageLaunchPermission =
      CancelImageLaunchPermissionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CancelImageLaunchPermissionResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelImageLaunchPermission where
  hashWithSalt _salt CancelImageLaunchPermission' {..} =
    _salt `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` imageId

instance Prelude.NFData CancelImageLaunchPermission where
  rnf CancelImageLaunchPermission' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf imageId

instance Data.ToHeaders CancelImageLaunchPermission where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CancelImageLaunchPermission where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelImageLaunchPermission where
  toQuery CancelImageLaunchPermission' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CancelImageLaunchPermission" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "ImageId" Data.=: imageId
      ]

-- | /See:/ 'newCancelImageLaunchPermissionResponse' smart constructor.
data CancelImageLaunchPermissionResponse = CancelImageLaunchPermissionResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelImageLaunchPermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'cancelImageLaunchPermissionResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'cancelImageLaunchPermissionResponse_httpStatus' - The response's http status code.
newCancelImageLaunchPermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelImageLaunchPermissionResponse
newCancelImageLaunchPermissionResponse pHttpStatus_ =
  CancelImageLaunchPermissionResponse'
    { return' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
cancelImageLaunchPermissionResponse_return :: Lens.Lens' CancelImageLaunchPermissionResponse (Prelude.Maybe Prelude.Bool)
cancelImageLaunchPermissionResponse_return = Lens.lens (\CancelImageLaunchPermissionResponse' {return'} -> return') (\s@CancelImageLaunchPermissionResponse' {} a -> s {return' = a} :: CancelImageLaunchPermissionResponse)

-- | The response's http status code.
cancelImageLaunchPermissionResponse_httpStatus :: Lens.Lens' CancelImageLaunchPermissionResponse Prelude.Int
cancelImageLaunchPermissionResponse_httpStatus = Lens.lens (\CancelImageLaunchPermissionResponse' {httpStatus} -> httpStatus) (\s@CancelImageLaunchPermissionResponse' {} a -> s {httpStatus = a} :: CancelImageLaunchPermissionResponse)

instance
  Prelude.NFData
    CancelImageLaunchPermissionResponse
  where
  rnf CancelImageLaunchPermissionResponse' {..} =
    Prelude.rnf return'
      `Prelude.seq` Prelude.rnf httpStatus
