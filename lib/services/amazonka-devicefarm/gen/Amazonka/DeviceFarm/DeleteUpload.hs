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
-- Module      : Amazonka.DeviceFarm.DeleteUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an upload given the upload ARN.
module Amazonka.DeviceFarm.DeleteUpload
  ( -- * Creating a Request
    DeleteUpload (..),
    newDeleteUpload,

    -- * Request Lenses
    deleteUpload_arn,

    -- * Destructuring the Response
    DeleteUploadResponse (..),
    newDeleteUploadResponse,

    -- * Response Lenses
    deleteUploadResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the delete upload operation.
--
-- /See:/ 'newDeleteUpload' smart constructor.
data DeleteUpload = DeleteUpload'
  { -- | Represents the Amazon Resource Name (ARN) of the Device Farm upload to
    -- delete.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteUpload_arn' - Represents the Amazon Resource Name (ARN) of the Device Farm upload to
-- delete.
newDeleteUpload ::
  -- | 'arn'
  Prelude.Text ->
  DeleteUpload
newDeleteUpload pArn_ = DeleteUpload' {arn = pArn_}

-- | Represents the Amazon Resource Name (ARN) of the Device Farm upload to
-- delete.
deleteUpload_arn :: Lens.Lens' DeleteUpload Prelude.Text
deleteUpload_arn = Lens.lens (\DeleteUpload' {arn} -> arn) (\s@DeleteUpload' {} a -> s {arn = a} :: DeleteUpload)

instance Core.AWSRequest DeleteUpload where
  type AWSResponse DeleteUpload = DeleteUploadResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteUploadResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteUpload where
  hashWithSalt _salt DeleteUpload' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteUpload where
  rnf DeleteUpload' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteUpload where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.DeleteUpload" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteUpload where
  toJSON DeleteUpload' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteUpload where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUpload where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a delete upload request.
--
-- /See:/ 'newDeleteUploadResponse' smart constructor.
data DeleteUploadResponse = DeleteUploadResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteUploadResponse_httpStatus' - The response's http status code.
newDeleteUploadResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteUploadResponse
newDeleteUploadResponse pHttpStatus_ =
  DeleteUploadResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteUploadResponse_httpStatus :: Lens.Lens' DeleteUploadResponse Prelude.Int
deleteUploadResponse_httpStatus = Lens.lens (\DeleteUploadResponse' {httpStatus} -> httpStatus) (\s@DeleteUploadResponse' {} a -> s {httpStatus = a} :: DeleteUploadResponse)

instance Prelude.NFData DeleteUploadResponse where
  rnf DeleteUploadResponse' {..} =
    Prelude.rnf httpStatus
