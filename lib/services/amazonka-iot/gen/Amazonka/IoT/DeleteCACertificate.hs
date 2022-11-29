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
-- Module      : Amazonka.IoT.DeleteCACertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a registered CA certificate.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteCACertificate>
-- action.
module Amazonka.IoT.DeleteCACertificate
  ( -- * Creating a Request
    DeleteCACertificate (..),
    newDeleteCACertificate,

    -- * Request Lenses
    deleteCACertificate_certificateId,

    -- * Destructuring the Response
    DeleteCACertificateResponse (..),
    newDeleteCACertificateResponse,

    -- * Response Lenses
    deleteCACertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input for the DeleteCACertificate operation.
--
-- /See:/ 'newDeleteCACertificate' smart constructor.
data DeleteCACertificate = DeleteCACertificate'
  { -- | The ID of the certificate to delete. (The last part of the certificate
    -- ARN contains the certificate ID.)
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCACertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'deleteCACertificate_certificateId' - The ID of the certificate to delete. (The last part of the certificate
-- ARN contains the certificate ID.)
newDeleteCACertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  DeleteCACertificate
newDeleteCACertificate pCertificateId_ =
  DeleteCACertificate'
    { certificateId =
        pCertificateId_
    }

-- | The ID of the certificate to delete. (The last part of the certificate
-- ARN contains the certificate ID.)
deleteCACertificate_certificateId :: Lens.Lens' DeleteCACertificate Prelude.Text
deleteCACertificate_certificateId = Lens.lens (\DeleteCACertificate' {certificateId} -> certificateId) (\s@DeleteCACertificate' {} a -> s {certificateId = a} :: DeleteCACertificate)

instance Core.AWSRequest DeleteCACertificate where
  type
    AWSResponse DeleteCACertificate =
      DeleteCACertificateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCACertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCACertificate where
  hashWithSalt _salt DeleteCACertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateId

instance Prelude.NFData DeleteCACertificate where
  rnf DeleteCACertificate' {..} =
    Prelude.rnf certificateId

instance Core.ToHeaders DeleteCACertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteCACertificate where
  toPath DeleteCACertificate' {..} =
    Prelude.mconcat
      ["/cacertificate/", Core.toBS certificateId]

instance Core.ToQuery DeleteCACertificate where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DeleteCACertificate operation.
--
-- /See:/ 'newDeleteCACertificateResponse' smart constructor.
data DeleteCACertificateResponse = DeleteCACertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCACertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCACertificateResponse_httpStatus' - The response's http status code.
newDeleteCACertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCACertificateResponse
newDeleteCACertificateResponse pHttpStatus_ =
  DeleteCACertificateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCACertificateResponse_httpStatus :: Lens.Lens' DeleteCACertificateResponse Prelude.Int
deleteCACertificateResponse_httpStatus = Lens.lens (\DeleteCACertificateResponse' {httpStatus} -> httpStatus) (\s@DeleteCACertificateResponse' {} a -> s {httpStatus = a} :: DeleteCACertificateResponse)

instance Prelude.NFData DeleteCACertificateResponse where
  rnf DeleteCACertificateResponse' {..} =
    Prelude.rnf httpStatus
