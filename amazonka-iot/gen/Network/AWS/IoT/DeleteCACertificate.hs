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
-- Module      : Network.AWS.IoT.DeleteCACertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a registered CA certificate.
module Network.AWS.IoT.DeleteCACertificate
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for the DeleteCACertificate operation.
--
-- /See:/ 'newDeleteCACertificate' smart constructor.
data DeleteCACertificate = DeleteCACertificate'
  { -- | The ID of the certificate to delete. (The last part of the certificate
    -- ARN contains the certificate ID.)
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCACertificate where
  type
    Rs DeleteCACertificate =
      DeleteCACertificateResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCACertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCACertificate

instance Prelude.NFData DeleteCACertificate

instance Prelude.ToHeaders DeleteCACertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteCACertificate where
  toPath DeleteCACertificate' {..} =
    Prelude.mconcat
      ["/cacertificate/", Prelude.toBS certificateId]

instance Prelude.ToQuery DeleteCACertificate where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DeleteCACertificate operation.
--
-- /See:/ 'newDeleteCACertificateResponse' smart constructor.
data DeleteCACertificateResponse = DeleteCACertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteCACertificateResponse
