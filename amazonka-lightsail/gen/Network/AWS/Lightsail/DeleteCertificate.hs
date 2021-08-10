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
-- Module      : Network.AWS.Lightsail.DeleteCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL\/TLS certificate for your Amazon Lightsail content
-- delivery network (CDN) distribution.
--
-- Certificates that are currently attached to a distribution cannot be
-- deleted. Use the @DetachCertificateFromDistribution@ action to detach a
-- certificate from a distribution.
module Network.AWS.Lightsail.DeleteCertificate
  ( -- * Creating a Request
    DeleteCertificate (..),
    newDeleteCertificate,

    -- * Request Lenses
    deleteCertificate_certificateName,

    -- * Destructuring the Response
    DeleteCertificateResponse (..),
    newDeleteCertificateResponse,

    -- * Response Lenses
    deleteCertificateResponse_operations,
    deleteCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCertificate' smart constructor.
data DeleteCertificate = DeleteCertificate'
  { -- | The name of the certificate to delete.
    --
    -- Use the @GetCertificates@ action to get a list of certificate names that
    -- you can specify.
    certificateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateName', 'deleteCertificate_certificateName' - The name of the certificate to delete.
--
-- Use the @GetCertificates@ action to get a list of certificate names that
-- you can specify.
newDeleteCertificate ::
  -- | 'certificateName'
  Prelude.Text ->
  DeleteCertificate
newDeleteCertificate pCertificateName_ =
  DeleteCertificate'
    { certificateName =
        pCertificateName_
    }

-- | The name of the certificate to delete.
--
-- Use the @GetCertificates@ action to get a list of certificate names that
-- you can specify.
deleteCertificate_certificateName :: Lens.Lens' DeleteCertificate Prelude.Text
deleteCertificate_certificateName = Lens.lens (\DeleteCertificate' {certificateName} -> certificateName) (\s@DeleteCertificate' {} a -> s {certificateName = a} :: DeleteCertificate)

instance Core.AWSRequest DeleteCertificate where
  type
    AWSResponse DeleteCertificate =
      DeleteCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteCertificateResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCertificate

instance Prelude.NFData DeleteCertificate

instance Core.ToHeaders DeleteCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.DeleteCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteCertificate where
  toJSON DeleteCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("certificateName" Core..= certificateName)
          ]
      )

instance Core.ToPath DeleteCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'deleteCertificateResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'deleteCertificateResponse_httpStatus' - The response's http status code.
newDeleteCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCertificateResponse
newDeleteCertificateResponse pHttpStatus_ =
  DeleteCertificateResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
deleteCertificateResponse_operations :: Lens.Lens' DeleteCertificateResponse (Prelude.Maybe [Operation])
deleteCertificateResponse_operations = Lens.lens (\DeleteCertificateResponse' {operations} -> operations) (\s@DeleteCertificateResponse' {} a -> s {operations = a} :: DeleteCertificateResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteCertificateResponse_httpStatus :: Lens.Lens' DeleteCertificateResponse Prelude.Int
deleteCertificateResponse_httpStatus = Lens.lens (\DeleteCertificateResponse' {httpStatus} -> httpStatus) (\s@DeleteCertificateResponse' {} a -> s {httpStatus = a} :: DeleteCertificateResponse)

instance Prelude.NFData DeleteCertificateResponse
