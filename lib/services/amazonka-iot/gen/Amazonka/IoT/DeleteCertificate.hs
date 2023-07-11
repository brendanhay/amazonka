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
-- Module      : Amazonka.IoT.DeleteCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate.
--
-- A certificate cannot be deleted if it has a policy or IoT thing attached
-- to it or if its status is set to ACTIVE. To delete a certificate, first
-- use the DetachPolicy action to detach all policies. Next, use the
-- UpdateCertificate action to set the certificate to the INACTIVE status.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteCertificate>
-- action.
module Amazonka.IoT.DeleteCertificate
  ( -- * Creating a Request
    DeleteCertificate (..),
    newDeleteCertificate,

    -- * Request Lenses
    deleteCertificate_forceDelete,
    deleteCertificate_certificateId,

    -- * Destructuring the Response
    DeleteCertificateResponse (..),
    newDeleteCertificateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeleteCertificate operation.
--
-- /See:/ 'newDeleteCertificate' smart constructor.
data DeleteCertificate = DeleteCertificate'
  { -- | Forces the deletion of a certificate if it is inactive and is not
    -- attached to an IoT thing.
    forceDelete :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text
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
-- 'forceDelete', 'deleteCertificate_forceDelete' - Forces the deletion of a certificate if it is inactive and is not
-- attached to an IoT thing.
--
-- 'certificateId', 'deleteCertificate_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
newDeleteCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  DeleteCertificate
newDeleteCertificate pCertificateId_ =
  DeleteCertificate'
    { forceDelete = Prelude.Nothing,
      certificateId = pCertificateId_
    }

-- | Forces the deletion of a certificate if it is inactive and is not
-- attached to an IoT thing.
deleteCertificate_forceDelete :: Lens.Lens' DeleteCertificate (Prelude.Maybe Prelude.Bool)
deleteCertificate_forceDelete = Lens.lens (\DeleteCertificate' {forceDelete} -> forceDelete) (\s@DeleteCertificate' {} a -> s {forceDelete = a} :: DeleteCertificate)

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
deleteCertificate_certificateId :: Lens.Lens' DeleteCertificate Prelude.Text
deleteCertificate_certificateId = Lens.lens (\DeleteCertificate' {certificateId} -> certificateId) (\s@DeleteCertificate' {} a -> s {certificateId = a} :: DeleteCertificate)

instance Core.AWSRequest DeleteCertificate where
  type
    AWSResponse DeleteCertificate =
      DeleteCertificateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteCertificateResponse'

instance Prelude.Hashable DeleteCertificate where
  hashWithSalt _salt DeleteCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` forceDelete
      `Prelude.hashWithSalt` certificateId

instance Prelude.NFData DeleteCertificate where
  rnf DeleteCertificate' {..} =
    Prelude.rnf forceDelete
      `Prelude.seq` Prelude.rnf certificateId

instance Data.ToHeaders DeleteCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCertificate where
  toPath DeleteCertificate' {..} =
    Prelude.mconcat
      ["/certificates/", Data.toBS certificateId]

instance Data.ToQuery DeleteCertificate where
  toQuery DeleteCertificate' {..} =
    Prelude.mconcat ["forceDelete" Data.=: forceDelete]

-- | /See:/ 'newDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCertificateResponse ::
  DeleteCertificateResponse
newDeleteCertificateResponse =
  DeleteCertificateResponse'

instance Prelude.NFData DeleteCertificateResponse where
  rnf _ = ()
