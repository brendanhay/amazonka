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
-- Module      : Network.AWS.IoT.DeleteCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate.
--
-- A certificate cannot be deleted if it has a policy or IoT thing attached
-- to it or if its status is set to ACTIVE. To delete a certificate, first
-- use the DetachPrincipalPolicy API to detach all policies. Next, use the
-- UpdateCertificate API to set the certificate to the INACTIVE status.
module Network.AWS.IoT.DeleteCertificate
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteCertificate where
  type Rs DeleteCertificate = DeleteCertificateResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteCertificateResponse'

instance Prelude.Hashable DeleteCertificate

instance Prelude.NFData DeleteCertificate

instance Prelude.ToHeaders DeleteCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteCertificate where
  toPath DeleteCertificate' {..} =
    Prelude.mconcat
      ["/certificates/", Prelude.toBS certificateId]

instance Prelude.ToQuery DeleteCertificate where
  toQuery DeleteCertificate' {..} =
    Prelude.mconcat
      ["forceDelete" Prelude.=: forceDelete]

-- | /See:/ 'newDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCertificateResponse ::
  DeleteCertificateResponse
newDeleteCertificateResponse =
  DeleteCertificateResponse'

instance Prelude.NFData DeleteCertificateResponse
