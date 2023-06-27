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
-- Module      : Amazonka.IoT.UpdateCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the specified certificate. This operation is
-- idempotent.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateCertificate>
-- action.
--
-- Certificates must be in the ACTIVE state to authenticate devices that
-- use a certificate to connect to IoT.
--
-- Within a few minutes of updating a certificate from the ACTIVE state to
-- any other state, IoT disconnects all devices that used that certificate
-- to connect. Devices cannot use a certificate that is not in the ACTIVE
-- state to reconnect.
module Amazonka.IoT.UpdateCertificate
  ( -- * Creating a Request
    UpdateCertificate (..),
    newUpdateCertificate,

    -- * Request Lenses
    updateCertificate_certificateId,
    updateCertificate_newStatus,

    -- * Destructuring the Response
    UpdateCertificateResponse (..),
    newUpdateCertificateResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the UpdateCertificate operation.
--
-- /See:/ 'newUpdateCertificate' smart constructor.
data UpdateCertificate = UpdateCertificate'
  { -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text,
    -- | The new status.
    --
    -- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION
    -- will result in an exception being thrown. PENDING_TRANSFER and
    -- PENDING_ACTIVATION are statuses used internally by IoT. They are not
    -- intended for developer use.
    --
    -- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
    -- not be used.
    newStatus' :: CertificateStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'updateCertificate_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
--
-- 'newStatus'', 'updateCertificate_newStatus' - The new status.
--
-- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION
-- will result in an exception being thrown. PENDING_TRANSFER and
-- PENDING_ACTIVATION are statuses used internally by IoT. They are not
-- intended for developer use.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
-- not be used.
newUpdateCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  -- | 'newStatus''
  CertificateStatus ->
  UpdateCertificate
newUpdateCertificate pCertificateId_ pNewStatus_ =
  UpdateCertificate'
    { certificateId = pCertificateId_,
      newStatus' = pNewStatus_
    }

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
updateCertificate_certificateId :: Lens.Lens' UpdateCertificate Prelude.Text
updateCertificate_certificateId = Lens.lens (\UpdateCertificate' {certificateId} -> certificateId) (\s@UpdateCertificate' {} a -> s {certificateId = a} :: UpdateCertificate)

-- | The new status.
--
-- __Note:__ Setting the status to PENDING_TRANSFER or PENDING_ACTIVATION
-- will result in an exception being thrown. PENDING_TRANSFER and
-- PENDING_ACTIVATION are statuses used internally by IoT. They are not
-- intended for developer use.
--
-- __Note:__ The status value REGISTER_INACTIVE is deprecated and should
-- not be used.
updateCertificate_newStatus :: Lens.Lens' UpdateCertificate CertificateStatus
updateCertificate_newStatus = Lens.lens (\UpdateCertificate' {newStatus'} -> newStatus') (\s@UpdateCertificate' {} a -> s {newStatus' = a} :: UpdateCertificate)

instance Core.AWSRequest UpdateCertificate where
  type
    AWSResponse UpdateCertificate =
      UpdateCertificateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateCertificateResponse'

instance Prelude.Hashable UpdateCertificate where
  hashWithSalt _salt UpdateCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` certificateId
      `Prelude.hashWithSalt` newStatus'

instance Prelude.NFData UpdateCertificate where
  rnf UpdateCertificate' {..} =
    Prelude.rnf certificateId
      `Prelude.seq` Prelude.rnf newStatus'

instance Data.ToHeaders UpdateCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateCertificate where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath UpdateCertificate where
  toPath UpdateCertificate' {..} =
    Prelude.mconcat
      ["/certificates/", Data.toBS certificateId]

instance Data.ToQuery UpdateCertificate where
  toQuery UpdateCertificate' {..} =
    Prelude.mconcat ["newStatus" Data.=: newStatus']

-- | /See:/ 'newUpdateCertificateResponse' smart constructor.
data UpdateCertificateResponse = UpdateCertificateResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateCertificateResponse ::
  UpdateCertificateResponse
newUpdateCertificateResponse =
  UpdateCertificateResponse'

instance Prelude.NFData UpdateCertificateResponse where
  rnf _ = ()
