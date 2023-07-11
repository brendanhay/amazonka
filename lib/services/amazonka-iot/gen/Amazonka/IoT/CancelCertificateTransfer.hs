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
-- Module      : Amazonka.IoT.CancelCertificateTransfer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending transfer for the specified certificate.
--
-- __Note__ Only the transfer source account can use this operation to
-- cancel a transfer. (Transfer destinations can use
-- RejectCertificateTransfer instead.) After transfer, IoT returns the
-- certificate to the source account in the INACTIVE state. After the
-- destination account has accepted the transfer, the transfer cannot be
-- cancelled.
--
-- After a certificate transfer is cancelled, the status of the certificate
-- changes from PENDING_TRANSFER to INACTIVE.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CancelCertificateTransfer>
-- action.
module Amazonka.IoT.CancelCertificateTransfer
  ( -- * Creating a Request
    CancelCertificateTransfer (..),
    newCancelCertificateTransfer,

    -- * Request Lenses
    cancelCertificateTransfer_certificateId,

    -- * Destructuring the Response
    CancelCertificateTransferResponse (..),
    newCancelCertificateTransferResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the CancelCertificateTransfer operation.
--
-- /See:/ 'newCancelCertificateTransfer' smart constructor.
data CancelCertificateTransfer = CancelCertificateTransfer'
  { -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCertificateTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'cancelCertificateTransfer_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
newCancelCertificateTransfer ::
  -- | 'certificateId'
  Prelude.Text ->
  CancelCertificateTransfer
newCancelCertificateTransfer pCertificateId_ =
  CancelCertificateTransfer'
    { certificateId =
        pCertificateId_
    }

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
cancelCertificateTransfer_certificateId :: Lens.Lens' CancelCertificateTransfer Prelude.Text
cancelCertificateTransfer_certificateId = Lens.lens (\CancelCertificateTransfer' {certificateId} -> certificateId) (\s@CancelCertificateTransfer' {} a -> s {certificateId = a} :: CancelCertificateTransfer)

instance Core.AWSRequest CancelCertificateTransfer where
  type
    AWSResponse CancelCertificateTransfer =
      CancelCertificateTransferResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull
      CancelCertificateTransferResponse'

instance Prelude.Hashable CancelCertificateTransfer where
  hashWithSalt _salt CancelCertificateTransfer' {..} =
    _salt `Prelude.hashWithSalt` certificateId

instance Prelude.NFData CancelCertificateTransfer where
  rnf CancelCertificateTransfer' {..} =
    Prelude.rnf certificateId

instance Data.ToHeaders CancelCertificateTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CancelCertificateTransfer where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath CancelCertificateTransfer where
  toPath CancelCertificateTransfer' {..} =
    Prelude.mconcat
      [ "/cancel-certificate-transfer/",
        Data.toBS certificateId
      ]

instance Data.ToQuery CancelCertificateTransfer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelCertificateTransferResponse' smart constructor.
data CancelCertificateTransferResponse = CancelCertificateTransferResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelCertificateTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCancelCertificateTransferResponse ::
  CancelCertificateTransferResponse
newCancelCertificateTransferResponse =
  CancelCertificateTransferResponse'

instance
  Prelude.NFData
    CancelCertificateTransferResponse
  where
  rnf _ = ()
