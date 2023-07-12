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
-- Module      : Amazonka.IoT.RejectCertificateTransfer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects a pending certificate transfer. After IoT rejects a certificate
-- transfer, the certificate status changes from __PENDING_TRANSFER__ to
-- __INACTIVE__.
--
-- To check for pending certificate transfers, call ListCertificates to
-- enumerate your certificates.
--
-- This operation can only be called by the transfer destination. After it
-- is called, the certificate will be returned to the source\'s account in
-- the INACTIVE state.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions RejectCertificateTransfer>
-- action.
module Amazonka.IoT.RejectCertificateTransfer
  ( -- * Creating a Request
    RejectCertificateTransfer (..),
    newRejectCertificateTransfer,

    -- * Request Lenses
    rejectCertificateTransfer_rejectReason,
    rejectCertificateTransfer_certificateId,

    -- * Destructuring the Response
    RejectCertificateTransferResponse (..),
    newRejectCertificateTransferResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the RejectCertificateTransfer operation.
--
-- /See:/ 'newRejectCertificateTransfer' smart constructor.
data RejectCertificateTransfer = RejectCertificateTransfer'
  { -- | The reason the certificate transfer was rejected.
    rejectReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectCertificateTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rejectReason', 'rejectCertificateTransfer_rejectReason' - The reason the certificate transfer was rejected.
--
-- 'certificateId', 'rejectCertificateTransfer_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
newRejectCertificateTransfer ::
  -- | 'certificateId'
  Prelude.Text ->
  RejectCertificateTransfer
newRejectCertificateTransfer pCertificateId_ =
  RejectCertificateTransfer'
    { rejectReason =
        Prelude.Nothing,
      certificateId = pCertificateId_
    }

-- | The reason the certificate transfer was rejected.
rejectCertificateTransfer_rejectReason :: Lens.Lens' RejectCertificateTransfer (Prelude.Maybe Prelude.Text)
rejectCertificateTransfer_rejectReason = Lens.lens (\RejectCertificateTransfer' {rejectReason} -> rejectReason) (\s@RejectCertificateTransfer' {} a -> s {rejectReason = a} :: RejectCertificateTransfer)

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
rejectCertificateTransfer_certificateId :: Lens.Lens' RejectCertificateTransfer Prelude.Text
rejectCertificateTransfer_certificateId = Lens.lens (\RejectCertificateTransfer' {certificateId} -> certificateId) (\s@RejectCertificateTransfer' {} a -> s {certificateId = a} :: RejectCertificateTransfer)

instance Core.AWSRequest RejectCertificateTransfer where
  type
    AWSResponse RejectCertificateTransfer =
      RejectCertificateTransferResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull
      RejectCertificateTransferResponse'

instance Prelude.Hashable RejectCertificateTransfer where
  hashWithSalt _salt RejectCertificateTransfer' {..} =
    _salt
      `Prelude.hashWithSalt` rejectReason
      `Prelude.hashWithSalt` certificateId

instance Prelude.NFData RejectCertificateTransfer where
  rnf RejectCertificateTransfer' {..} =
    Prelude.rnf rejectReason
      `Prelude.seq` Prelude.rnf certificateId

instance Data.ToHeaders RejectCertificateTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RejectCertificateTransfer where
  toJSON RejectCertificateTransfer' {..} =
    Data.object
      ( Prelude.catMaybes
          [("rejectReason" Data..=) Prelude.<$> rejectReason]
      )

instance Data.ToPath RejectCertificateTransfer where
  toPath RejectCertificateTransfer' {..} =
    Prelude.mconcat
      [ "/reject-certificate-transfer/",
        Data.toBS certificateId
      ]

instance Data.ToQuery RejectCertificateTransfer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectCertificateTransferResponse' smart constructor.
data RejectCertificateTransferResponse = RejectCertificateTransferResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectCertificateTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newRejectCertificateTransferResponse ::
  RejectCertificateTransferResponse
newRejectCertificateTransferResponse =
  RejectCertificateTransferResponse'

instance
  Prelude.NFData
    RejectCertificateTransferResponse
  where
  rnf _ = ()
