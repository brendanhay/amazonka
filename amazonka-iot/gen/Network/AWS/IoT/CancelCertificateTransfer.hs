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
-- Module      : Network.AWS.IoT.CancelCertificateTransfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a pending transfer for the specified certificate.
--
-- __Note__ Only the transfer source account can use this operation to
-- cancel a transfer. (Transfer destinations can use
-- RejectCertificateTransfer instead.) After transfer, AWS IoT returns the
-- certificate to the source account in the INACTIVE state. After the
-- destination account has accepted the transfer, the transfer cannot be
-- cancelled.
--
-- After a certificate transfer is cancelled, the status of the certificate
-- changes from PENDING_TRANSFER to INACTIVE.
module Network.AWS.IoT.CancelCertificateTransfer
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CancelCertificateTransfer operation.
--
-- /See:/ 'newCancelCertificateTransfer' smart constructor.
data CancelCertificateTransfer = CancelCertificateTransfer'
  { -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest CancelCertificateTransfer where
  type
    Rs CancelCertificateTransfer =
      CancelCertificateTransferResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveNull
      CancelCertificateTransferResponse'

instance Prelude.Hashable CancelCertificateTransfer

instance Prelude.NFData CancelCertificateTransfer

instance Prelude.ToHeaders CancelCertificateTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CancelCertificateTransfer where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath CancelCertificateTransfer where
  toPath CancelCertificateTransfer' {..} =
    Prelude.mconcat
      [ "/cancel-certificate-transfer/",
        Prelude.toBS certificateId
      ]

instance Prelude.ToQuery CancelCertificateTransfer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelCertificateTransferResponse' smart constructor.
data CancelCertificateTransferResponse = CancelCertificateTransferResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
