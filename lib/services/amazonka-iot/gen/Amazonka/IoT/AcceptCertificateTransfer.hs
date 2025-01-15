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
-- Module      : Amazonka.IoT.AcceptCertificateTransfer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a pending certificate transfer. The default state of the
-- certificate is INACTIVE.
--
-- To check for pending certificate transfers, call ListCertificates to
-- enumerate your certificates.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions AcceptCertificateTransfer>
-- action.
module Amazonka.IoT.AcceptCertificateTransfer
  ( -- * Creating a Request
    AcceptCertificateTransfer (..),
    newAcceptCertificateTransfer,

    -- * Request Lenses
    acceptCertificateTransfer_setAsActive,
    acceptCertificateTransfer_certificateId,

    -- * Destructuring the Response
    AcceptCertificateTransferResponse (..),
    newAcceptCertificateTransferResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the AcceptCertificateTransfer operation.
--
-- /See:/ 'newAcceptCertificateTransfer' smart constructor.
data AcceptCertificateTransfer = AcceptCertificateTransfer'
  { -- | Specifies whether the certificate is active.
    setAsActive :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptCertificateTransfer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setAsActive', 'acceptCertificateTransfer_setAsActive' - Specifies whether the certificate is active.
--
-- 'certificateId', 'acceptCertificateTransfer_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
newAcceptCertificateTransfer ::
  -- | 'certificateId'
  Prelude.Text ->
  AcceptCertificateTransfer
newAcceptCertificateTransfer pCertificateId_ =
  AcceptCertificateTransfer'
    { setAsActive =
        Prelude.Nothing,
      certificateId = pCertificateId_
    }

-- | Specifies whether the certificate is active.
acceptCertificateTransfer_setAsActive :: Lens.Lens' AcceptCertificateTransfer (Prelude.Maybe Prelude.Bool)
acceptCertificateTransfer_setAsActive = Lens.lens (\AcceptCertificateTransfer' {setAsActive} -> setAsActive) (\s@AcceptCertificateTransfer' {} a -> s {setAsActive = a} :: AcceptCertificateTransfer)

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
acceptCertificateTransfer_certificateId :: Lens.Lens' AcceptCertificateTransfer Prelude.Text
acceptCertificateTransfer_certificateId = Lens.lens (\AcceptCertificateTransfer' {certificateId} -> certificateId) (\s@AcceptCertificateTransfer' {} a -> s {certificateId = a} :: AcceptCertificateTransfer)

instance Core.AWSRequest AcceptCertificateTransfer where
  type
    AWSResponse AcceptCertificateTransfer =
      AcceptCertificateTransferResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull
      AcceptCertificateTransferResponse'

instance Prelude.Hashable AcceptCertificateTransfer where
  hashWithSalt _salt AcceptCertificateTransfer' {..} =
    _salt
      `Prelude.hashWithSalt` setAsActive
      `Prelude.hashWithSalt` certificateId

instance Prelude.NFData AcceptCertificateTransfer where
  rnf AcceptCertificateTransfer' {..} =
    Prelude.rnf setAsActive `Prelude.seq`
      Prelude.rnf certificateId

instance Data.ToHeaders AcceptCertificateTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON AcceptCertificateTransfer where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath AcceptCertificateTransfer where
  toPath AcceptCertificateTransfer' {..} =
    Prelude.mconcat
      [ "/accept-certificate-transfer/",
        Data.toBS certificateId
      ]

instance Data.ToQuery AcceptCertificateTransfer where
  toQuery AcceptCertificateTransfer' {..} =
    Prelude.mconcat ["setAsActive" Data.=: setAsActive]

-- | /See:/ 'newAcceptCertificateTransferResponse' smart constructor.
data AcceptCertificateTransferResponse = AcceptCertificateTransferResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptCertificateTransferResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAcceptCertificateTransferResponse ::
  AcceptCertificateTransferResponse
newAcceptCertificateTransferResponse =
  AcceptCertificateTransferResponse'

instance
  Prelude.NFData
    AcceptCertificateTransferResponse
  where
  rnf _ = ()
