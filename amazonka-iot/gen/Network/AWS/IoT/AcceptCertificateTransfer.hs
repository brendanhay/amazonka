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
-- Module      : Network.AWS.IoT.AcceptCertificateTransfer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts a pending certificate transfer. The default state of the
-- certificate is INACTIVE.
--
-- To check for pending certificate transfers, call ListCertificates to
-- enumerate your certificates.
module Network.AWS.IoT.AcceptCertificateTransfer
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest AcceptCertificateTransfer where
  type
    Rs AcceptCertificateTransfer =
      AcceptCertificateTransferResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveNull
      AcceptCertificateTransferResponse'

instance Prelude.Hashable AcceptCertificateTransfer

instance Prelude.NFData AcceptCertificateTransfer

instance Prelude.ToHeaders AcceptCertificateTransfer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON AcceptCertificateTransfer where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath AcceptCertificateTransfer where
  toPath AcceptCertificateTransfer' {..} =
    Prelude.mconcat
      [ "/accept-certificate-transfer/",
        Prelude.toBS certificateId
      ]

instance Prelude.ToQuery AcceptCertificateTransfer where
  toQuery AcceptCertificateTransfer' {..} =
    Prelude.mconcat
      ["setAsActive" Prelude.=: setAsActive]

-- | /See:/ 'newAcceptCertificateTransferResponse' smart constructor.
data AcceptCertificateTransferResponse = AcceptCertificateTransferResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
