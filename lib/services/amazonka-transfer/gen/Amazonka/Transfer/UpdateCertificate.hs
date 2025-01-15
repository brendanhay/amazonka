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
-- Module      : Amazonka.Transfer.UpdateCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the active and inactive dates for a certificate.
module Amazonka.Transfer.UpdateCertificate
  ( -- * Creating a Request
    UpdateCertificate (..),
    newUpdateCertificate,

    -- * Request Lenses
    updateCertificate_activeDate,
    updateCertificate_description,
    updateCertificate_inactiveDate,
    updateCertificate_certificateId,

    -- * Destructuring the Response
    UpdateCertificateResponse (..),
    newUpdateCertificateResponse,

    -- * Response Lenses
    updateCertificateResponse_httpStatus,
    updateCertificateResponse_certificateId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newUpdateCertificate' smart constructor.
data UpdateCertificate = UpdateCertificate'
  { -- | An optional date that specifies when the certificate becomes active.
    activeDate :: Prelude.Maybe Data.POSIX,
    -- | A short description to help identify the certificate.
    description :: Prelude.Maybe Prelude.Text,
    -- | An optional date that specifies when the certificate becomes inactive.
    inactiveDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the certificate object that you are updating.
    certificateId :: Prelude.Text
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
-- 'activeDate', 'updateCertificate_activeDate' - An optional date that specifies when the certificate becomes active.
--
-- 'description', 'updateCertificate_description' - A short description to help identify the certificate.
--
-- 'inactiveDate', 'updateCertificate_inactiveDate' - An optional date that specifies when the certificate becomes inactive.
--
-- 'certificateId', 'updateCertificate_certificateId' - The identifier of the certificate object that you are updating.
newUpdateCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  UpdateCertificate
newUpdateCertificate pCertificateId_ =
  UpdateCertificate'
    { activeDate = Prelude.Nothing,
      description = Prelude.Nothing,
      inactiveDate = Prelude.Nothing,
      certificateId = pCertificateId_
    }

-- | An optional date that specifies when the certificate becomes active.
updateCertificate_activeDate :: Lens.Lens' UpdateCertificate (Prelude.Maybe Prelude.UTCTime)
updateCertificate_activeDate = Lens.lens (\UpdateCertificate' {activeDate} -> activeDate) (\s@UpdateCertificate' {} a -> s {activeDate = a} :: UpdateCertificate) Prelude.. Lens.mapping Data._Time

-- | A short description to help identify the certificate.
updateCertificate_description :: Lens.Lens' UpdateCertificate (Prelude.Maybe Prelude.Text)
updateCertificate_description = Lens.lens (\UpdateCertificate' {description} -> description) (\s@UpdateCertificate' {} a -> s {description = a} :: UpdateCertificate)

-- | An optional date that specifies when the certificate becomes inactive.
updateCertificate_inactiveDate :: Lens.Lens' UpdateCertificate (Prelude.Maybe Prelude.UTCTime)
updateCertificate_inactiveDate = Lens.lens (\UpdateCertificate' {inactiveDate} -> inactiveDate) (\s@UpdateCertificate' {} a -> s {inactiveDate = a} :: UpdateCertificate) Prelude.. Lens.mapping Data._Time

-- | The identifier of the certificate object that you are updating.
updateCertificate_certificateId :: Lens.Lens' UpdateCertificate Prelude.Text
updateCertificate_certificateId = Lens.lens (\UpdateCertificate' {certificateId} -> certificateId) (\s@UpdateCertificate' {} a -> s {certificateId = a} :: UpdateCertificate)

instance Core.AWSRequest UpdateCertificate where
  type
    AWSResponse UpdateCertificate =
      UpdateCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CertificateId")
      )

instance Prelude.Hashable UpdateCertificate where
  hashWithSalt _salt UpdateCertificate' {..} =
    _salt
      `Prelude.hashWithSalt` activeDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` inactiveDate
      `Prelude.hashWithSalt` certificateId

instance Prelude.NFData UpdateCertificate where
  rnf UpdateCertificate' {..} =
    Prelude.rnf activeDate `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf inactiveDate `Prelude.seq`
          Prelude.rnf certificateId

instance Data.ToHeaders UpdateCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.UpdateCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCertificate where
  toJSON UpdateCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveDate" Data..=) Prelude.<$> activeDate,
            ("Description" Data..=) Prelude.<$> description,
            ("InactiveDate" Data..=) Prelude.<$> inactiveDate,
            Prelude.Just
              ("CertificateId" Data..= certificateId)
          ]
      )

instance Data.ToPath UpdateCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCertificateResponse' smart constructor.
data UpdateCertificateResponse = UpdateCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the identifier of the certificate object that you are updating.
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCertificateResponse_httpStatus' - The response's http status code.
--
-- 'certificateId', 'updateCertificateResponse_certificateId' - Returns the identifier of the certificate object that you are updating.
newUpdateCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'certificateId'
  Prelude.Text ->
  UpdateCertificateResponse
newUpdateCertificateResponse
  pHttpStatus_
  pCertificateId_ =
    UpdateCertificateResponse'
      { httpStatus =
          pHttpStatus_,
        certificateId = pCertificateId_
      }

-- | The response's http status code.
updateCertificateResponse_httpStatus :: Lens.Lens' UpdateCertificateResponse Prelude.Int
updateCertificateResponse_httpStatus = Lens.lens (\UpdateCertificateResponse' {httpStatus} -> httpStatus) (\s@UpdateCertificateResponse' {} a -> s {httpStatus = a} :: UpdateCertificateResponse)

-- | Returns the identifier of the certificate object that you are updating.
updateCertificateResponse_certificateId :: Lens.Lens' UpdateCertificateResponse Prelude.Text
updateCertificateResponse_certificateId = Lens.lens (\UpdateCertificateResponse' {certificateId} -> certificateId) (\s@UpdateCertificateResponse' {} a -> s {certificateId = a} :: UpdateCertificateResponse)

instance Prelude.NFData UpdateCertificateResponse where
  rnf UpdateCertificateResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf certificateId
