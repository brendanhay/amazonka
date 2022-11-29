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
-- Module      : Amazonka.Transfer.DescribeCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the certificate that\'s identified by the @CertificateId@.
module Amazonka.Transfer.DescribeCertificate
  ( -- * Creating a Request
    DescribeCertificate (..),
    newDescribeCertificate,

    -- * Request Lenses
    describeCertificate_certificateId,

    -- * Destructuring the Response
    DescribeCertificateResponse (..),
    newDescribeCertificateResponse,

    -- * Response Lenses
    describeCertificateResponse_httpStatus,
    describeCertificateResponse_certificate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newDescribeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { -- | An array of identifiers for the imported certificates. You use this
    -- identifier for working with profiles and partner profiles.
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateId', 'describeCertificate_certificateId' - An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
newDescribeCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  DescribeCertificate
newDescribeCertificate pCertificateId_ =
  DescribeCertificate'
    { certificateId =
        pCertificateId_
    }

-- | An array of identifiers for the imported certificates. You use this
-- identifier for working with profiles and partner profiles.
describeCertificate_certificateId :: Lens.Lens' DescribeCertificate Prelude.Text
describeCertificate_certificateId = Lens.lens (\DescribeCertificate' {certificateId} -> certificateId) (\s@DescribeCertificate' {} a -> s {certificateId = a} :: DescribeCertificate)

instance Core.AWSRequest DescribeCertificate where
  type
    AWSResponse DescribeCertificate =
      DescribeCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Certificate")
      )

instance Prelude.Hashable DescribeCertificate where
  hashWithSalt _salt DescribeCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateId

instance Prelude.NFData DescribeCertificate where
  rnf DescribeCertificate' {..} =
    Prelude.rnf certificateId

instance Core.ToHeaders DescribeCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.DescribeCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCertificate where
  toJSON DescribeCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateId" Core..= certificateId)
          ]
      )

instance Core.ToPath DescribeCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The details for the specified certificate, returned as an object.
    certificate :: DescribedCertificate
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeCertificateResponse_httpStatus' - The response's http status code.
--
-- 'certificate', 'describeCertificateResponse_certificate' - The details for the specified certificate, returned as an object.
newDescribeCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'certificate'
  DescribedCertificate ->
  DescribeCertificateResponse
newDescribeCertificateResponse
  pHttpStatus_
  pCertificate_ =
    DescribeCertificateResponse'
      { httpStatus =
          pHttpStatus_,
        certificate = pCertificate_
      }

-- | The response's http status code.
describeCertificateResponse_httpStatus :: Lens.Lens' DescribeCertificateResponse Prelude.Int
describeCertificateResponse_httpStatus = Lens.lens (\DescribeCertificateResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificateResponse' {} a -> s {httpStatus = a} :: DescribeCertificateResponse)

-- | The details for the specified certificate, returned as an object.
describeCertificateResponse_certificate :: Lens.Lens' DescribeCertificateResponse DescribedCertificate
describeCertificateResponse_certificate = Lens.lens (\DescribeCertificateResponse' {certificate} -> certificate) (\s@DescribeCertificateResponse' {} a -> s {certificate = a} :: DescribeCertificateResponse)

instance Prelude.NFData DescribeCertificateResponse where
  rnf DescribeCertificateResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf certificate
