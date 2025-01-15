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
-- Module      : Amazonka.IoT.DescribeCertificate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified certificate.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeCertificate>
-- action.
module Amazonka.IoT.DescribeCertificate
  ( -- * Creating a Request
    DescribeCertificate (..),
    newDescribeCertificate,

    -- * Request Lenses
    describeCertificate_certificateId,

    -- * Destructuring the Response
    DescribeCertificateResponse (..),
    newDescribeCertificateResponse,

    -- * Response Lenses
    describeCertificateResponse_certificateDescription,
    describeCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DescribeCertificate operation.
--
-- /See:/ 'newDescribeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
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
-- 'certificateId', 'describeCertificate_certificateId' - The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
newDescribeCertificate ::
  -- | 'certificateId'
  Prelude.Text ->
  DescribeCertificate
newDescribeCertificate pCertificateId_ =
  DescribeCertificate'
    { certificateId =
        pCertificateId_
    }

-- | The ID of the certificate. (The last part of the certificate ARN
-- contains the certificate ID.)
describeCertificate_certificateId :: Lens.Lens' DescribeCertificate Prelude.Text
describeCertificate_certificateId = Lens.lens (\DescribeCertificate' {certificateId} -> certificateId) (\s@DescribeCertificate' {} a -> s {certificateId = a} :: DescribeCertificate)

instance Core.AWSRequest DescribeCertificate where
  type
    AWSResponse DescribeCertificate =
      DescribeCertificateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            Prelude.<$> (x Data..?> "certificateDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCertificate where
  hashWithSalt _salt DescribeCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateId

instance Prelude.NFData DescribeCertificate where
  rnf DescribeCertificate' {..} =
    Prelude.rnf certificateId

instance Data.ToHeaders DescribeCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeCertificate where
  toPath DescribeCertificate' {..} =
    Prelude.mconcat
      ["/certificates/", Data.toBS certificateId]

instance Data.ToQuery DescribeCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | The output of the DescribeCertificate operation.
--
-- /See:/ 'newDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { -- | The description of the certificate.
    certificateDescription :: Prelude.Maybe CertificateDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateDescription', 'describeCertificateResponse_certificateDescription' - The description of the certificate.
--
-- 'httpStatus', 'describeCertificateResponse_httpStatus' - The response's http status code.
newDescribeCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCertificateResponse
newDescribeCertificateResponse pHttpStatus_ =
  DescribeCertificateResponse'
    { certificateDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The description of the certificate.
describeCertificateResponse_certificateDescription :: Lens.Lens' DescribeCertificateResponse (Prelude.Maybe CertificateDescription)
describeCertificateResponse_certificateDescription = Lens.lens (\DescribeCertificateResponse' {certificateDescription} -> certificateDescription) (\s@DescribeCertificateResponse' {} a -> s {certificateDescription = a} :: DescribeCertificateResponse)

-- | The response's http status code.
describeCertificateResponse_httpStatus :: Lens.Lens' DescribeCertificateResponse Prelude.Int
describeCertificateResponse_httpStatus = Lens.lens (\DescribeCertificateResponse' {httpStatus} -> httpStatus) (\s@DescribeCertificateResponse' {} a -> s {httpStatus = a} :: DescribeCertificateResponse)

instance Prelude.NFData DescribeCertificateResponse where
  rnf DescribeCertificateResponse' {..} =
    Prelude.rnf certificateDescription `Prelude.seq`
      Prelude.rnf httpStatus
