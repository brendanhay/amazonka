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
-- Module      : Network.AWS.IoT.DescribeCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified certificate.
module Network.AWS.IoT.DescribeCertificate
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeCertificate operation.
--
-- /See:/ 'newDescribeCertificate' smart constructor.
data DescribeCertificate = DescribeCertificate'
  { -- | The ID of the certificate. (The last part of the certificate ARN
    -- contains the certificate ID.)
    certificateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeCertificate where
  type
    Rs DescribeCertificate =
      DescribeCertificateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCertificateResponse'
            Prelude.<$> (x Prelude..?> "certificateDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCertificate

instance Prelude.NFData DescribeCertificate

instance Prelude.ToHeaders DescribeCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeCertificate where
  toPath DescribeCertificate' {..} =
    Prelude.mconcat
      ["/certificates/", Prelude.toBS certificateId]

instance Prelude.ToQuery DescribeCertificate where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DescribeCertificateResponse
