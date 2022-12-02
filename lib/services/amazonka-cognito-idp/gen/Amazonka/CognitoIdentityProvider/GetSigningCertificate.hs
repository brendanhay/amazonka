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
-- Module      : Amazonka.CognitoIdentityProvider.GetSigningCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This method takes a user pool ID, and returns the signing certificate.
-- The issued certificate is valid for 10 years from the date of issue.
--
-- Amazon Cognito issues and assigns a new signing certificate annually.
-- This process returns a new value in the response to
-- @GetSigningCertificate@, but doesn\'t invalidate the original
-- certificate.
module Amazonka.CognitoIdentityProvider.GetSigningCertificate
  ( -- * Creating a Request
    GetSigningCertificate (..),
    newGetSigningCertificate,

    -- * Request Lenses
    getSigningCertificate_userPoolId,

    -- * Destructuring the Response
    GetSigningCertificateResponse (..),
    newGetSigningCertificateResponse,

    -- * Response Lenses
    getSigningCertificateResponse_certificate,
    getSigningCertificateResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to get a signing certificate from Amazon Cognito.
--
-- /See:/ 'newGetSigningCertificate' smart constructor.
data GetSigningCertificate = GetSigningCertificate'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSigningCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolId', 'getSigningCertificate_userPoolId' - The user pool ID.
newGetSigningCertificate ::
  -- | 'userPoolId'
  Prelude.Text ->
  GetSigningCertificate
newGetSigningCertificate pUserPoolId_ =
  GetSigningCertificate' {userPoolId = pUserPoolId_}

-- | The user pool ID.
getSigningCertificate_userPoolId :: Lens.Lens' GetSigningCertificate Prelude.Text
getSigningCertificate_userPoolId = Lens.lens (\GetSigningCertificate' {userPoolId} -> userPoolId) (\s@GetSigningCertificate' {} a -> s {userPoolId = a} :: GetSigningCertificate)

instance Core.AWSRequest GetSigningCertificate where
  type
    AWSResponse GetSigningCertificate =
      GetSigningCertificateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSigningCertificateResponse'
            Prelude.<$> (x Data..?> "Certificate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSigningCertificate where
  hashWithSalt _salt GetSigningCertificate' {..} =
    _salt `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData GetSigningCertificate where
  rnf GetSigningCertificate' {..} =
    Prelude.rnf userPoolId

instance Data.ToHeaders GetSigningCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.GetSigningCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSigningCertificate where
  toJSON GetSigningCertificate' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserPoolId" Data..= userPoolId)]
      )

instance Data.ToPath GetSigningCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSigningCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | Response from Amazon Cognito for a signing certificate request.
--
-- /See:/ 'newGetSigningCertificateResponse' smart constructor.
data GetSigningCertificateResponse = GetSigningCertificateResponse'
  { -- | The signing certificate.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSigningCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificate', 'getSigningCertificateResponse_certificate' - The signing certificate.
--
-- 'httpStatus', 'getSigningCertificateResponse_httpStatus' - The response's http status code.
newGetSigningCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSigningCertificateResponse
newGetSigningCertificateResponse pHttpStatus_ =
  GetSigningCertificateResponse'
    { certificate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The signing certificate.
getSigningCertificateResponse_certificate :: Lens.Lens' GetSigningCertificateResponse (Prelude.Maybe Prelude.Text)
getSigningCertificateResponse_certificate = Lens.lens (\GetSigningCertificateResponse' {certificate} -> certificate) (\s@GetSigningCertificateResponse' {} a -> s {certificate = a} :: GetSigningCertificateResponse)

-- | The response's http status code.
getSigningCertificateResponse_httpStatus :: Lens.Lens' GetSigningCertificateResponse Prelude.Int
getSigningCertificateResponse_httpStatus = Lens.lens (\GetSigningCertificateResponse' {httpStatus} -> httpStatus) (\s@GetSigningCertificateResponse' {} a -> s {httpStatus = a} :: GetSigningCertificateResponse)

instance Prelude.NFData GetSigningCertificateResponse where
  rnf GetSigningCertificateResponse' {..} =
    Prelude.rnf certificate
      `Prelude.seq` Prelude.rnf httpStatus
