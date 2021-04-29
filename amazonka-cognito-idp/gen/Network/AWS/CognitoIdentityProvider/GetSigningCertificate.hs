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
-- Module      : Network.AWS.CognitoIdentityProvider.GetSigningCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This method takes a user pool ID, and returns the signing certificate.
module Network.AWS.CognitoIdentityProvider.GetSigningCertificate
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

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to get a signing certificate from Cognito.
--
-- /See:/ 'newGetSigningCertificate' smart constructor.
data GetSigningCertificate = GetSigningCertificate'
  { -- | The user pool ID.
    userPoolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetSigningCertificate where
  type
    Rs GetSigningCertificate =
      GetSigningCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSigningCertificateResponse'
            Prelude.<$> (x Prelude..?> "Certificate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSigningCertificate

instance Prelude.NFData GetSigningCertificate

instance Prelude.ToHeaders GetSigningCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSCognitoIdentityProviderService.GetSigningCertificate" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetSigningCertificate where
  toJSON GetSigningCertificate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("UserPoolId" Prelude..= userPoolId)]
      )

instance Prelude.ToPath GetSigningCertificate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetSigningCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | Response from Cognito for a signing certificate request.
--
-- /See:/ 'newGetSigningCertificateResponse' smart constructor.
data GetSigningCertificateResponse = GetSigningCertificateResponse'
  { -- | The signing certificate.
    certificate :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetSigningCertificateResponse
