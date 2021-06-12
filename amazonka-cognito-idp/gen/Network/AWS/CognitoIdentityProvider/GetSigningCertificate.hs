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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to get a signing certificate from Cognito.
--
-- /See:/ 'newGetSigningCertificate' smart constructor.
data GetSigningCertificate = GetSigningCertificate'
  { -- | The user pool ID.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetSigningCertificate
newGetSigningCertificate pUserPoolId_ =
  GetSigningCertificate' {userPoolId = pUserPoolId_}

-- | The user pool ID.
getSigningCertificate_userPoolId :: Lens.Lens' GetSigningCertificate Core.Text
getSigningCertificate_userPoolId = Lens.lens (\GetSigningCertificate' {userPoolId} -> userPoolId) (\s@GetSigningCertificate' {} a -> s {userPoolId = a} :: GetSigningCertificate)

instance Core.AWSRequest GetSigningCertificate where
  type
    AWSResponse GetSigningCertificate =
      GetSigningCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSigningCertificateResponse'
            Core.<$> (x Core..?> "Certificate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSigningCertificate

instance Core.NFData GetSigningCertificate

instance Core.ToHeaders GetSigningCertificate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.GetSigningCertificate" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSigningCertificate where
  toJSON GetSigningCertificate' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("UserPoolId" Core..= userPoolId)]
      )

instance Core.ToPath GetSigningCertificate where
  toPath = Core.const "/"

instance Core.ToQuery GetSigningCertificate where
  toQuery = Core.const Core.mempty

-- | Response from Cognito for a signing certificate request.
--
-- /See:/ 'newGetSigningCertificateResponse' smart constructor.
data GetSigningCertificateResponse = GetSigningCertificateResponse'
  { -- | The signing certificate.
    certificate :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetSigningCertificateResponse
newGetSigningCertificateResponse pHttpStatus_ =
  GetSigningCertificateResponse'
    { certificate =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The signing certificate.
getSigningCertificateResponse_certificate :: Lens.Lens' GetSigningCertificateResponse (Core.Maybe Core.Text)
getSigningCertificateResponse_certificate = Lens.lens (\GetSigningCertificateResponse' {certificate} -> certificate) (\s@GetSigningCertificateResponse' {} a -> s {certificate = a} :: GetSigningCertificateResponse)

-- | The response's http status code.
getSigningCertificateResponse_httpStatus :: Lens.Lens' GetSigningCertificateResponse Core.Int
getSigningCertificateResponse_httpStatus = Lens.lens (\GetSigningCertificateResponse' {httpStatus} -> httpStatus) (\s@GetSigningCertificateResponse' {} a -> s {httpStatus = a} :: GetSigningCertificateResponse)

instance Core.NFData GetSigningCertificateResponse
