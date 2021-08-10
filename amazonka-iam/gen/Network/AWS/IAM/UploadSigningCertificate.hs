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
-- Module      : Network.AWS.IAM.UploadSigningCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads an X.509 signing certificate and associates it with the
-- specified IAM user. Some AWS services require you to use certificates to
-- validate requests that are signed with a corresponding private key. When
-- you upload the certificate, its default status is @Active@.
--
-- For information about when you would use an X.509 signing certificate,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Managing server certificates in IAM>
-- in the /IAM User Guide/.
--
-- If the @UserName@ is not specified, the IAM user name is determined
-- implicitly based on the AWS access key ID used to sign the request. This
-- operation works for access keys under the AWS account. Consequently, you
-- can use this operation to manage AWS account root user credentials even
-- if the AWS account has no associated users.
--
-- Because the body of an X.509 certificate can be large, you should use
-- POST rather than GET when calling @UploadSigningCertificate@. For
-- information about setting up signatures and authorization through the
-- API, see
-- <https://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing AWS API requests>
-- in the /AWS General Reference/. For general information about using the
-- Query API with IAM, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/IAM_UsingQueryAPI.html Making query requests>
-- in the /IAM User Guide/.
module Network.AWS.IAM.UploadSigningCertificate
  ( -- * Creating a Request
    UploadSigningCertificate (..),
    newUploadSigningCertificate,

    -- * Request Lenses
    uploadSigningCertificate_userName,
    uploadSigningCertificate_certificateBody,

    -- * Destructuring the Response
    UploadSigningCertificateResponse (..),
    newUploadSigningCertificateResponse,

    -- * Response Lenses
    uploadSigningCertificateResponse_httpStatus,
    uploadSigningCertificateResponse_certificate,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUploadSigningCertificate' smart constructor.
data UploadSigningCertificate = UploadSigningCertificate'
  { -- | The name of the user the signing certificate is for.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Maybe Prelude.Text,
    -- | The contents of the signing certificate.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
    -- this parameter is a string of characters consisting of the following:
    --
    -- -   Any printable ASCII character ranging from the space character
    --     (@\\u0020@) through the end of the ASCII character range
    --
    -- -   The printable characters in the Basic Latin and Latin-1 Supplement
    --     character set (through @\\u00FF@)
    --
    -- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
    --     carriage return (@\\u000D@)
    certificateBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadSigningCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'uploadSigningCertificate_userName' - The name of the user the signing certificate is for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'certificateBody', 'uploadSigningCertificate_certificateBody' - The contents of the signing certificate.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
newUploadSigningCertificate ::
  -- | 'certificateBody'
  Prelude.Text ->
  UploadSigningCertificate
newUploadSigningCertificate pCertificateBody_ =
  UploadSigningCertificate'
    { userName =
        Prelude.Nothing,
      certificateBody = pCertificateBody_
    }

-- | The name of the user the signing certificate is for.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
uploadSigningCertificate_userName :: Lens.Lens' UploadSigningCertificate (Prelude.Maybe Prelude.Text)
uploadSigningCertificate_userName = Lens.lens (\UploadSigningCertificate' {userName} -> userName) (\s@UploadSigningCertificate' {} a -> s {userName = a} :: UploadSigningCertificate)

-- | The contents of the signing certificate.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> used to validate
-- this parameter is a string of characters consisting of the following:
--
-- -   Any printable ASCII character ranging from the space character
--     (@\\u0020@) through the end of the ASCII character range
--
-- -   The printable characters in the Basic Latin and Latin-1 Supplement
--     character set (through @\\u00FF@)
--
-- -   The special characters tab (@\\u0009@), line feed (@\\u000A@), and
--     carriage return (@\\u000D@)
uploadSigningCertificate_certificateBody :: Lens.Lens' UploadSigningCertificate Prelude.Text
uploadSigningCertificate_certificateBody = Lens.lens (\UploadSigningCertificate' {certificateBody} -> certificateBody) (\s@UploadSigningCertificate' {} a -> s {certificateBody = a} :: UploadSigningCertificate)

instance Core.AWSRequest UploadSigningCertificate where
  type
    AWSResponse UploadSigningCertificate =
      UploadSigningCertificateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UploadSigningCertificateResult"
      ( \s h x ->
          UploadSigningCertificateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "Certificate")
      )

instance Prelude.Hashable UploadSigningCertificate

instance Prelude.NFData UploadSigningCertificate

instance Core.ToHeaders UploadSigningCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath UploadSigningCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery UploadSigningCertificate where
  toQuery UploadSigningCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("UploadSigningCertificate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-05-08" :: Prelude.ByteString),
        "UserName" Core.=: userName,
        "CertificateBody" Core.=: certificateBody
      ]

-- | Contains the response to a successful UploadSigningCertificate request.
--
-- /See:/ 'newUploadSigningCertificateResponse' smart constructor.
data UploadSigningCertificateResponse = UploadSigningCertificateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the certificate.
    certificate :: SigningCertificate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadSigningCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'uploadSigningCertificateResponse_httpStatus' - The response's http status code.
--
-- 'certificate', 'uploadSigningCertificateResponse_certificate' - Information about the certificate.
newUploadSigningCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'certificate'
  SigningCertificate ->
  UploadSigningCertificateResponse
newUploadSigningCertificateResponse
  pHttpStatus_
  pCertificate_ =
    UploadSigningCertificateResponse'
      { httpStatus =
          pHttpStatus_,
        certificate = pCertificate_
      }

-- | The response's http status code.
uploadSigningCertificateResponse_httpStatus :: Lens.Lens' UploadSigningCertificateResponse Prelude.Int
uploadSigningCertificateResponse_httpStatus = Lens.lens (\UploadSigningCertificateResponse' {httpStatus} -> httpStatus) (\s@UploadSigningCertificateResponse' {} a -> s {httpStatus = a} :: UploadSigningCertificateResponse)

-- | Information about the certificate.
uploadSigningCertificateResponse_certificate :: Lens.Lens' UploadSigningCertificateResponse SigningCertificate
uploadSigningCertificateResponse_certificate = Lens.lens (\UploadSigningCertificateResponse' {certificate} -> certificate) (\s@UploadSigningCertificateResponse' {} a -> s {certificate = a} :: UploadSigningCertificateResponse)

instance
  Prelude.NFData
    UploadSigningCertificateResponse
