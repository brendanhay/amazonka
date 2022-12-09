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
-- Module      : Amazonka.IAM.UploadServerCertificate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Uploads a server certificate entity for the Amazon Web Services account.
-- The server certificate entity includes a public key certificate, a
-- private key, and an optional certificate chain, which should all be
-- PEM-encoded.
--
-- We recommend that you use
-- <https://docs.aws.amazon.com/acm/ Certificate Manager> to provision,
-- manage, and deploy your server certificates. With ACM you can request a
-- certificate, deploy it to Amazon Web Services resources, and let ACM
-- handle certificate renewals for you. Certificates provided by ACM are
-- free. For more information about using ACM, see the
-- <https://docs.aws.amazon.com/acm/latest/userguide/ Certificate Manager User Guide>.
--
-- For more information about working with server certificates, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_server-certs.html Working with server certificates>
-- in the /IAM User Guide/. This topic includes a list of Amazon Web
-- Services services that can use the server certificates that you manage
-- with IAM.
--
-- For information about the number of server certificates you can upload,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_iam-quotas.html IAM and STS quotas>
-- in the /IAM User Guide/.
--
-- Because the body of the public key certificate, private key, and the
-- certificate chain can be large, you should use POST rather than GET when
-- calling @UploadServerCertificate@. For information about setting up
-- signatures and authorization through the API, see
-- <https://docs.aws.amazon.com/general/latest/gr/signing_aws_api_requests.html Signing Amazon Web Services API requests>
-- in the /Amazon Web Services General Reference/. For general information
-- about using the Query API with IAM, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/programming.html Calling the API by making HTTP query requests>
-- in the /IAM User Guide/.
module Amazonka.IAM.UploadServerCertificate
  ( -- * Creating a Request
    UploadServerCertificate (..),
    newUploadServerCertificate,

    -- * Request Lenses
    uploadServerCertificate_certificateChain,
    uploadServerCertificate_path,
    uploadServerCertificate_tags,
    uploadServerCertificate_serverCertificateName,
    uploadServerCertificate_certificateBody,
    uploadServerCertificate_privateKey,

    -- * Destructuring the Response
    UploadServerCertificateResponse (..),
    newUploadServerCertificateResponse,

    -- * Response Lenses
    uploadServerCertificateResponse_serverCertificateMetadata,
    uploadServerCertificateResponse_tags,
    uploadServerCertificateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUploadServerCertificate' smart constructor.
data UploadServerCertificate = UploadServerCertificate'
  { -- | The contents of the certificate chain. This is typically a concatenation
    -- of the PEM-encoded public key certificates of the chain.
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
    certificateChain :: Prelude.Maybe Prelude.Text,
    -- | The path for the server certificate. For more information about paths,
    -- see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    --
    -- This parameter is optional. If it is not included, it defaults to a
    -- slash (\/). This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    --
    -- If you are uploading a server certificate specifically for use with
    -- Amazon CloudFront distributions, you must specify a path using the
    -- @path@ parameter. The path must begin with @\/cloudfront@ and must
    -- include a trailing slash (for example, @\/cloudfront\/test\/@).
    path :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that you want to attach to the new IAM server certificate
    -- resource. Each tag consists of a key name and an associated value. For
    -- more information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the server certificate. Do not include the path in this
    -- value. The name of the certificate cannot contain any spaces.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    serverCertificateName :: Prelude.Text,
    -- | The contents of the public key certificate in PEM-encoded format.
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
    certificateBody :: Prelude.Text,
    -- | The contents of the private key in PEM-encoded format.
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
    privateKey :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadServerCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateChain', 'uploadServerCertificate_certificateChain' - The contents of the certificate chain. This is typically a concatenation
-- of the PEM-encoded public key certificates of the chain.
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
--
-- 'path', 'uploadServerCertificate_path' - The path for the server certificate. For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/). This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- If you are uploading a server certificate specifically for use with
-- Amazon CloudFront distributions, you must specify a path using the
-- @path@ parameter. The path must begin with @\/cloudfront@ and must
-- include a trailing slash (for example, @\/cloudfront\/test\/@).
--
-- 'tags', 'uploadServerCertificate_tags' - A list of tags that you want to attach to the new IAM server certificate
-- resource. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'serverCertificateName', 'uploadServerCertificate_serverCertificateName' - The name for the server certificate. Do not include the path in this
-- value. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
--
-- 'certificateBody', 'uploadServerCertificate_certificateBody' - The contents of the public key certificate in PEM-encoded format.
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
--
-- 'privateKey', 'uploadServerCertificate_privateKey' - The contents of the private key in PEM-encoded format.
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
newUploadServerCertificate ::
  -- | 'serverCertificateName'
  Prelude.Text ->
  -- | 'certificateBody'
  Prelude.Text ->
  -- | 'privateKey'
  Prelude.Text ->
  UploadServerCertificate
newUploadServerCertificate
  pServerCertificateName_
  pCertificateBody_
  pPrivateKey_ =
    UploadServerCertificate'
      { certificateChain =
          Prelude.Nothing,
        path = Prelude.Nothing,
        tags = Prelude.Nothing,
        serverCertificateName = pServerCertificateName_,
        certificateBody = pCertificateBody_,
        privateKey = Data._Sensitive Lens.# pPrivateKey_
      }

-- | The contents of the certificate chain. This is typically a concatenation
-- of the PEM-encoded public key certificates of the chain.
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
uploadServerCertificate_certificateChain :: Lens.Lens' UploadServerCertificate (Prelude.Maybe Prelude.Text)
uploadServerCertificate_certificateChain = Lens.lens (\UploadServerCertificate' {certificateChain} -> certificateChain) (\s@UploadServerCertificate' {} a -> s {certificateChain = a} :: UploadServerCertificate)

-- | The path for the server certificate. For more information about paths,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/). This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- If you are uploading a server certificate specifically for use with
-- Amazon CloudFront distributions, you must specify a path using the
-- @path@ parameter. The path must begin with @\/cloudfront@ and must
-- include a trailing slash (for example, @\/cloudfront\/test\/@).
uploadServerCertificate_path :: Lens.Lens' UploadServerCertificate (Prelude.Maybe Prelude.Text)
uploadServerCertificate_path = Lens.lens (\UploadServerCertificate' {path} -> path) (\s@UploadServerCertificate' {} a -> s {path = a} :: UploadServerCertificate)

-- | A list of tags that you want to attach to the new IAM server certificate
-- resource. Each tag consists of a key name and an associated value. For
-- more information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
uploadServerCertificate_tags :: Lens.Lens' UploadServerCertificate (Prelude.Maybe [Tag])
uploadServerCertificate_tags = Lens.lens (\UploadServerCertificate' {tags} -> tags) (\s@UploadServerCertificate' {} a -> s {tags = a} :: UploadServerCertificate) Prelude.. Lens.mapping Lens.coerced

-- | The name for the server certificate. Do not include the path in this
-- value. The name of the certificate cannot contain any spaces.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
uploadServerCertificate_serverCertificateName :: Lens.Lens' UploadServerCertificate Prelude.Text
uploadServerCertificate_serverCertificateName = Lens.lens (\UploadServerCertificate' {serverCertificateName} -> serverCertificateName) (\s@UploadServerCertificate' {} a -> s {serverCertificateName = a} :: UploadServerCertificate)

-- | The contents of the public key certificate in PEM-encoded format.
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
uploadServerCertificate_certificateBody :: Lens.Lens' UploadServerCertificate Prelude.Text
uploadServerCertificate_certificateBody = Lens.lens (\UploadServerCertificate' {certificateBody} -> certificateBody) (\s@UploadServerCertificate' {} a -> s {certificateBody = a} :: UploadServerCertificate)

-- | The contents of the private key in PEM-encoded format.
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
uploadServerCertificate_privateKey :: Lens.Lens' UploadServerCertificate Prelude.Text
uploadServerCertificate_privateKey = Lens.lens (\UploadServerCertificate' {privateKey} -> privateKey) (\s@UploadServerCertificate' {} a -> s {privateKey = a} :: UploadServerCertificate) Prelude.. Data._Sensitive

instance Core.AWSRequest UploadServerCertificate where
  type
    AWSResponse UploadServerCertificate =
      UploadServerCertificateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "UploadServerCertificateResult"
      ( \s h x ->
          UploadServerCertificateResponse'
            Prelude.<$> (x Data..@? "ServerCertificateMetadata")
            Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UploadServerCertificate where
  hashWithSalt _salt UploadServerCertificate' {..} =
    _salt `Prelude.hashWithSalt` certificateChain
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serverCertificateName
      `Prelude.hashWithSalt` certificateBody
      `Prelude.hashWithSalt` privateKey

instance Prelude.NFData UploadServerCertificate where
  rnf UploadServerCertificate' {..} =
    Prelude.rnf certificateChain
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serverCertificateName
      `Prelude.seq` Prelude.rnf certificateBody
      `Prelude.seq` Prelude.rnf privateKey

instance Data.ToHeaders UploadServerCertificate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UploadServerCertificate where
  toPath = Prelude.const "/"

instance Data.ToQuery UploadServerCertificate where
  toQuery UploadServerCertificate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("UploadServerCertificate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "CertificateChain" Data.=: certificateChain,
        "Path" Data.=: path,
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "ServerCertificateName"
          Data.=: serverCertificateName,
        "CertificateBody" Data.=: certificateBody,
        "PrivateKey" Data.=: privateKey
      ]

-- | Contains the response to a successful UploadServerCertificate request.
--
-- /See:/ 'newUploadServerCertificateResponse' smart constructor.
data UploadServerCertificateResponse = UploadServerCertificateResponse'
  { -- | The meta information of the uploaded server certificate without its
    -- certificate body, certificate chain, and private key.
    serverCertificateMetadata :: Prelude.Maybe ServerCertificateMetadata,
    -- | A list of tags that are attached to the new IAM server certificate. The
    -- returned list of tags is sorted by tag key. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadServerCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateMetadata', 'uploadServerCertificateResponse_serverCertificateMetadata' - The meta information of the uploaded server certificate without its
-- certificate body, certificate chain, and private key.
--
-- 'tags', 'uploadServerCertificateResponse_tags' - A list of tags that are attached to the new IAM server certificate. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'httpStatus', 'uploadServerCertificateResponse_httpStatus' - The response's http status code.
newUploadServerCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UploadServerCertificateResponse
newUploadServerCertificateResponse pHttpStatus_ =
  UploadServerCertificateResponse'
    { serverCertificateMetadata =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The meta information of the uploaded server certificate without its
-- certificate body, certificate chain, and private key.
uploadServerCertificateResponse_serverCertificateMetadata :: Lens.Lens' UploadServerCertificateResponse (Prelude.Maybe ServerCertificateMetadata)
uploadServerCertificateResponse_serverCertificateMetadata = Lens.lens (\UploadServerCertificateResponse' {serverCertificateMetadata} -> serverCertificateMetadata) (\s@UploadServerCertificateResponse' {} a -> s {serverCertificateMetadata = a} :: UploadServerCertificateResponse)

-- | A list of tags that are attached to the new IAM server certificate. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
uploadServerCertificateResponse_tags :: Lens.Lens' UploadServerCertificateResponse (Prelude.Maybe [Tag])
uploadServerCertificateResponse_tags = Lens.lens (\UploadServerCertificateResponse' {tags} -> tags) (\s@UploadServerCertificateResponse' {} a -> s {tags = a} :: UploadServerCertificateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
uploadServerCertificateResponse_httpStatus :: Lens.Lens' UploadServerCertificateResponse Prelude.Int
uploadServerCertificateResponse_httpStatus = Lens.lens (\UploadServerCertificateResponse' {httpStatus} -> httpStatus) (\s@UploadServerCertificateResponse' {} a -> s {httpStatus = a} :: UploadServerCertificateResponse)

instance
  Prelude.NFData
    UploadServerCertificateResponse
  where
  rnf UploadServerCertificateResponse' {..} =
    Prelude.rnf serverCertificateMetadata
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
