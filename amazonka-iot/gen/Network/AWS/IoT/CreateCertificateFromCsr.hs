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
-- Module      : Network.AWS.IoT.CreateCertificateFromCsr
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an X.509 certificate using the specified certificate signing
-- request.
--
-- __Note:__ The CSR must include a public key that is either an RSA key
-- with a length of at least 2048 bits or an ECC key from NIST P-256 or
-- NIST P-384 curves.
--
-- __Note:__ Reusing the same certificate signing request (CSR) results in
-- a distinct certificate.
--
-- You can create multiple certificates in a batch by creating a directory,
-- copying multiple .csr files into that directory, and then specifying
-- that directory on the command line. The following commands show how to
-- create a batch of certificates given a batch of CSRs.
--
-- Assuming a set of CSRs are located inside of the directory
-- my-csr-directory:
--
-- On Linux and OS X, the command is:
--
-- \$ ls my-csr-directory\/ | xargs -I {} aws iot
-- create-certificate-from-csr --certificate-signing-request
-- file:\/\/my-csr-directory\/{}
--
-- This command lists all of the CSRs in my-csr-directory and pipes each
-- CSR file name to the aws iot create-certificate-from-csr AWS CLI command
-- to create a certificate for the corresponding CSR.
--
-- The aws iot create-certificate-from-csr part of the command can also be
-- run in parallel to speed up the certificate creation process:
--
-- \$ ls my-csr-directory\/ | xargs -P 10 -I {} aws iot
-- create-certificate-from-csr --certificate-signing-request
-- file:\/\/my-csr-directory\/{}
--
-- On Windows PowerShell, the command to create certificates for all CSRs
-- in my-csr-directory is:
--
-- > ls -Name my-csr-directory | %{aws iot create-certificate-from-csr
-- --certificate-signing-request file:\/\/my-csr-directory\/$_}
--
-- On a Windows command prompt, the command to create certificates for all
-- CSRs in my-csr-directory is:
--
-- > forfiles \/p my-csr-directory \/c \"cmd \/c aws iot
-- create-certificate-from-csr --certificate-signing-request
-- file:\/\/\@path\"
module Network.AWS.IoT.CreateCertificateFromCsr
  ( -- * Creating a Request
    CreateCertificateFromCsr (..),
    newCreateCertificateFromCsr,

    -- * Request Lenses
    createCertificateFromCsr_setAsActive,
    createCertificateFromCsr_certificateSigningRequest,

    -- * Destructuring the Response
    CreateCertificateFromCsrResponse (..),
    newCreateCertificateFromCsrResponse,

    -- * Response Lenses
    createCertificateFromCsrResponse_certificateArn,
    createCertificateFromCsrResponse_certificateId,
    createCertificateFromCsrResponse_certificatePem,
    createCertificateFromCsrResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the CreateCertificateFromCsr operation.
--
-- /See:/ 'newCreateCertificateFromCsr' smart constructor.
data CreateCertificateFromCsr = CreateCertificateFromCsr'
  { -- | Specifies whether the certificate is active.
    setAsActive :: Prelude.Maybe Prelude.Bool,
    -- | The certificate signing request (CSR).
    certificateSigningRequest :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateFromCsr' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setAsActive', 'createCertificateFromCsr_setAsActive' - Specifies whether the certificate is active.
--
-- 'certificateSigningRequest', 'createCertificateFromCsr_certificateSigningRequest' - The certificate signing request (CSR).
newCreateCertificateFromCsr ::
  -- | 'certificateSigningRequest'
  Prelude.Text ->
  CreateCertificateFromCsr
newCreateCertificateFromCsr
  pCertificateSigningRequest_ =
    CreateCertificateFromCsr'
      { setAsActive =
          Prelude.Nothing,
        certificateSigningRequest =
          pCertificateSigningRequest_
      }

-- | Specifies whether the certificate is active.
createCertificateFromCsr_setAsActive :: Lens.Lens' CreateCertificateFromCsr (Prelude.Maybe Prelude.Bool)
createCertificateFromCsr_setAsActive = Lens.lens (\CreateCertificateFromCsr' {setAsActive} -> setAsActive) (\s@CreateCertificateFromCsr' {} a -> s {setAsActive = a} :: CreateCertificateFromCsr)

-- | The certificate signing request (CSR).
createCertificateFromCsr_certificateSigningRequest :: Lens.Lens' CreateCertificateFromCsr Prelude.Text
createCertificateFromCsr_certificateSigningRequest = Lens.lens (\CreateCertificateFromCsr' {certificateSigningRequest} -> certificateSigningRequest) (\s@CreateCertificateFromCsr' {} a -> s {certificateSigningRequest = a} :: CreateCertificateFromCsr)

instance Prelude.AWSRequest CreateCertificateFromCsr where
  type
    Rs CreateCertificateFromCsr =
      CreateCertificateFromCsrResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCertificateFromCsrResponse'
            Prelude.<$> (x Prelude..?> "certificateArn")
            Prelude.<*> (x Prelude..?> "certificateId")
            Prelude.<*> (x Prelude..?> "certificatePem")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCertificateFromCsr

instance Prelude.NFData CreateCertificateFromCsr

instance Prelude.ToHeaders CreateCertificateFromCsr where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON CreateCertificateFromCsr where
  toJSON CreateCertificateFromCsr' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "certificateSigningRequest"
                  Prelude..= certificateSigningRequest
              )
          ]
      )

instance Prelude.ToPath CreateCertificateFromCsr where
  toPath = Prelude.const "/certificates"

instance Prelude.ToQuery CreateCertificateFromCsr where
  toQuery CreateCertificateFromCsr' {..} =
    Prelude.mconcat
      ["setAsActive" Prelude.=: setAsActive]

-- | The output from the CreateCertificateFromCsr operation.
--
-- /See:/ 'newCreateCertificateFromCsrResponse' smart constructor.
data CreateCertificateFromCsrResponse = CreateCertificateFromCsrResponse'
  { -- | The Amazon Resource Name (ARN) of the certificate. You can use the ARN
    -- as a principal for policy operations.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the certificate. Certificate management operations only take a
    -- certificateId.
    certificateId :: Prelude.Maybe Prelude.Text,
    -- | The certificate data, in PEM format.
    certificatePem :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateFromCsrResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'createCertificateFromCsrResponse_certificateArn' - The Amazon Resource Name (ARN) of the certificate. You can use the ARN
-- as a principal for policy operations.
--
-- 'certificateId', 'createCertificateFromCsrResponse_certificateId' - The ID of the certificate. Certificate management operations only take a
-- certificateId.
--
-- 'certificatePem', 'createCertificateFromCsrResponse_certificatePem' - The certificate data, in PEM format.
--
-- 'httpStatus', 'createCertificateFromCsrResponse_httpStatus' - The response's http status code.
newCreateCertificateFromCsrResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCertificateFromCsrResponse
newCreateCertificateFromCsrResponse pHttpStatus_ =
  CreateCertificateFromCsrResponse'
    { certificateArn =
        Prelude.Nothing,
      certificateId = Prelude.Nothing,
      certificatePem = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the certificate. You can use the ARN
-- as a principal for policy operations.
createCertificateFromCsrResponse_certificateArn :: Lens.Lens' CreateCertificateFromCsrResponse (Prelude.Maybe Prelude.Text)
createCertificateFromCsrResponse_certificateArn = Lens.lens (\CreateCertificateFromCsrResponse' {certificateArn} -> certificateArn) (\s@CreateCertificateFromCsrResponse' {} a -> s {certificateArn = a} :: CreateCertificateFromCsrResponse)

-- | The ID of the certificate. Certificate management operations only take a
-- certificateId.
createCertificateFromCsrResponse_certificateId :: Lens.Lens' CreateCertificateFromCsrResponse (Prelude.Maybe Prelude.Text)
createCertificateFromCsrResponse_certificateId = Lens.lens (\CreateCertificateFromCsrResponse' {certificateId} -> certificateId) (\s@CreateCertificateFromCsrResponse' {} a -> s {certificateId = a} :: CreateCertificateFromCsrResponse)

-- | The certificate data, in PEM format.
createCertificateFromCsrResponse_certificatePem :: Lens.Lens' CreateCertificateFromCsrResponse (Prelude.Maybe Prelude.Text)
createCertificateFromCsrResponse_certificatePem = Lens.lens (\CreateCertificateFromCsrResponse' {certificatePem} -> certificatePem) (\s@CreateCertificateFromCsrResponse' {} a -> s {certificatePem = a} :: CreateCertificateFromCsrResponse)

-- | The response's http status code.
createCertificateFromCsrResponse_httpStatus :: Lens.Lens' CreateCertificateFromCsrResponse Prelude.Int
createCertificateFromCsrResponse_httpStatus = Lens.lens (\CreateCertificateFromCsrResponse' {httpStatus} -> httpStatus) (\s@CreateCertificateFromCsrResponse' {} a -> s {httpStatus = a} :: CreateCertificateFromCsrResponse)

instance
  Prelude.NFData
    CreateCertificateFromCsrResponse
