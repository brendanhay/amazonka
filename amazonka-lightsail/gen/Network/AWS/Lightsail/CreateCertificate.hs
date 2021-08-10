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
-- Module      : Network.AWS.Lightsail.CreateCertificate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an SSL\/TLS certificate for an Amazon Lightsail content delivery
-- network (CDN) distribution and a container service.
--
-- After the certificate is valid, use the
-- @AttachCertificateToDistribution@ action to use the certificate and its
-- domains with your distribution. Or use the @UpdateContainerService@
-- action to use the certificate and its domains with your container
-- service.
--
-- Only certificates created in the @us-east-1@ AWS Region can be attached
-- to Lightsail distributions. Lightsail distributions are global resources
-- that can reference an origin in any AWS Region, and distribute its
-- content globally. However, all distributions are located in the
-- @us-east-1@ Region.
module Network.AWS.Lightsail.CreateCertificate
  ( -- * Creating a Request
    CreateCertificate (..),
    newCreateCertificate,

    -- * Request Lenses
    createCertificate_subjectAlternativeNames,
    createCertificate_tags,
    createCertificate_certificateName,
    createCertificate_domainName,

    -- * Destructuring the Response
    CreateCertificateResponse (..),
    newCreateCertificateResponse,

    -- * Response Lenses
    createCertificateResponse_operations,
    createCertificateResponse_certificate,
    createCertificateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateCertificate' smart constructor.
data CreateCertificate = CreateCertificate'
  { -- | An array of strings that specify the alternate domains (e.g.,
    -- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
    -- certificate.
    --
    -- You can specify a maximum of nine alternate domains (in addition to the
    -- primary domain name).
    --
    -- Wildcard domain entries (e.g., @*.example.com@) are not supported.
    subjectAlternativeNames :: Prelude.Maybe [Prelude.Text],
    -- | The tag keys and optional values to add to the certificate during
    -- create.
    --
    -- Use the @TagResource@ action to tag a resource after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the certificate.
    certificateName :: Prelude.Text,
    -- | The domain name (e.g., @example.com@) for the certificate.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectAlternativeNames', 'createCertificate_subjectAlternativeNames' - An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
--
-- You can specify a maximum of nine alternate domains (in addition to the
-- primary domain name).
--
-- Wildcard domain entries (e.g., @*.example.com@) are not supported.
--
-- 'tags', 'createCertificate_tags' - The tag keys and optional values to add to the certificate during
-- create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
--
-- 'certificateName', 'createCertificate_certificateName' - The name for the certificate.
--
-- 'domainName', 'createCertificate_domainName' - The domain name (e.g., @example.com@) for the certificate.
newCreateCertificate ::
  -- | 'certificateName'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  CreateCertificate
newCreateCertificate pCertificateName_ pDomainName_ =
  CreateCertificate'
    { subjectAlternativeNames =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      certificateName = pCertificateName_,
      domainName = pDomainName_
    }

-- | An array of strings that specify the alternate domains (e.g.,
-- @example2.com@) and subdomains (e.g., @blog.example.com@) for the
-- certificate.
--
-- You can specify a maximum of nine alternate domains (in addition to the
-- primary domain name).
--
-- Wildcard domain entries (e.g., @*.example.com@) are not supported.
createCertificate_subjectAlternativeNames :: Lens.Lens' CreateCertificate (Prelude.Maybe [Prelude.Text])
createCertificate_subjectAlternativeNames = Lens.lens (\CreateCertificate' {subjectAlternativeNames} -> subjectAlternativeNames) (\s@CreateCertificate' {} a -> s {subjectAlternativeNames = a} :: CreateCertificate) Prelude.. Lens.mapping Lens._Coerce

-- | The tag keys and optional values to add to the certificate during
-- create.
--
-- Use the @TagResource@ action to tag a resource after it\'s created.
createCertificate_tags :: Lens.Lens' CreateCertificate (Prelude.Maybe [Tag])
createCertificate_tags = Lens.lens (\CreateCertificate' {tags} -> tags) (\s@CreateCertificate' {} a -> s {tags = a} :: CreateCertificate) Prelude.. Lens.mapping Lens._Coerce

-- | The name for the certificate.
createCertificate_certificateName :: Lens.Lens' CreateCertificate Prelude.Text
createCertificate_certificateName = Lens.lens (\CreateCertificate' {certificateName} -> certificateName) (\s@CreateCertificate' {} a -> s {certificateName = a} :: CreateCertificate)

-- | The domain name (e.g., @example.com@) for the certificate.
createCertificate_domainName :: Lens.Lens' CreateCertificate Prelude.Text
createCertificate_domainName = Lens.lens (\CreateCertificate' {domainName} -> domainName) (\s@CreateCertificate' {} a -> s {domainName = a} :: CreateCertificate)

instance Core.AWSRequest CreateCertificate where
  type
    AWSResponse CreateCertificate =
      CreateCertificateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCertificateResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "certificate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateCertificate

instance Prelude.NFData CreateCertificate

instance Core.ToHeaders CreateCertificate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateCertificate" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCertificate where
  toJSON CreateCertificate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("subjectAlternativeNames" Core..=)
              Prelude.<$> subjectAlternativeNames,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("certificateName" Core..= certificateName),
            Prelude.Just ("domainName" Core..= domainName)
          ]
      )

instance Core.ToPath CreateCertificate where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateCertificate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCertificateResponse' smart constructor.
data CreateCertificateResponse = CreateCertificateResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | An object that describes the certificate created.
    certificate :: Prelude.Maybe CertificateSummary,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCertificateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createCertificateResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'certificate', 'createCertificateResponse_certificate' - An object that describes the certificate created.
--
-- 'httpStatus', 'createCertificateResponse_httpStatus' - The response's http status code.
newCreateCertificateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCertificateResponse
newCreateCertificateResponse pHttpStatus_ =
  CreateCertificateResponse'
    { operations =
        Prelude.Nothing,
      certificate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createCertificateResponse_operations :: Lens.Lens' CreateCertificateResponse (Prelude.Maybe [Operation])
createCertificateResponse_operations = Lens.lens (\CreateCertificateResponse' {operations} -> operations) (\s@CreateCertificateResponse' {} a -> s {operations = a} :: CreateCertificateResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An object that describes the certificate created.
createCertificateResponse_certificate :: Lens.Lens' CreateCertificateResponse (Prelude.Maybe CertificateSummary)
createCertificateResponse_certificate = Lens.lens (\CreateCertificateResponse' {certificate} -> certificate) (\s@CreateCertificateResponse' {} a -> s {certificate = a} :: CreateCertificateResponse)

-- | The response's http status code.
createCertificateResponse_httpStatus :: Lens.Lens' CreateCertificateResponse Prelude.Int
createCertificateResponse_httpStatus = Lens.lens (\CreateCertificateResponse' {httpStatus} -> httpStatus) (\s@CreateCertificateResponse' {} a -> s {httpStatus = a} :: CreateCertificateResponse)

instance Prelude.NFData CreateCertificateResponse
