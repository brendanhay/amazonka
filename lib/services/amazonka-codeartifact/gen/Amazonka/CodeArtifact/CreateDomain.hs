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
-- Module      : Amazonka.CodeArtifact.CreateDomain
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a domain. CodeArtifact /domains/ make it easier to manage
-- multiple repositories across an organization. You can use a domain to
-- apply permissions across many repositories owned by different Amazon Web
-- Services accounts. An asset is stored only once in a domain, even if
-- it\'s in multiple repositories.
--
-- Although you can have multiple domains, we recommend a single production
-- domain that contains all published artifacts so that your development
-- teams can find and share packages. You can use a second pre-production
-- domain to test changes to the production domain configuration.
module Amazonka.CodeArtifact.CreateDomain
  ( -- * Creating a Request
    CreateDomain (..),
    newCreateDomain,

    -- * Request Lenses
    createDomain_tags,
    createDomain_encryptionKey,
    createDomain_domain,

    -- * Destructuring the Response
    CreateDomainResponse (..),
    newCreateDomainResponse,

    -- * Response Lenses
    createDomainResponse_domain,
    createDomainResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDomain' smart constructor.
data CreateDomain = CreateDomain'
  { -- | One or more tag key-value pairs for the domain.
    tags :: Prelude.Maybe [Tag],
    -- | The encryption key for the domain. This is used to encrypt content
    -- stored in a domain. An encryption key can be a key ID, a key Amazon
    -- Resource Name (ARN), a key alias, or a key alias ARN. To specify an
    -- @encryptionKey@, your IAM role must have @kms:DescribeKey@ and
    -- @kms:CreateGrant@ permissions on the encryption key that is used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestSyntax DescribeKey>
    -- in the /Key Management Service API Reference/ and
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html Key Management Service API Permissions Reference>
    -- in the /Key Management Service Developer Guide/.
    --
    -- CodeArtifact supports only symmetric CMKs. Do not associate an
    -- asymmetric CMK with your domain. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
    -- in the /Key Management Service Developer Guide/.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain to create. All domain names in an Amazon Web
    -- Services Region that are in the same Amazon Web Services account must be
    -- unique. The domain name is used as the prefix in DNS hostnames. Do not
    -- use sensitive information in a domain name because it is publicly
    -- discoverable.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createDomain_tags' - One or more tag key-value pairs for the domain.
--
-- 'encryptionKey', 'createDomain_encryptionKey' - The encryption key for the domain. This is used to encrypt content
-- stored in a domain. An encryption key can be a key ID, a key Amazon
-- Resource Name (ARN), a key alias, or a key alias ARN. To specify an
-- @encryptionKey@, your IAM role must have @kms:DescribeKey@ and
-- @kms:CreateGrant@ permissions on the encryption key that is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestSyntax DescribeKey>
-- in the /Key Management Service API Reference/ and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html Key Management Service API Permissions Reference>
-- in the /Key Management Service Developer Guide/.
--
-- CodeArtifact supports only symmetric CMKs. Do not associate an
-- asymmetric CMK with your domain. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
-- in the /Key Management Service Developer Guide/.
--
-- 'domain', 'createDomain_domain' - The name of the domain to create. All domain names in an Amazon Web
-- Services Region that are in the same Amazon Web Services account must be
-- unique. The domain name is used as the prefix in DNS hostnames. Do not
-- use sensitive information in a domain name because it is publicly
-- discoverable.
newCreateDomain ::
  -- | 'domain'
  Prelude.Text ->
  CreateDomain
newCreateDomain pDomain_ =
  CreateDomain'
    { tags = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      domain = pDomain_
    }

-- | One or more tag key-value pairs for the domain.
createDomain_tags :: Lens.Lens' CreateDomain (Prelude.Maybe [Tag])
createDomain_tags = Lens.lens (\CreateDomain' {tags} -> tags) (\s@CreateDomain' {} a -> s {tags = a} :: CreateDomain) Prelude.. Lens.mapping Lens.coerced

-- | The encryption key for the domain. This is used to encrypt content
-- stored in a domain. An encryption key can be a key ID, a key Amazon
-- Resource Name (ARN), a key alias, or a key alias ARN. To specify an
-- @encryptionKey@, your IAM role must have @kms:DescribeKey@ and
-- @kms:CreateGrant@ permissions on the encryption key that is used. For
-- more information, see
-- <https://docs.aws.amazon.com/kms/latest/APIReference/API_DescribeKey.html#API_DescribeKey_RequestSyntax DescribeKey>
-- in the /Key Management Service API Reference/ and
-- <https://docs.aws.amazon.com/kms/latest/developerguide/kms-api-permissions-reference.html Key Management Service API Permissions Reference>
-- in the /Key Management Service Developer Guide/.
--
-- CodeArtifact supports only symmetric CMKs. Do not associate an
-- asymmetric CMK with your domain. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/symmetric-asymmetric.html Using symmetric and asymmetric keys>
-- in the /Key Management Service Developer Guide/.
createDomain_encryptionKey :: Lens.Lens' CreateDomain (Prelude.Maybe Prelude.Text)
createDomain_encryptionKey = Lens.lens (\CreateDomain' {encryptionKey} -> encryptionKey) (\s@CreateDomain' {} a -> s {encryptionKey = a} :: CreateDomain)

-- | The name of the domain to create. All domain names in an Amazon Web
-- Services Region that are in the same Amazon Web Services account must be
-- unique. The domain name is used as the prefix in DNS hostnames. Do not
-- use sensitive information in a domain name because it is publicly
-- discoverable.
createDomain_domain :: Lens.Lens' CreateDomain Prelude.Text
createDomain_domain = Lens.lens (\CreateDomain' {domain} -> domain) (\s@CreateDomain' {} a -> s {domain = a} :: CreateDomain)

instance Core.AWSRequest CreateDomain where
  type AWSResponse CreateDomain = CreateDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDomainResponse'
            Prelude.<$> (x Data..?> "domain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDomain where
  hashWithSalt _salt CreateDomain' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` domain

instance Prelude.NFData CreateDomain where
  rnf CreateDomain' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf domain

instance Data.ToHeaders CreateDomain where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDomain where
  toJSON CreateDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("encryptionKey" Data..=) Prelude.<$> encryptionKey
          ]
      )

instance Data.ToPath CreateDomain where
  toPath = Prelude.const "/v1/domain"

instance Data.ToQuery CreateDomain where
  toQuery CreateDomain' {..} =
    Prelude.mconcat ["domain" Data.=: domain]

-- | /See:/ 'newCreateDomainResponse' smart constructor.
data CreateDomainResponse = CreateDomainResponse'
  { -- | Contains information about the created domain after processing the
    -- request.
    domain :: Prelude.Maybe DomainDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'createDomainResponse_domain' - Contains information about the created domain after processing the
-- request.
--
-- 'httpStatus', 'createDomainResponse_httpStatus' - The response's http status code.
newCreateDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDomainResponse
newCreateDomainResponse pHttpStatus_ =
  CreateDomainResponse'
    { domain = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains information about the created domain after processing the
-- request.
createDomainResponse_domain :: Lens.Lens' CreateDomainResponse (Prelude.Maybe DomainDescription)
createDomainResponse_domain = Lens.lens (\CreateDomainResponse' {domain} -> domain) (\s@CreateDomainResponse' {} a -> s {domain = a} :: CreateDomainResponse)

-- | The response's http status code.
createDomainResponse_httpStatus :: Lens.Lens' CreateDomainResponse Prelude.Int
createDomainResponse_httpStatus = Lens.lens (\CreateDomainResponse' {httpStatus} -> httpStatus) (\s@CreateDomainResponse' {} a -> s {httpStatus = a} :: CreateDomainResponse)

instance Prelude.NFData CreateDomainResponse where
  rnf CreateDomainResponse' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf httpStatus
