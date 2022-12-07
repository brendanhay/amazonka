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
-- Module      : Amazonka.WorkMail.CreateOrganization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new WorkMail organization. Optionally, you can choose to
-- associate an existing AWS Directory Service directory with your
-- organization. If an AWS Directory Service directory ID is specified, the
-- organization alias must match the directory alias. If you choose not to
-- associate an existing directory with your organization, then we create a
-- new WorkMail directory for you. For more information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/add_new_organization.html Adding an organization>
-- in the /WorkMail Administrator Guide/.
--
-- You can associate multiple email domains with an organization, then
-- choose your default email domain from the WorkMail console. You can also
-- associate a domain that is managed in an Amazon Route 53 public hosted
-- zone. For more information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain>
-- and
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/default_domain.html Choosing the default domain>
-- in the /WorkMail Administrator Guide/.
--
-- Optionally, you can use a customer managed key from AWS Key Management
-- Service (AWS KMS) to encrypt email for your organization. If you don\'t
-- associate an AWS KMS key, WorkMail creates a default, AWS managed key
-- for you.
module Amazonka.WorkMail.CreateOrganization
  ( -- * Creating a Request
    CreateOrganization (..),
    newCreateOrganization,

    -- * Request Lenses
    createOrganization_directoryId,
    createOrganization_domains,
    createOrganization_clientToken,
    createOrganization_kmsKeyArn,
    createOrganization_enableInteroperability,
    createOrganization_alias,

    -- * Destructuring the Response
    CreateOrganizationResponse (..),
    newCreateOrganizationResponse,

    -- * Response Lenses
    createOrganizationResponse_organizationId,
    createOrganizationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkMail.Types

-- | /See:/ 'newCreateOrganization' smart constructor.
data CreateOrganization = CreateOrganization'
  { -- | The AWS Directory Service directory ID.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The email domains to associate with the organization.
    domains :: Prelude.Maybe [Domain],
    -- | The idempotency token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a customer managed key from AWS KMS.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | When @true@, allows organization interoperability between WorkMail and
    -- Microsoft Exchange. If @true@, you must include a AD Connector directory
    -- ID in the request.
    enableInteroperability :: Prelude.Maybe Prelude.Bool,
    -- | The organization alias.
    alias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'createOrganization_directoryId' - The AWS Directory Service directory ID.
--
-- 'domains', 'createOrganization_domains' - The email domains to associate with the organization.
--
-- 'clientToken', 'createOrganization_clientToken' - The idempotency token associated with the request.
--
-- 'kmsKeyArn', 'createOrganization_kmsKeyArn' - The Amazon Resource Name (ARN) of a customer managed key from AWS KMS.
--
-- 'enableInteroperability', 'createOrganization_enableInteroperability' - When @true@, allows organization interoperability between WorkMail and
-- Microsoft Exchange. If @true@, you must include a AD Connector directory
-- ID in the request.
--
-- 'alias', 'createOrganization_alias' - The organization alias.
newCreateOrganization ::
  -- | 'alias'
  Prelude.Text ->
  CreateOrganization
newCreateOrganization pAlias_ =
  CreateOrganization'
    { directoryId = Prelude.Nothing,
      domains = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing,
      enableInteroperability = Prelude.Nothing,
      alias = pAlias_
    }

-- | The AWS Directory Service directory ID.
createOrganization_directoryId :: Lens.Lens' CreateOrganization (Prelude.Maybe Prelude.Text)
createOrganization_directoryId = Lens.lens (\CreateOrganization' {directoryId} -> directoryId) (\s@CreateOrganization' {} a -> s {directoryId = a} :: CreateOrganization)

-- | The email domains to associate with the organization.
createOrganization_domains :: Lens.Lens' CreateOrganization (Prelude.Maybe [Domain])
createOrganization_domains = Lens.lens (\CreateOrganization' {domains} -> domains) (\s@CreateOrganization' {} a -> s {domains = a} :: CreateOrganization) Prelude.. Lens.mapping Lens.coerced

-- | The idempotency token associated with the request.
createOrganization_clientToken :: Lens.Lens' CreateOrganization (Prelude.Maybe Prelude.Text)
createOrganization_clientToken = Lens.lens (\CreateOrganization' {clientToken} -> clientToken) (\s@CreateOrganization' {} a -> s {clientToken = a} :: CreateOrganization)

-- | The Amazon Resource Name (ARN) of a customer managed key from AWS KMS.
createOrganization_kmsKeyArn :: Lens.Lens' CreateOrganization (Prelude.Maybe Prelude.Text)
createOrganization_kmsKeyArn = Lens.lens (\CreateOrganization' {kmsKeyArn} -> kmsKeyArn) (\s@CreateOrganization' {} a -> s {kmsKeyArn = a} :: CreateOrganization)

-- | When @true@, allows organization interoperability between WorkMail and
-- Microsoft Exchange. If @true@, you must include a AD Connector directory
-- ID in the request.
createOrganization_enableInteroperability :: Lens.Lens' CreateOrganization (Prelude.Maybe Prelude.Bool)
createOrganization_enableInteroperability = Lens.lens (\CreateOrganization' {enableInteroperability} -> enableInteroperability) (\s@CreateOrganization' {} a -> s {enableInteroperability = a} :: CreateOrganization)

-- | The organization alias.
createOrganization_alias :: Lens.Lens' CreateOrganization Prelude.Text
createOrganization_alias = Lens.lens (\CreateOrganization' {alias} -> alias) (\s@CreateOrganization' {} a -> s {alias = a} :: CreateOrganization)

instance Core.AWSRequest CreateOrganization where
  type
    AWSResponse CreateOrganization =
      CreateOrganizationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOrganizationResponse'
            Prelude.<$> (x Data..?> "OrganizationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOrganization where
  hashWithSalt _salt CreateOrganization' {..} =
    _salt `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` domains
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` enableInteroperability
      `Prelude.hashWithSalt` alias

instance Prelude.NFData CreateOrganization where
  rnf CreateOrganization' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf domains
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf enableInteroperability
      `Prelude.seq` Prelude.rnf alias

instance Data.ToHeaders CreateOrganization where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkMailService.CreateOrganization" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateOrganization where
  toJSON CreateOrganization' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DirectoryId" Data..=) Prelude.<$> directoryId,
            ("Domains" Data..=) Prelude.<$> domains,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("EnableInteroperability" Data..=)
              Prelude.<$> enableInteroperability,
            Prelude.Just ("Alias" Data..= alias)
          ]
      )

instance Data.ToPath CreateOrganization where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateOrganization where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { -- | The organization ID.
    organizationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOrganizationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'createOrganizationResponse_organizationId' - The organization ID.
--
-- 'httpStatus', 'createOrganizationResponse_httpStatus' - The response's http status code.
newCreateOrganizationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOrganizationResponse
newCreateOrganizationResponse pHttpStatus_ =
  CreateOrganizationResponse'
    { organizationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The organization ID.
createOrganizationResponse_organizationId :: Lens.Lens' CreateOrganizationResponse (Prelude.Maybe Prelude.Text)
createOrganizationResponse_organizationId = Lens.lens (\CreateOrganizationResponse' {organizationId} -> organizationId) (\s@CreateOrganizationResponse' {} a -> s {organizationId = a} :: CreateOrganizationResponse)

-- | The response's http status code.
createOrganizationResponse_httpStatus :: Lens.Lens' CreateOrganizationResponse Prelude.Int
createOrganizationResponse_httpStatus = Lens.lens (\CreateOrganizationResponse' {httpStatus} -> httpStatus) (\s@CreateOrganizationResponse' {} a -> s {httpStatus = a} :: CreateOrganizationResponse)

instance Prelude.NFData CreateOrganizationResponse where
  rnf CreateOrganizationResponse' {..} =
    Prelude.rnf organizationId
      `Prelude.seq` Prelude.rnf httpStatus
