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
-- Module      : Network.AWS.WorkMail.CreateOrganization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon WorkMail organization. Optionally, you can choose
-- to associate an existing AWS Directory Service directory with your
-- organization. If an AWS Directory Service directory ID is specified, the
-- organization alias must match the directory alias. If you choose not to
-- associate an existing directory with your organization, then we create a
-- new Amazon WorkMail directory for you. For more information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/add_new_organization.html Adding an organization>
-- in the /Amazon WorkMail Administrator Guide/.
--
-- You can associate multiple email domains with an organization, then set
-- your default email domain from the Amazon WorkMail console. You can also
-- associate a domain that is managed in an Amazon Route 53 public hosted
-- zone. For more information, see
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain>
-- and
-- <https://docs.aws.amazon.com/workmail/latest/adminguide/default_domain.html Choosing the default domain>
-- in the /Amazon WorkMail Administrator Guide/.
--
-- Optionally, you can use a customer managed master key from AWS Key
-- Management Service (AWS KMS) to encrypt email for your organization. If
-- you don\'t associate an AWS KMS key, Amazon WorkMail creates a default
-- AWS managed master key for you.
module Network.AWS.WorkMail.CreateOrganization
  ( -- * Creating a Request
    CreateOrganization (..),
    newCreateOrganization,

    -- * Request Lenses
    createOrganization_enableInteroperability,
    createOrganization_domains,
    createOrganization_kmsKeyArn,
    createOrganization_directoryId,
    createOrganization_clientToken,
    createOrganization_alias,

    -- * Destructuring the Response
    CreateOrganizationResponse (..),
    newCreateOrganizationResponse,

    -- * Response Lenses
    createOrganizationResponse_organizationId,
    createOrganizationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newCreateOrganization' smart constructor.
data CreateOrganization = CreateOrganization'
  { -- | When @true@, allows organization interoperability between Amazon
    -- WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD
    -- Connector directory ID is included in the request.
    enableInteroperability :: Core.Maybe Core.Bool,
    -- | The email domains to associate with the organization.
    domains :: Core.Maybe [Domain],
    -- | The Amazon Resource Name (ARN) of a customer managed master key from AWS
    -- KMS.
    kmsKeyArn :: Core.Maybe Core.Text,
    -- | The AWS Directory Service directory ID.
    directoryId :: Core.Maybe Core.Text,
    -- | The idempotency token associated with the request.
    clientToken :: Core.Maybe Core.Text,
    -- | The organization alias.
    alias :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateOrganization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableInteroperability', 'createOrganization_enableInteroperability' - When @true@, allows organization interoperability between Amazon
-- WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD
-- Connector directory ID is included in the request.
--
-- 'domains', 'createOrganization_domains' - The email domains to associate with the organization.
--
-- 'kmsKeyArn', 'createOrganization_kmsKeyArn' - The Amazon Resource Name (ARN) of a customer managed master key from AWS
-- KMS.
--
-- 'directoryId', 'createOrganization_directoryId' - The AWS Directory Service directory ID.
--
-- 'clientToken', 'createOrganization_clientToken' - The idempotency token associated with the request.
--
-- 'alias', 'createOrganization_alias' - The organization alias.
newCreateOrganization ::
  -- | 'alias'
  Core.Text ->
  CreateOrganization
newCreateOrganization pAlias_ =
  CreateOrganization'
    { enableInteroperability =
        Core.Nothing,
      domains = Core.Nothing,
      kmsKeyArn = Core.Nothing,
      directoryId = Core.Nothing,
      clientToken = Core.Nothing,
      alias = pAlias_
    }

-- | When @true@, allows organization interoperability between Amazon
-- WorkMail and Microsoft Exchange. Can only be set to @true@ if an AD
-- Connector directory ID is included in the request.
createOrganization_enableInteroperability :: Lens.Lens' CreateOrganization (Core.Maybe Core.Bool)
createOrganization_enableInteroperability = Lens.lens (\CreateOrganization' {enableInteroperability} -> enableInteroperability) (\s@CreateOrganization' {} a -> s {enableInteroperability = a} :: CreateOrganization)

-- | The email domains to associate with the organization.
createOrganization_domains :: Lens.Lens' CreateOrganization (Core.Maybe [Domain])
createOrganization_domains = Lens.lens (\CreateOrganization' {domains} -> domains) (\s@CreateOrganization' {} a -> s {domains = a} :: CreateOrganization) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of a customer managed master key from AWS
-- KMS.
createOrganization_kmsKeyArn :: Lens.Lens' CreateOrganization (Core.Maybe Core.Text)
createOrganization_kmsKeyArn = Lens.lens (\CreateOrganization' {kmsKeyArn} -> kmsKeyArn) (\s@CreateOrganization' {} a -> s {kmsKeyArn = a} :: CreateOrganization)

-- | The AWS Directory Service directory ID.
createOrganization_directoryId :: Lens.Lens' CreateOrganization (Core.Maybe Core.Text)
createOrganization_directoryId = Lens.lens (\CreateOrganization' {directoryId} -> directoryId) (\s@CreateOrganization' {} a -> s {directoryId = a} :: CreateOrganization)

-- | The idempotency token associated with the request.
createOrganization_clientToken :: Lens.Lens' CreateOrganization (Core.Maybe Core.Text)
createOrganization_clientToken = Lens.lens (\CreateOrganization' {clientToken} -> clientToken) (\s@CreateOrganization' {} a -> s {clientToken = a} :: CreateOrganization)

-- | The organization alias.
createOrganization_alias :: Lens.Lens' CreateOrganization Core.Text
createOrganization_alias = Lens.lens (\CreateOrganization' {alias} -> alias) (\s@CreateOrganization' {} a -> s {alias = a} :: CreateOrganization)

instance Core.AWSRequest CreateOrganization where
  type
    AWSResponse CreateOrganization =
      CreateOrganizationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOrganizationResponse'
            Core.<$> (x Core..?> "OrganizationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateOrganization

instance Core.NFData CreateOrganization

instance Core.ToHeaders CreateOrganization where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkMailService.CreateOrganization" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateOrganization where
  toJSON CreateOrganization' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EnableInteroperability" Core..=)
              Core.<$> enableInteroperability,
            ("Domains" Core..=) Core.<$> domains,
            ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn,
            ("DirectoryId" Core..=) Core.<$> directoryId,
            ("ClientToken" Core..=) Core.<$> clientToken,
            Core.Just ("Alias" Core..= alias)
          ]
      )

instance Core.ToPath CreateOrganization where
  toPath = Core.const "/"

instance Core.ToQuery CreateOrganization where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateOrganizationResponse' smart constructor.
data CreateOrganizationResponse = CreateOrganizationResponse'
  { -- | The organization ID.
    organizationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateOrganizationResponse
newCreateOrganizationResponse pHttpStatus_ =
  CreateOrganizationResponse'
    { organizationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The organization ID.
createOrganizationResponse_organizationId :: Lens.Lens' CreateOrganizationResponse (Core.Maybe Core.Text)
createOrganizationResponse_organizationId = Lens.lens (\CreateOrganizationResponse' {organizationId} -> organizationId) (\s@CreateOrganizationResponse' {} a -> s {organizationId = a} :: CreateOrganizationResponse)

-- | The response's http status code.
createOrganizationResponse_httpStatus :: Lens.Lens' CreateOrganizationResponse Core.Int
createOrganizationResponse_httpStatus = Lens.lens (\CreateOrganizationResponse' {httpStatus} -> httpStatus) (\s@CreateOrganizationResponse' {} a -> s {httpStatus = a} :: CreateOrganizationResponse)

instance Core.NFData CreateOrganizationResponse
