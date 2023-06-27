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
-- Module      : Amazonka.VPCLattice.CreateService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a service. A service is any software application that can run on
-- instances containers, or serverless functions within an account or
-- virtual private cloud (VPC).
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc-lattice/latest/ug/services.html Services>
-- in the /Amazon VPC Lattice User Guide/.
module Amazonka.VPCLattice.CreateService
  ( -- * Creating a Request
    CreateService (..),
    newCreateService,

    -- * Request Lenses
    createService_authType,
    createService_certificateArn,
    createService_clientToken,
    createService_customDomainName,
    createService_tags,
    createService_name,

    -- * Destructuring the Response
    CreateServiceResponse (..),
    newCreateServiceResponse,

    -- * Response Lenses
    createServiceResponse_arn,
    createServiceResponse_authType,
    createServiceResponse_certificateArn,
    createServiceResponse_customDomainName,
    createServiceResponse_dnsEntry,
    createServiceResponse_id,
    createServiceResponse_name,
    createServiceResponse_status,
    createServiceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateService' smart constructor.
data CreateService = CreateService'
  { -- | The type of IAM policy.
    --
    -- -   @NONE@: The resource does not use an IAM policy. This is the
    --     default.
    --
    -- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
    --     auth is enabled and an auth policy is required.
    authType :: Prelude.Maybe AuthType,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The tags for the service.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the service. The name must be unique within the account. The
    -- valid characters are a-z, 0-9, and hyphens (-). You can\'t use a hyphen
    -- as the first or last character, or immediately after another hyphen.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authType', 'createService_authType' - The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
--
-- 'certificateArn', 'createService_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'clientToken', 'createService_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'customDomainName', 'createService_customDomainName' - The custom domain name of the service.
--
-- 'tags', 'createService_tags' - The tags for the service.
--
-- 'name', 'createService_name' - The name of the service. The name must be unique within the account. The
-- valid characters are a-z, 0-9, and hyphens (-). You can\'t use a hyphen
-- as the first or last character, or immediately after another hyphen.
newCreateService ::
  -- | 'name'
  Prelude.Text ->
  CreateService
newCreateService pName_ =
  CreateService'
    { authType = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | The type of IAM policy.
--
-- -   @NONE@: The resource does not use an IAM policy. This is the
--     default.
--
-- -   @AWS_IAM@: The resource uses an IAM policy. When this type is used,
--     auth is enabled and an auth policy is required.
createService_authType :: Lens.Lens' CreateService (Prelude.Maybe AuthType)
createService_authType = Lens.lens (\CreateService' {authType} -> authType) (\s@CreateService' {} a -> s {authType = a} :: CreateService)

-- | The Amazon Resource Name (ARN) of the certificate.
createService_certificateArn :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_certificateArn = Lens.lens (\CreateService' {certificateArn} -> certificateArn) (\s@CreateService' {} a -> s {certificateArn = a} :: CreateService)

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createService_clientToken :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_clientToken = Lens.lens (\CreateService' {clientToken} -> clientToken) (\s@CreateService' {} a -> s {clientToken = a} :: CreateService)

-- | The custom domain name of the service.
createService_customDomainName :: Lens.Lens' CreateService (Prelude.Maybe Prelude.Text)
createService_customDomainName = Lens.lens (\CreateService' {customDomainName} -> customDomainName) (\s@CreateService' {} a -> s {customDomainName = a} :: CreateService)

-- | The tags for the service.
createService_tags :: Lens.Lens' CreateService (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createService_tags = Lens.lens (\CreateService' {tags} -> tags) (\s@CreateService' {} a -> s {tags = a} :: CreateService) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service. The name must be unique within the account. The
-- valid characters are a-z, 0-9, and hyphens (-). You can\'t use a hyphen
-- as the first or last character, or immediately after another hyphen.
createService_name :: Lens.Lens' CreateService Prelude.Text
createService_name = Lens.lens (\CreateService' {name} -> name) (\s@CreateService' {} a -> s {name = a} :: CreateService)

instance Core.AWSRequest CreateService where
  type
    AWSResponse CreateService =
      CreateServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "authType")
            Prelude.<*> (x Data..?> "certificateArn")
            Prelude.<*> (x Data..?> "customDomainName")
            Prelude.<*> (x Data..?> "dnsEntry")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateService where
  hashWithSalt _salt CreateService' {..} =
    _salt
      `Prelude.hashWithSalt` authType
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` customDomainName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateService where
  rnf CreateService' {..} =
    Prelude.rnf authType
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateService where
  toJSON CreateService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authType" Data..=) Prelude.<$> authType,
            ("certificateArn" Data..=)
              Prelude.<$> certificateArn,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("customDomainName" Data..=)
              Prelude.<$> customDomainName,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateService where
  toPath = Prelude.const "/services"

instance Data.ToQuery CreateService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceResponse' smart constructor.
data CreateServiceResponse = CreateServiceResponse'
  { -- | The Amazon Resource Name (ARN) of the service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The type of IAM policy.
    authType :: Prelude.Maybe AuthType,
    -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name of the service.
    dnsEntry :: Prelude.Maybe DnsEntry,
    -- | The ID of the service.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the service.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status. If the status is @CREATE_FAILED@, you will have to delete
    -- and recreate the service.
    status :: Prelude.Maybe ServiceStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createServiceResponse_arn' - The Amazon Resource Name (ARN) of the service.
--
-- 'authType', 'createServiceResponse_authType' - The type of IAM policy.
--
-- 'certificateArn', 'createServiceResponse_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'customDomainName', 'createServiceResponse_customDomainName' - The custom domain name of the service.
--
-- 'dnsEntry', 'createServiceResponse_dnsEntry' - The public DNS name of the service.
--
-- 'id', 'createServiceResponse_id' - The ID of the service.
--
-- 'name', 'createServiceResponse_name' - The name of the service.
--
-- 'status', 'createServiceResponse_status' - The status. If the status is @CREATE_FAILED@, you will have to delete
-- and recreate the service.
--
-- 'httpStatus', 'createServiceResponse_httpStatus' - The response's http status code.
newCreateServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceResponse
newCreateServiceResponse pHttpStatus_ =
  CreateServiceResponse'
    { arn = Prelude.Nothing,
      authType = Prelude.Nothing,
      certificateArn = Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      dnsEntry = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the service.
createServiceResponse_arn :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_arn = Lens.lens (\CreateServiceResponse' {arn} -> arn) (\s@CreateServiceResponse' {} a -> s {arn = a} :: CreateServiceResponse)

-- | The type of IAM policy.
createServiceResponse_authType :: Lens.Lens' CreateServiceResponse (Prelude.Maybe AuthType)
createServiceResponse_authType = Lens.lens (\CreateServiceResponse' {authType} -> authType) (\s@CreateServiceResponse' {} a -> s {authType = a} :: CreateServiceResponse)

-- | The Amazon Resource Name (ARN) of the certificate.
createServiceResponse_certificateArn :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_certificateArn = Lens.lens (\CreateServiceResponse' {certificateArn} -> certificateArn) (\s@CreateServiceResponse' {} a -> s {certificateArn = a} :: CreateServiceResponse)

-- | The custom domain name of the service.
createServiceResponse_customDomainName :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_customDomainName = Lens.lens (\CreateServiceResponse' {customDomainName} -> customDomainName) (\s@CreateServiceResponse' {} a -> s {customDomainName = a} :: CreateServiceResponse)

-- | The public DNS name of the service.
createServiceResponse_dnsEntry :: Lens.Lens' CreateServiceResponse (Prelude.Maybe DnsEntry)
createServiceResponse_dnsEntry = Lens.lens (\CreateServiceResponse' {dnsEntry} -> dnsEntry) (\s@CreateServiceResponse' {} a -> s {dnsEntry = a} :: CreateServiceResponse)

-- | The ID of the service.
createServiceResponse_id :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_id = Lens.lens (\CreateServiceResponse' {id} -> id) (\s@CreateServiceResponse' {} a -> s {id = a} :: CreateServiceResponse)

-- | The name of the service.
createServiceResponse_name :: Lens.Lens' CreateServiceResponse (Prelude.Maybe Prelude.Text)
createServiceResponse_name = Lens.lens (\CreateServiceResponse' {name} -> name) (\s@CreateServiceResponse' {} a -> s {name = a} :: CreateServiceResponse)

-- | The status. If the status is @CREATE_FAILED@, you will have to delete
-- and recreate the service.
createServiceResponse_status :: Lens.Lens' CreateServiceResponse (Prelude.Maybe ServiceStatus)
createServiceResponse_status = Lens.lens (\CreateServiceResponse' {status} -> status) (\s@CreateServiceResponse' {} a -> s {status = a} :: CreateServiceResponse)

-- | The response's http status code.
createServiceResponse_httpStatus :: Lens.Lens' CreateServiceResponse Prelude.Int
createServiceResponse_httpStatus = Lens.lens (\CreateServiceResponse' {httpStatus} -> httpStatus) (\s@CreateServiceResponse' {} a -> s {httpStatus = a} :: CreateServiceResponse)

instance Prelude.NFData CreateServiceResponse where
  rnf CreateServiceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authType
      `Prelude.seq` Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf dnsEntry
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
