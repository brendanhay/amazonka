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
-- Module      : Amazonka.VPCLattice.CreateServiceNetworkServiceAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a service with a service network.
--
-- You can\'t use this operation if the service and service network are
-- already associated or if there is a disassociation or deletion in
-- progress. If the association fails, you can retry the operation by
-- deleting the association and recreating it.
--
-- You cannot associate a service and service network that are shared with
-- a caller. The caller must own either the service or the service network.
--
-- As a result of this operation, the association is created in the service
-- network account and the association owner account.
module Amazonka.VPCLattice.CreateServiceNetworkServiceAssociation
  ( -- * Creating a Request
    CreateServiceNetworkServiceAssociation (..),
    newCreateServiceNetworkServiceAssociation,

    -- * Request Lenses
    createServiceNetworkServiceAssociation_clientToken,
    createServiceNetworkServiceAssociation_tags,
    createServiceNetworkServiceAssociation_serviceIdentifier,
    createServiceNetworkServiceAssociation_serviceNetworkIdentifier,

    -- * Destructuring the Response
    CreateServiceNetworkServiceAssociationResponse (..),
    newCreateServiceNetworkServiceAssociationResponse,

    -- * Response Lenses
    createServiceNetworkServiceAssociationResponse_arn,
    createServiceNetworkServiceAssociationResponse_createdBy,
    createServiceNetworkServiceAssociationResponse_customDomainName,
    createServiceNetworkServiceAssociationResponse_dnsEntry,
    createServiceNetworkServiceAssociationResponse_id,
    createServiceNetworkServiceAssociationResponse_status,
    createServiceNetworkServiceAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newCreateServiceNetworkServiceAssociation' smart constructor.
data CreateServiceNetworkServiceAssociation = CreateServiceNetworkServiceAssociation'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If you retry a request that completed
    -- successfully using the same client token and parameters, the retry
    -- succeeds without performing any actions. If the parameters aren\'t
    -- identical, the retry fails.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The tags for the association.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID or Amazon Resource Name (ARN) of the service.
    serviceIdentifier :: Prelude.Text,
    -- | The ID or Amazon Resource Name (ARN) of the service network. You must
    -- use the ARN if the resources specified in the operation are in different
    -- accounts.
    serviceNetworkIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceNetworkServiceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createServiceNetworkServiceAssociation_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
--
-- 'tags', 'createServiceNetworkServiceAssociation_tags' - The tags for the association.
--
-- 'serviceIdentifier', 'createServiceNetworkServiceAssociation_serviceIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
--
-- 'serviceNetworkIdentifier', 'createServiceNetworkServiceAssociation_serviceNetworkIdentifier' - The ID or Amazon Resource Name (ARN) of the service network. You must
-- use the ARN if the resources specified in the operation are in different
-- accounts.
newCreateServiceNetworkServiceAssociation ::
  -- | 'serviceIdentifier'
  Prelude.Text ->
  -- | 'serviceNetworkIdentifier'
  Prelude.Text ->
  CreateServiceNetworkServiceAssociation
newCreateServiceNetworkServiceAssociation
  pServiceIdentifier_
  pServiceNetworkIdentifier_ =
    CreateServiceNetworkServiceAssociation'
      { clientToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        serviceIdentifier =
          pServiceIdentifier_,
        serviceNetworkIdentifier =
          pServiceNetworkIdentifier_
      }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If you retry a request that completed
-- successfully using the same client token and parameters, the retry
-- succeeds without performing any actions. If the parameters aren\'t
-- identical, the retry fails.
createServiceNetworkServiceAssociation_clientToken :: Lens.Lens' CreateServiceNetworkServiceAssociation (Prelude.Maybe Prelude.Text)
createServiceNetworkServiceAssociation_clientToken = Lens.lens (\CreateServiceNetworkServiceAssociation' {clientToken} -> clientToken) (\s@CreateServiceNetworkServiceAssociation' {} a -> s {clientToken = a} :: CreateServiceNetworkServiceAssociation)

-- | The tags for the association.
createServiceNetworkServiceAssociation_tags :: Lens.Lens' CreateServiceNetworkServiceAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createServiceNetworkServiceAssociation_tags = Lens.lens (\CreateServiceNetworkServiceAssociation' {tags} -> tags) (\s@CreateServiceNetworkServiceAssociation' {} a -> s {tags = a} :: CreateServiceNetworkServiceAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID or Amazon Resource Name (ARN) of the service.
createServiceNetworkServiceAssociation_serviceIdentifier :: Lens.Lens' CreateServiceNetworkServiceAssociation Prelude.Text
createServiceNetworkServiceAssociation_serviceIdentifier = Lens.lens (\CreateServiceNetworkServiceAssociation' {serviceIdentifier} -> serviceIdentifier) (\s@CreateServiceNetworkServiceAssociation' {} a -> s {serviceIdentifier = a} :: CreateServiceNetworkServiceAssociation)

-- | The ID or Amazon Resource Name (ARN) of the service network. You must
-- use the ARN if the resources specified in the operation are in different
-- accounts.
createServiceNetworkServiceAssociation_serviceNetworkIdentifier :: Lens.Lens' CreateServiceNetworkServiceAssociation Prelude.Text
createServiceNetworkServiceAssociation_serviceNetworkIdentifier = Lens.lens (\CreateServiceNetworkServiceAssociation' {serviceNetworkIdentifier} -> serviceNetworkIdentifier) (\s@CreateServiceNetworkServiceAssociation' {} a -> s {serviceNetworkIdentifier = a} :: CreateServiceNetworkServiceAssociation)

instance
  Core.AWSRequest
    CreateServiceNetworkServiceAssociation
  where
  type
    AWSResponse
      CreateServiceNetworkServiceAssociation =
      CreateServiceNetworkServiceAssociationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateServiceNetworkServiceAssociationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "createdBy")
            Prelude.<*> (x Data..?> "customDomainName")
            Prelude.<*> (x Data..?> "dnsEntry")
            Prelude.<*> (x Data..?> "id")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateServiceNetworkServiceAssociation
  where
  hashWithSalt
    _salt
    CreateServiceNetworkServiceAssociation' {..} =
      _salt
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` serviceIdentifier
        `Prelude.hashWithSalt` serviceNetworkIdentifier

instance
  Prelude.NFData
    CreateServiceNetworkServiceAssociation
  where
  rnf CreateServiceNetworkServiceAssociation' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serviceIdentifier
      `Prelude.seq` Prelude.rnf serviceNetworkIdentifier

instance
  Data.ToHeaders
    CreateServiceNetworkServiceAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateServiceNetworkServiceAssociation
  where
  toJSON CreateServiceNetworkServiceAssociation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("serviceIdentifier" Data..= serviceIdentifier),
            Prelude.Just
              ( "serviceNetworkIdentifier"
                  Data..= serviceNetworkIdentifier
              )
          ]
      )

instance
  Data.ToPath
    CreateServiceNetworkServiceAssociation
  where
  toPath =
    Prelude.const "/servicenetworkserviceassociations"

instance
  Data.ToQuery
    CreateServiceNetworkServiceAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateServiceNetworkServiceAssociationResponse' smart constructor.
data CreateServiceNetworkServiceAssociationResponse = CreateServiceNetworkServiceAssociationResponse'
  { -- | The Amazon Resource Name (ARN) of the association.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The account that created the association.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name of the service.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the service.
    dnsEntry :: Prelude.Maybe DnsEntry,
    -- | The ID of the association.
    id :: Prelude.Maybe Prelude.Text,
    -- | The operation\'s status.
    status :: Prelude.Maybe ServiceNetworkServiceAssociationStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateServiceNetworkServiceAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createServiceNetworkServiceAssociationResponse_arn' - The Amazon Resource Name (ARN) of the association.
--
-- 'createdBy', 'createServiceNetworkServiceAssociationResponse_createdBy' - The account that created the association.
--
-- 'customDomainName', 'createServiceNetworkServiceAssociationResponse_customDomainName' - The custom domain name of the service.
--
-- 'dnsEntry', 'createServiceNetworkServiceAssociationResponse_dnsEntry' - The DNS name of the service.
--
-- 'id', 'createServiceNetworkServiceAssociationResponse_id' - The ID of the association.
--
-- 'status', 'createServiceNetworkServiceAssociationResponse_status' - The operation\'s status.
--
-- 'httpStatus', 'createServiceNetworkServiceAssociationResponse_httpStatus' - The response's http status code.
newCreateServiceNetworkServiceAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateServiceNetworkServiceAssociationResponse
newCreateServiceNetworkServiceAssociationResponse
  pHttpStatus_ =
    CreateServiceNetworkServiceAssociationResponse'
      { arn =
          Prelude.Nothing,
        createdBy = Prelude.Nothing,
        customDomainName =
          Prelude.Nothing,
        dnsEntry = Prelude.Nothing,
        id = Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the association.
createServiceNetworkServiceAssociationResponse_arn :: Lens.Lens' CreateServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkServiceAssociationResponse_arn = Lens.lens (\CreateServiceNetworkServiceAssociationResponse' {arn} -> arn) (\s@CreateServiceNetworkServiceAssociationResponse' {} a -> s {arn = a} :: CreateServiceNetworkServiceAssociationResponse)

-- | The account that created the association.
createServiceNetworkServiceAssociationResponse_createdBy :: Lens.Lens' CreateServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkServiceAssociationResponse_createdBy = Lens.lens (\CreateServiceNetworkServiceAssociationResponse' {createdBy} -> createdBy) (\s@CreateServiceNetworkServiceAssociationResponse' {} a -> s {createdBy = a} :: CreateServiceNetworkServiceAssociationResponse)

-- | The custom domain name of the service.
createServiceNetworkServiceAssociationResponse_customDomainName :: Lens.Lens' CreateServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkServiceAssociationResponse_customDomainName = Lens.lens (\CreateServiceNetworkServiceAssociationResponse' {customDomainName} -> customDomainName) (\s@CreateServiceNetworkServiceAssociationResponse' {} a -> s {customDomainName = a} :: CreateServiceNetworkServiceAssociationResponse)

-- | The DNS name of the service.
createServiceNetworkServiceAssociationResponse_dnsEntry :: Lens.Lens' CreateServiceNetworkServiceAssociationResponse (Prelude.Maybe DnsEntry)
createServiceNetworkServiceAssociationResponse_dnsEntry = Lens.lens (\CreateServiceNetworkServiceAssociationResponse' {dnsEntry} -> dnsEntry) (\s@CreateServiceNetworkServiceAssociationResponse' {} a -> s {dnsEntry = a} :: CreateServiceNetworkServiceAssociationResponse)

-- | The ID of the association.
createServiceNetworkServiceAssociationResponse_id :: Lens.Lens' CreateServiceNetworkServiceAssociationResponse (Prelude.Maybe Prelude.Text)
createServiceNetworkServiceAssociationResponse_id = Lens.lens (\CreateServiceNetworkServiceAssociationResponse' {id} -> id) (\s@CreateServiceNetworkServiceAssociationResponse' {} a -> s {id = a} :: CreateServiceNetworkServiceAssociationResponse)

-- | The operation\'s status.
createServiceNetworkServiceAssociationResponse_status :: Lens.Lens' CreateServiceNetworkServiceAssociationResponse (Prelude.Maybe ServiceNetworkServiceAssociationStatus)
createServiceNetworkServiceAssociationResponse_status = Lens.lens (\CreateServiceNetworkServiceAssociationResponse' {status} -> status) (\s@CreateServiceNetworkServiceAssociationResponse' {} a -> s {status = a} :: CreateServiceNetworkServiceAssociationResponse)

-- | The response's http status code.
createServiceNetworkServiceAssociationResponse_httpStatus :: Lens.Lens' CreateServiceNetworkServiceAssociationResponse Prelude.Int
createServiceNetworkServiceAssociationResponse_httpStatus = Lens.lens (\CreateServiceNetworkServiceAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateServiceNetworkServiceAssociationResponse' {} a -> s {httpStatus = a} :: CreateServiceNetworkServiceAssociationResponse)

instance
  Prelude.NFData
    CreateServiceNetworkServiceAssociationResponse
  where
  rnf
    CreateServiceNetworkServiceAssociationResponse' {..} =
      Prelude.rnf arn
        `Prelude.seq` Prelude.rnf createdBy
        `Prelude.seq` Prelude.rnf customDomainName
        `Prelude.seq` Prelude.rnf dnsEntry
        `Prelude.seq` Prelude.rnf id
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf httpStatus
