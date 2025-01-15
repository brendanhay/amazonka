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
-- Module      : Amazonka.Route53AutoNaming.CreatePrivateDnsNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private namespace based on DNS, which is visible only inside a
-- specified Amazon VPC. The namespace defines your service naming scheme.
-- For example, if you name your namespace @example.com@ and name your
-- service @backend@, the resulting DNS name for the service is
-- @backend.example.com@. Service instances that are registered using a
-- private DNS namespace can be discovered using either a
-- @DiscoverInstances@ request or using DNS. For the current quota on the
-- number of namespaces that you can create using the same Amazon Web
-- Services account, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html Cloud Map quotas>
-- in the /Cloud Map Developer Guide/.
module Amazonka.Route53AutoNaming.CreatePrivateDnsNamespace
  ( -- * Creating a Request
    CreatePrivateDnsNamespace (..),
    newCreatePrivateDnsNamespace,

    -- * Request Lenses
    createPrivateDnsNamespace_creatorRequestId,
    createPrivateDnsNamespace_description,
    createPrivateDnsNamespace_properties,
    createPrivateDnsNamespace_tags,
    createPrivateDnsNamespace_name,
    createPrivateDnsNamespace_vpc,

    -- * Destructuring the Response
    CreatePrivateDnsNamespaceResponse (..),
    newCreatePrivateDnsNamespaceResponse,

    -- * Response Lenses
    createPrivateDnsNamespaceResponse_operationId,
    createPrivateDnsNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newCreatePrivateDnsNamespace' smart constructor.
data CreatePrivateDnsNamespace = CreatePrivateDnsNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreatePrivateDnsNamespace@ requests to be retried without the risk of
    -- running the operation twice. @CreatorRequestId@ can be any unique string
    -- (for example, a date\/timestamp).
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | A description for the namespace.
    description :: Prelude.Maybe Prelude.Text,
    -- | Properties for the private DNS namespace.
    properties :: Prelude.Maybe PrivateDnsNamespaceProperties,
    -- | The tags to add to the namespace. Each tag consists of a key and an
    -- optional value that you define. Tags keys can be up to 128 characters in
    -- length, and tag values can be up to 256 characters in length.
    tags :: Prelude.Maybe [Tag],
    -- | The name that you want to assign to this namespace. When you create a
    -- private DNS namespace, Cloud Map automatically creates an Amazon
    -- Route 53 private hosted zone that has the same name as the namespace.
    name :: Prelude.Text,
    -- | The ID of the Amazon VPC that you want to associate the namespace with.
    vpc :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePrivateDnsNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorRequestId', 'createPrivateDnsNamespace_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @CreatePrivateDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @CreatorRequestId@ can be any unique string
-- (for example, a date\/timestamp).
--
-- 'description', 'createPrivateDnsNamespace_description' - A description for the namespace.
--
-- 'properties', 'createPrivateDnsNamespace_properties' - Properties for the private DNS namespace.
--
-- 'tags', 'createPrivateDnsNamespace_tags' - The tags to add to the namespace. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
--
-- 'name', 'createPrivateDnsNamespace_name' - The name that you want to assign to this namespace. When you create a
-- private DNS namespace, Cloud Map automatically creates an Amazon
-- Route 53 private hosted zone that has the same name as the namespace.
--
-- 'vpc', 'createPrivateDnsNamespace_vpc' - The ID of the Amazon VPC that you want to associate the namespace with.
newCreatePrivateDnsNamespace ::
  -- | 'name'
  Prelude.Text ->
  -- | 'vpc'
  Prelude.Text ->
  CreatePrivateDnsNamespace
newCreatePrivateDnsNamespace pName_ pVpc_ =
  CreatePrivateDnsNamespace'
    { creatorRequestId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      properties = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      vpc = pVpc_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreatePrivateDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @CreatorRequestId@ can be any unique string
-- (for example, a date\/timestamp).
createPrivateDnsNamespace_creatorRequestId :: Lens.Lens' CreatePrivateDnsNamespace (Prelude.Maybe Prelude.Text)
createPrivateDnsNamespace_creatorRequestId = Lens.lens (\CreatePrivateDnsNamespace' {creatorRequestId} -> creatorRequestId) (\s@CreatePrivateDnsNamespace' {} a -> s {creatorRequestId = a} :: CreatePrivateDnsNamespace)

-- | A description for the namespace.
createPrivateDnsNamespace_description :: Lens.Lens' CreatePrivateDnsNamespace (Prelude.Maybe Prelude.Text)
createPrivateDnsNamespace_description = Lens.lens (\CreatePrivateDnsNamespace' {description} -> description) (\s@CreatePrivateDnsNamespace' {} a -> s {description = a} :: CreatePrivateDnsNamespace)

-- | Properties for the private DNS namespace.
createPrivateDnsNamespace_properties :: Lens.Lens' CreatePrivateDnsNamespace (Prelude.Maybe PrivateDnsNamespaceProperties)
createPrivateDnsNamespace_properties = Lens.lens (\CreatePrivateDnsNamespace' {properties} -> properties) (\s@CreatePrivateDnsNamespace' {} a -> s {properties = a} :: CreatePrivateDnsNamespace)

-- | The tags to add to the namespace. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
createPrivateDnsNamespace_tags :: Lens.Lens' CreatePrivateDnsNamespace (Prelude.Maybe [Tag])
createPrivateDnsNamespace_tags = Lens.lens (\CreatePrivateDnsNamespace' {tags} -> tags) (\s@CreatePrivateDnsNamespace' {} a -> s {tags = a} :: CreatePrivateDnsNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The name that you want to assign to this namespace. When you create a
-- private DNS namespace, Cloud Map automatically creates an Amazon
-- Route 53 private hosted zone that has the same name as the namespace.
createPrivateDnsNamespace_name :: Lens.Lens' CreatePrivateDnsNamespace Prelude.Text
createPrivateDnsNamespace_name = Lens.lens (\CreatePrivateDnsNamespace' {name} -> name) (\s@CreatePrivateDnsNamespace' {} a -> s {name = a} :: CreatePrivateDnsNamespace)

-- | The ID of the Amazon VPC that you want to associate the namespace with.
createPrivateDnsNamespace_vpc :: Lens.Lens' CreatePrivateDnsNamespace Prelude.Text
createPrivateDnsNamespace_vpc = Lens.lens (\CreatePrivateDnsNamespace' {vpc} -> vpc) (\s@CreatePrivateDnsNamespace' {} a -> s {vpc = a} :: CreatePrivateDnsNamespace)

instance Core.AWSRequest CreatePrivateDnsNamespace where
  type
    AWSResponse CreatePrivateDnsNamespace =
      CreatePrivateDnsNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePrivateDnsNamespaceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePrivateDnsNamespace where
  hashWithSalt _salt CreatePrivateDnsNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` vpc

instance Prelude.NFData CreatePrivateDnsNamespace where
  rnf CreatePrivateDnsNamespace' {..} =
    Prelude.rnf creatorRequestId `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf properties `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf vpc

instance Data.ToHeaders CreatePrivateDnsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.CreatePrivateDnsNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePrivateDnsNamespace where
  toJSON CreatePrivateDnsNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Data..=)
              Prelude.<$> creatorRequestId,
            ("Description" Data..=) Prelude.<$> description,
            ("Properties" Data..=) Prelude.<$> properties,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Vpc" Data..= vpc)
          ]
      )

instance Data.ToPath CreatePrivateDnsNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePrivateDnsNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePrivateDnsNamespaceResponse' smart constructor.
data CreatePrivateDnsNamespaceResponse = CreatePrivateDnsNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePrivateDnsNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'createPrivateDnsNamespaceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'createPrivateDnsNamespaceResponse_httpStatus' - The response's http status code.
newCreatePrivateDnsNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePrivateDnsNamespaceResponse
newCreatePrivateDnsNamespaceResponse pHttpStatus_ =
  CreatePrivateDnsNamespaceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
createPrivateDnsNamespaceResponse_operationId :: Lens.Lens' CreatePrivateDnsNamespaceResponse (Prelude.Maybe Prelude.Text)
createPrivateDnsNamespaceResponse_operationId = Lens.lens (\CreatePrivateDnsNamespaceResponse' {operationId} -> operationId) (\s@CreatePrivateDnsNamespaceResponse' {} a -> s {operationId = a} :: CreatePrivateDnsNamespaceResponse)

-- | The response's http status code.
createPrivateDnsNamespaceResponse_httpStatus :: Lens.Lens' CreatePrivateDnsNamespaceResponse Prelude.Int
createPrivateDnsNamespaceResponse_httpStatus = Lens.lens (\CreatePrivateDnsNamespaceResponse' {httpStatus} -> httpStatus) (\s@CreatePrivateDnsNamespaceResponse' {} a -> s {httpStatus = a} :: CreatePrivateDnsNamespaceResponse)

instance
  Prelude.NFData
    CreatePrivateDnsNamespaceResponse
  where
  rnf CreatePrivateDnsNamespaceResponse' {..} =
    Prelude.rnf operationId `Prelude.seq`
      Prelude.rnf httpStatus
