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
-- Module      : Amazonka.Route53AutoNaming.CreatePublicDnsNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public namespace based on DNS, which is visible on the
-- internet. The namespace defines your service naming scheme. For example,
-- if you name your namespace @example.com@ and name your service
-- @backend@, the resulting DNS name for the service is
-- @backend.example.com@. You can discover instances that were registered
-- with a public DNS namespace by using either a @DiscoverInstances@
-- request or using DNS. For the current quota on the number of namespaces
-- that you can create using the same Amazon Web Services account, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html Cloud Map quotas>
-- in the /Cloud Map Developer Guide/.
--
-- The @CreatePublicDnsNamespace@ API operation is not supported in the
-- Amazon Web Services GovCloud (US) Regions.
module Amazonka.Route53AutoNaming.CreatePublicDnsNamespace
  ( -- * Creating a Request
    CreatePublicDnsNamespace (..),
    newCreatePublicDnsNamespace,

    -- * Request Lenses
    createPublicDnsNamespace_creatorRequestId,
    createPublicDnsNamespace_description,
    createPublicDnsNamespace_properties,
    createPublicDnsNamespace_tags,
    createPublicDnsNamespace_name,

    -- * Destructuring the Response
    CreatePublicDnsNamespaceResponse (..),
    newCreatePublicDnsNamespaceResponse,

    -- * Response Lenses
    createPublicDnsNamespaceResponse_operationId,
    createPublicDnsNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newCreatePublicDnsNamespace' smart constructor.
data CreatePublicDnsNamespace = CreatePublicDnsNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreatePublicDnsNamespace@ requests to be retried without the risk of
    -- running the operation twice. @CreatorRequestId@ can be any unique string
    -- (for example, a date\/timestamp).
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | A description for the namespace.
    description :: Prelude.Maybe Prelude.Text,
    -- | Properties for the public DNS namespace.
    properties :: Prelude.Maybe PublicDnsNamespaceProperties,
    -- | The tags to add to the namespace. Each tag consists of a key and an
    -- optional value that you define. Tags keys can be up to 128 characters in
    -- length, and tag values can be up to 256 characters in length.
    tags :: Prelude.Maybe [Tag],
    -- | The name that you want to assign to this namespace.
    --
    -- Do not include sensitive information in the name. The name is publicly
    -- available using DNS queries.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublicDnsNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorRequestId', 'createPublicDnsNamespace_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @CreatePublicDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @CreatorRequestId@ can be any unique string
-- (for example, a date\/timestamp).
--
-- 'description', 'createPublicDnsNamespace_description' - A description for the namespace.
--
-- 'properties', 'createPublicDnsNamespace_properties' - Properties for the public DNS namespace.
--
-- 'tags', 'createPublicDnsNamespace_tags' - The tags to add to the namespace. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
--
-- 'name', 'createPublicDnsNamespace_name' - The name that you want to assign to this namespace.
--
-- Do not include sensitive information in the name. The name is publicly
-- available using DNS queries.
newCreatePublicDnsNamespace ::
  -- | 'name'
  Prelude.Text ->
  CreatePublicDnsNamespace
newCreatePublicDnsNamespace pName_ =
  CreatePublicDnsNamespace'
    { creatorRequestId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      properties = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreatePublicDnsNamespace@ requests to be retried without the risk of
-- running the operation twice. @CreatorRequestId@ can be any unique string
-- (for example, a date\/timestamp).
createPublicDnsNamespace_creatorRequestId :: Lens.Lens' CreatePublicDnsNamespace (Prelude.Maybe Prelude.Text)
createPublicDnsNamespace_creatorRequestId = Lens.lens (\CreatePublicDnsNamespace' {creatorRequestId} -> creatorRequestId) (\s@CreatePublicDnsNamespace' {} a -> s {creatorRequestId = a} :: CreatePublicDnsNamespace)

-- | A description for the namespace.
createPublicDnsNamespace_description :: Lens.Lens' CreatePublicDnsNamespace (Prelude.Maybe Prelude.Text)
createPublicDnsNamespace_description = Lens.lens (\CreatePublicDnsNamespace' {description} -> description) (\s@CreatePublicDnsNamespace' {} a -> s {description = a} :: CreatePublicDnsNamespace)

-- | Properties for the public DNS namespace.
createPublicDnsNamespace_properties :: Lens.Lens' CreatePublicDnsNamespace (Prelude.Maybe PublicDnsNamespaceProperties)
createPublicDnsNamespace_properties = Lens.lens (\CreatePublicDnsNamespace' {properties} -> properties) (\s@CreatePublicDnsNamespace' {} a -> s {properties = a} :: CreatePublicDnsNamespace)

-- | The tags to add to the namespace. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
createPublicDnsNamespace_tags :: Lens.Lens' CreatePublicDnsNamespace (Prelude.Maybe [Tag])
createPublicDnsNamespace_tags = Lens.lens (\CreatePublicDnsNamespace' {tags} -> tags) (\s@CreatePublicDnsNamespace' {} a -> s {tags = a} :: CreatePublicDnsNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The name that you want to assign to this namespace.
--
-- Do not include sensitive information in the name. The name is publicly
-- available using DNS queries.
createPublicDnsNamespace_name :: Lens.Lens' CreatePublicDnsNamespace Prelude.Text
createPublicDnsNamespace_name = Lens.lens (\CreatePublicDnsNamespace' {name} -> name) (\s@CreatePublicDnsNamespace' {} a -> s {name = a} :: CreatePublicDnsNamespace)

instance Core.AWSRequest CreatePublicDnsNamespace where
  type
    AWSResponse CreatePublicDnsNamespace =
      CreatePublicDnsNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePublicDnsNamespaceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePublicDnsNamespace where
  hashWithSalt _salt CreatePublicDnsNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreatePublicDnsNamespace where
  rnf CreatePublicDnsNamespace' {..} =
    Prelude.rnf creatorRequestId `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf properties `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf name

instance Data.ToHeaders CreatePublicDnsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.CreatePublicDnsNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePublicDnsNamespace where
  toJSON CreatePublicDnsNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Data..=)
              Prelude.<$> creatorRequestId,
            ("Description" Data..=) Prelude.<$> description,
            ("Properties" Data..=) Prelude.<$> properties,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreatePublicDnsNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery CreatePublicDnsNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePublicDnsNamespaceResponse' smart constructor.
data CreatePublicDnsNamespaceResponse = CreatePublicDnsNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePublicDnsNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'createPublicDnsNamespaceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'createPublicDnsNamespaceResponse_httpStatus' - The response's http status code.
newCreatePublicDnsNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePublicDnsNamespaceResponse
newCreatePublicDnsNamespaceResponse pHttpStatus_ =
  CreatePublicDnsNamespaceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
createPublicDnsNamespaceResponse_operationId :: Lens.Lens' CreatePublicDnsNamespaceResponse (Prelude.Maybe Prelude.Text)
createPublicDnsNamespaceResponse_operationId = Lens.lens (\CreatePublicDnsNamespaceResponse' {operationId} -> operationId) (\s@CreatePublicDnsNamespaceResponse' {} a -> s {operationId = a} :: CreatePublicDnsNamespaceResponse)

-- | The response's http status code.
createPublicDnsNamespaceResponse_httpStatus :: Lens.Lens' CreatePublicDnsNamespaceResponse Prelude.Int
createPublicDnsNamespaceResponse_httpStatus = Lens.lens (\CreatePublicDnsNamespaceResponse' {httpStatus} -> httpStatus) (\s@CreatePublicDnsNamespaceResponse' {} a -> s {httpStatus = a} :: CreatePublicDnsNamespaceResponse)

instance
  Prelude.NFData
    CreatePublicDnsNamespaceResponse
  where
  rnf CreatePublicDnsNamespaceResponse' {..} =
    Prelude.rnf operationId `Prelude.seq`
      Prelude.rnf httpStatus
