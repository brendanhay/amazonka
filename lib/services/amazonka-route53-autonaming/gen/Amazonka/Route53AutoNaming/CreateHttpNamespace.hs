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
-- Module      : Amazonka.Route53AutoNaming.CreateHttpNamespace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HTTP namespace. Service instances registered using an HTTP
-- namespace can be discovered using a @DiscoverInstances@ request but
-- can\'t be discovered using DNS.
--
-- For the current quota on the number of namespaces that you can create
-- using the same Amazon Web Services account, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html Cloud Map quotas>
-- in the /Cloud Map Developer Guide/.
module Amazonka.Route53AutoNaming.CreateHttpNamespace
  ( -- * Creating a Request
    CreateHttpNamespace (..),
    newCreateHttpNamespace,

    -- * Request Lenses
    createHttpNamespace_creatorRequestId,
    createHttpNamespace_description,
    createHttpNamespace_tags,
    createHttpNamespace_name,

    -- * Destructuring the Response
    CreateHttpNamespaceResponse (..),
    newCreateHttpNamespaceResponse,

    -- * Response Lenses
    createHttpNamespaceResponse_operationId,
    createHttpNamespaceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53AutoNaming.Types

-- | /See:/ 'newCreateHttpNamespace' smart constructor.
data CreateHttpNamespace = CreateHttpNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreateHttpNamespace@ requests to be retried without the risk of running
    -- the operation twice. @CreatorRequestId@ can be any unique string (for
    -- example, a date\/time stamp).
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | A description for the namespace.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to add to the namespace. Each tag consists of a key and an
    -- optional value that you define. Tags keys can be up to 128 characters in
    -- length, and tag values can be up to 256 characters in length.
    tags :: Prelude.Maybe [Tag],
    -- | The name that you want to assign to this namespace.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHttpNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorRequestId', 'createHttpNamespace_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @CreateHttpNamespace@ requests to be retried without the risk of running
-- the operation twice. @CreatorRequestId@ can be any unique string (for
-- example, a date\/time stamp).
--
-- 'description', 'createHttpNamespace_description' - A description for the namespace.
--
-- 'tags', 'createHttpNamespace_tags' - The tags to add to the namespace. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
--
-- 'name', 'createHttpNamespace_name' - The name that you want to assign to this namespace.
newCreateHttpNamespace ::
  -- | 'name'
  Prelude.Text ->
  CreateHttpNamespace
newCreateHttpNamespace pName_ =
  CreateHttpNamespace'
    { creatorRequestId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreateHttpNamespace@ requests to be retried without the risk of running
-- the operation twice. @CreatorRequestId@ can be any unique string (for
-- example, a date\/time stamp).
createHttpNamespace_creatorRequestId :: Lens.Lens' CreateHttpNamespace (Prelude.Maybe Prelude.Text)
createHttpNamespace_creatorRequestId = Lens.lens (\CreateHttpNamespace' {creatorRequestId} -> creatorRequestId) (\s@CreateHttpNamespace' {} a -> s {creatorRequestId = a} :: CreateHttpNamespace)

-- | A description for the namespace.
createHttpNamespace_description :: Lens.Lens' CreateHttpNamespace (Prelude.Maybe Prelude.Text)
createHttpNamespace_description = Lens.lens (\CreateHttpNamespace' {description} -> description) (\s@CreateHttpNamespace' {} a -> s {description = a} :: CreateHttpNamespace)

-- | The tags to add to the namespace. Each tag consists of a key and an
-- optional value that you define. Tags keys can be up to 128 characters in
-- length, and tag values can be up to 256 characters in length.
createHttpNamespace_tags :: Lens.Lens' CreateHttpNamespace (Prelude.Maybe [Tag])
createHttpNamespace_tags = Lens.lens (\CreateHttpNamespace' {tags} -> tags) (\s@CreateHttpNamespace' {} a -> s {tags = a} :: CreateHttpNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The name that you want to assign to this namespace.
createHttpNamespace_name :: Lens.Lens' CreateHttpNamespace Prelude.Text
createHttpNamespace_name = Lens.lens (\CreateHttpNamespace' {name} -> name) (\s@CreateHttpNamespace' {} a -> s {name = a} :: CreateHttpNamespace)

instance Core.AWSRequest CreateHttpNamespace where
  type
    AWSResponse CreateHttpNamespace =
      CreateHttpNamespaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHttpNamespaceResponse'
            Prelude.<$> (x Data..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHttpNamespace where
  hashWithSalt _salt CreateHttpNamespace' {..} =
    _salt
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateHttpNamespace where
  rnf CreateHttpNamespace' {..} =
    Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateHttpNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53AutoNaming_v20170314.CreateHttpNamespace" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateHttpNamespace where
  toJSON CreateHttpNamespace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Data..=)
              Prelude.<$> creatorRequestId,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateHttpNamespace where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHttpNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateHttpNamespaceResponse' smart constructor.
data CreateHttpNamespaceResponse = CreateHttpNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateHttpNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operationId', 'createHttpNamespaceResponse_operationId' - A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
--
-- 'httpStatus', 'createHttpNamespaceResponse_httpStatus' - The response's http status code.
newCreateHttpNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateHttpNamespaceResponse
newCreateHttpNamespaceResponse pHttpStatus_ =
  CreateHttpNamespaceResponse'
    { operationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
createHttpNamespaceResponse_operationId :: Lens.Lens' CreateHttpNamespaceResponse (Prelude.Maybe Prelude.Text)
createHttpNamespaceResponse_operationId = Lens.lens (\CreateHttpNamespaceResponse' {operationId} -> operationId) (\s@CreateHttpNamespaceResponse' {} a -> s {operationId = a} :: CreateHttpNamespaceResponse)

-- | The response's http status code.
createHttpNamespaceResponse_httpStatus :: Lens.Lens' CreateHttpNamespaceResponse Prelude.Int
createHttpNamespaceResponse_httpStatus = Lens.lens (\CreateHttpNamespaceResponse' {httpStatus} -> httpStatus) (\s@CreateHttpNamespaceResponse' {} a -> s {httpStatus = a} :: CreateHttpNamespaceResponse)

instance Prelude.NFData CreateHttpNamespaceResponse where
  rnf CreateHttpNamespaceResponse' {..} =
    Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf httpStatus
