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
-- Module      : Network.AWS.Route53AutoNaming.CreateHttpNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HTTP namespace. Service instances that you register using an
-- HTTP namespace can be discovered using a @DiscoverInstances@ request but
-- can\'t be discovered using DNS.
--
-- For the current quota on the number of namespaces that you can create
-- using the same AWS account, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map quotas>
-- in the /AWS Cloud Map Developer Guide/.
module Network.AWS.Route53AutoNaming.CreateHttpNamespace
  ( -- * Creating a Request
    CreateHttpNamespace (..),
    newCreateHttpNamespace,

    -- * Request Lenses
    createHttpNamespace_creatorRequestId,
    createHttpNamespace_tags,
    createHttpNamespace_description,
    createHttpNamespace_name,

    -- * Destructuring the Response
    CreateHttpNamespaceResponse (..),
    newCreateHttpNamespaceResponse,

    -- * Response Lenses
    createHttpNamespaceResponse_operationId,
    createHttpNamespaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newCreateHttpNamespace' smart constructor.
data CreateHttpNamespace = CreateHttpNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreateHttpNamespace@ requests to be retried without the risk of
    -- executing the operation twice. @CreatorRequestId@ can be any unique
    -- string, for example, a date\/time stamp.
    creatorRequestId :: Core.Maybe Core.Text,
    -- | The tags to add to the namespace. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Core.Maybe [Tag],
    -- | A description for the namespace.
    description :: Core.Maybe Core.Text,
    -- | The name that you want to assign to this namespace.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHttpNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorRequestId', 'createHttpNamespace_creatorRequestId' - A unique string that identifies the request and that allows failed
-- @CreateHttpNamespace@ requests to be retried without the risk of
-- executing the operation twice. @CreatorRequestId@ can be any unique
-- string, for example, a date\/time stamp.
--
-- 'tags', 'createHttpNamespace_tags' - The tags to add to the namespace. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'description', 'createHttpNamespace_description' - A description for the namespace.
--
-- 'name', 'createHttpNamespace_name' - The name that you want to assign to this namespace.
newCreateHttpNamespace ::
  -- | 'name'
  Core.Text ->
  CreateHttpNamespace
newCreateHttpNamespace pName_ =
  CreateHttpNamespace'
    { creatorRequestId =
        Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      name = pName_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreateHttpNamespace@ requests to be retried without the risk of
-- executing the operation twice. @CreatorRequestId@ can be any unique
-- string, for example, a date\/time stamp.
createHttpNamespace_creatorRequestId :: Lens.Lens' CreateHttpNamespace (Core.Maybe Core.Text)
createHttpNamespace_creatorRequestId = Lens.lens (\CreateHttpNamespace' {creatorRequestId} -> creatorRequestId) (\s@CreateHttpNamespace' {} a -> s {creatorRequestId = a} :: CreateHttpNamespace)

-- | The tags to add to the namespace. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createHttpNamespace_tags :: Lens.Lens' CreateHttpNamespace (Core.Maybe [Tag])
createHttpNamespace_tags = Lens.lens (\CreateHttpNamespace' {tags} -> tags) (\s@CreateHttpNamespace' {} a -> s {tags = a} :: CreateHttpNamespace) Core.. Lens.mapping Lens._Coerce

-- | A description for the namespace.
createHttpNamespace_description :: Lens.Lens' CreateHttpNamespace (Core.Maybe Core.Text)
createHttpNamespace_description = Lens.lens (\CreateHttpNamespace' {description} -> description) (\s@CreateHttpNamespace' {} a -> s {description = a} :: CreateHttpNamespace)

-- | The name that you want to assign to this namespace.
createHttpNamespace_name :: Lens.Lens' CreateHttpNamespace Core.Text
createHttpNamespace_name = Lens.lens (\CreateHttpNamespace' {name} -> name) (\s@CreateHttpNamespace' {} a -> s {name = a} :: CreateHttpNamespace)

instance Core.AWSRequest CreateHttpNamespace where
  type
    AWSResponse CreateHttpNamespace =
      CreateHttpNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateHttpNamespaceResponse'
            Core.<$> (x Core..?> "OperationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateHttpNamespace

instance Core.NFData CreateHttpNamespace

instance Core.ToHeaders CreateHttpNamespace where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.CreateHttpNamespace" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateHttpNamespace where
  toJSON CreateHttpNamespace' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatorRequestId" Core..=)
              Core.<$> creatorRequestId,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreateHttpNamespace where
  toPath = Core.const "/"

instance Core.ToQuery CreateHttpNamespace where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateHttpNamespaceResponse' smart constructor.
data CreateHttpNamespaceResponse = CreateHttpNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateHttpNamespaceResponse
newCreateHttpNamespaceResponse pHttpStatus_ =
  CreateHttpNamespaceResponse'
    { operationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
createHttpNamespaceResponse_operationId :: Lens.Lens' CreateHttpNamespaceResponse (Core.Maybe Core.Text)
createHttpNamespaceResponse_operationId = Lens.lens (\CreateHttpNamespaceResponse' {operationId} -> operationId) (\s@CreateHttpNamespaceResponse' {} a -> s {operationId = a} :: CreateHttpNamespaceResponse)

-- | The response's http status code.
createHttpNamespaceResponse_httpStatus :: Lens.Lens' CreateHttpNamespaceResponse Core.Int
createHttpNamespaceResponse_httpStatus = Lens.lens (\CreateHttpNamespaceResponse' {httpStatus} -> httpStatus) (\s@CreateHttpNamespaceResponse' {} a -> s {httpStatus = a} :: CreateHttpNamespaceResponse)

instance Core.NFData CreateHttpNamespaceResponse
