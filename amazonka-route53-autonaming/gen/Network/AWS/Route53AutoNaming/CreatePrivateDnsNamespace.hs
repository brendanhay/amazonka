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
-- Module      : Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a private namespace based on DNS, which will be visible only
-- inside a specified Amazon VPC. The namespace defines your service naming
-- scheme. For example, if you name your namespace @example.com@ and name
-- your service @backend@, the resulting DNS name for the service will be
-- @backend.example.com@. For the current quota on the number of namespaces
-- that you can create using the same AWS account, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits>
-- in the /AWS Cloud Map Developer Guide/.
module Network.AWS.Route53AutoNaming.CreatePrivateDnsNamespace
  ( -- * Creating a Request
    CreatePrivateDnsNamespace (..),
    newCreatePrivateDnsNamespace,

    -- * Request Lenses
    createPrivateDnsNamespace_creatorRequestId,
    createPrivateDnsNamespace_tags,
    createPrivateDnsNamespace_description,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newCreatePrivateDnsNamespace' smart constructor.
data CreatePrivateDnsNamespace = CreatePrivateDnsNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreatePrivateDnsNamespace@ requests to be retried without the risk of
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
    -- | The name that you want to assign to this namespace. When you create a
    -- private DNS namespace, AWS Cloud Map automatically creates an Amazon
    -- Route 53 private hosted zone that has the same name as the namespace.
    name :: Core.Text,
    -- | The ID of the Amazon VPC that you want to associate the namespace with.
    vpc :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- executing the operation twice. @CreatorRequestId@ can be any unique
-- string, for example, a date\/time stamp.
--
-- 'tags', 'createPrivateDnsNamespace_tags' - The tags to add to the namespace. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'description', 'createPrivateDnsNamespace_description' - A description for the namespace.
--
-- 'name', 'createPrivateDnsNamespace_name' - The name that you want to assign to this namespace. When you create a
-- private DNS namespace, AWS Cloud Map automatically creates an Amazon
-- Route 53 private hosted zone that has the same name as the namespace.
--
-- 'vpc', 'createPrivateDnsNamespace_vpc' - The ID of the Amazon VPC that you want to associate the namespace with.
newCreatePrivateDnsNamespace ::
  -- | 'name'
  Core.Text ->
  -- | 'vpc'
  Core.Text ->
  CreatePrivateDnsNamespace
newCreatePrivateDnsNamespace pName_ pVpc_ =
  CreatePrivateDnsNamespace'
    { creatorRequestId =
        Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      name = pName_,
      vpc = pVpc_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreatePrivateDnsNamespace@ requests to be retried without the risk of
-- executing the operation twice. @CreatorRequestId@ can be any unique
-- string, for example, a date\/time stamp.
createPrivateDnsNamespace_creatorRequestId :: Lens.Lens' CreatePrivateDnsNamespace (Core.Maybe Core.Text)
createPrivateDnsNamespace_creatorRequestId = Lens.lens (\CreatePrivateDnsNamespace' {creatorRequestId} -> creatorRequestId) (\s@CreatePrivateDnsNamespace' {} a -> s {creatorRequestId = a} :: CreatePrivateDnsNamespace)

-- | The tags to add to the namespace. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createPrivateDnsNamespace_tags :: Lens.Lens' CreatePrivateDnsNamespace (Core.Maybe [Tag])
createPrivateDnsNamespace_tags = Lens.lens (\CreatePrivateDnsNamespace' {tags} -> tags) (\s@CreatePrivateDnsNamespace' {} a -> s {tags = a} :: CreatePrivateDnsNamespace) Core.. Lens.mapping Lens._Coerce

-- | A description for the namespace.
createPrivateDnsNamespace_description :: Lens.Lens' CreatePrivateDnsNamespace (Core.Maybe Core.Text)
createPrivateDnsNamespace_description = Lens.lens (\CreatePrivateDnsNamespace' {description} -> description) (\s@CreatePrivateDnsNamespace' {} a -> s {description = a} :: CreatePrivateDnsNamespace)

-- | The name that you want to assign to this namespace. When you create a
-- private DNS namespace, AWS Cloud Map automatically creates an Amazon
-- Route 53 private hosted zone that has the same name as the namespace.
createPrivateDnsNamespace_name :: Lens.Lens' CreatePrivateDnsNamespace Core.Text
createPrivateDnsNamespace_name = Lens.lens (\CreatePrivateDnsNamespace' {name} -> name) (\s@CreatePrivateDnsNamespace' {} a -> s {name = a} :: CreatePrivateDnsNamespace)

-- | The ID of the Amazon VPC that you want to associate the namespace with.
createPrivateDnsNamespace_vpc :: Lens.Lens' CreatePrivateDnsNamespace Core.Text
createPrivateDnsNamespace_vpc = Lens.lens (\CreatePrivateDnsNamespace' {vpc} -> vpc) (\s@CreatePrivateDnsNamespace' {} a -> s {vpc = a} :: CreatePrivateDnsNamespace)

instance Core.AWSRequest CreatePrivateDnsNamespace where
  type
    AWSResponse CreatePrivateDnsNamespace =
      CreatePrivateDnsNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePrivateDnsNamespaceResponse'
            Core.<$> (x Core..?> "OperationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePrivateDnsNamespace

instance Core.NFData CreatePrivateDnsNamespace

instance Core.ToHeaders CreatePrivateDnsNamespace where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53AutoNaming_v20170314.CreatePrivateDnsNamespace" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePrivateDnsNamespace where
  toJSON CreatePrivateDnsNamespace' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatorRequestId" Core..=)
              Core.<$> creatorRequestId,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("Vpc" Core..= vpc)
          ]
      )

instance Core.ToPath CreatePrivateDnsNamespace where
  toPath = Core.const "/"

instance Core.ToQuery CreatePrivateDnsNamespace where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePrivateDnsNamespaceResponse' smart constructor.
data CreatePrivateDnsNamespaceResponse = CreatePrivateDnsNamespaceResponse'
  { -- | A value that you can use to determine whether the request completed
    -- successfully. To get the status of the operation, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
    operationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreatePrivateDnsNamespaceResponse
newCreatePrivateDnsNamespaceResponse pHttpStatus_ =
  CreatePrivateDnsNamespaceResponse'
    { operationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A value that you can use to determine whether the request completed
-- successfully. To get the status of the operation, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_GetOperation.html GetOperation>.
createPrivateDnsNamespaceResponse_operationId :: Lens.Lens' CreatePrivateDnsNamespaceResponse (Core.Maybe Core.Text)
createPrivateDnsNamespaceResponse_operationId = Lens.lens (\CreatePrivateDnsNamespaceResponse' {operationId} -> operationId) (\s@CreatePrivateDnsNamespaceResponse' {} a -> s {operationId = a} :: CreatePrivateDnsNamespaceResponse)

-- | The response's http status code.
createPrivateDnsNamespaceResponse_httpStatus :: Lens.Lens' CreatePrivateDnsNamespaceResponse Core.Int
createPrivateDnsNamespaceResponse_httpStatus = Lens.lens (\CreatePrivateDnsNamespaceResponse' {httpStatus} -> httpStatus) (\s@CreatePrivateDnsNamespaceResponse' {} a -> s {httpStatus = a} :: CreatePrivateDnsNamespaceResponse)

instance
  Core.NFData
    CreatePrivateDnsNamespaceResponse
