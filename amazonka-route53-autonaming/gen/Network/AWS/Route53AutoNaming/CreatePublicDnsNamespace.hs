{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a public namespace based on DNS, which will be visible on the
-- internet. The namespace defines your service naming scheme. For example,
-- if you name your namespace @example.com@ and name your service
-- @backend@, the resulting DNS name for the service will be
-- @backend.example.com@. For the current quota on the number of namespaces
-- that you can create using the same AWS account, see
-- <https://docs.aws.amazon.com/cloud-map/latest/dg/cloud-map-limits.html AWS Cloud Map Limits>
-- in the /AWS Cloud Map Developer Guide/.
module Network.AWS.Route53AutoNaming.CreatePublicDnsNamespace
  ( -- * Creating a Request
    CreatePublicDnsNamespace (..),
    newCreatePublicDnsNamespace,

    -- * Request Lenses
    createPublicDnsNamespace_creatorRequestId,
    createPublicDnsNamespace_tags,
    createPublicDnsNamespace_description,
    createPublicDnsNamespace_name,

    -- * Destructuring the Response
    CreatePublicDnsNamespaceResponse (..),
    newCreatePublicDnsNamespaceResponse,

    -- * Response Lenses
    createPublicDnsNamespaceResponse_operationId,
    createPublicDnsNamespaceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newCreatePublicDnsNamespace' smart constructor.
data CreatePublicDnsNamespace = CreatePublicDnsNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreatePublicDnsNamespace@ requests to be retried without the risk of
    -- executing the operation twice. @CreatorRequestId@ can be any unique
    -- string, for example, a date\/time stamp.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The tags to add to the namespace. Each tag consists of a key and an
    -- optional value, both of which you define. Tag keys can have a maximum
    -- character length of 128 characters, and tag values can have a maximum
    -- length of 256 characters.
    tags :: Prelude.Maybe [Tag],
    -- | A description for the namespace.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name that you want to assign to this namespace.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- executing the operation twice. @CreatorRequestId@ can be any unique
-- string, for example, a date\/time stamp.
--
-- 'tags', 'createPublicDnsNamespace_tags' - The tags to add to the namespace. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
--
-- 'description', 'createPublicDnsNamespace_description' - A description for the namespace.
--
-- 'name', 'createPublicDnsNamespace_name' - The name that you want to assign to this namespace.
newCreatePublicDnsNamespace ::
  -- | 'name'
  Prelude.Text ->
  CreatePublicDnsNamespace
newCreatePublicDnsNamespace pName_ =
  CreatePublicDnsNamespace'
    { creatorRequestId =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreatePublicDnsNamespace@ requests to be retried without the risk of
-- executing the operation twice. @CreatorRequestId@ can be any unique
-- string, for example, a date\/time stamp.
createPublicDnsNamespace_creatorRequestId :: Lens.Lens' CreatePublicDnsNamespace (Prelude.Maybe Prelude.Text)
createPublicDnsNamespace_creatorRequestId = Lens.lens (\CreatePublicDnsNamespace' {creatorRequestId} -> creatorRequestId) (\s@CreatePublicDnsNamespace' {} a -> s {creatorRequestId = a} :: CreatePublicDnsNamespace)

-- | The tags to add to the namespace. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createPublicDnsNamespace_tags :: Lens.Lens' CreatePublicDnsNamespace (Prelude.Maybe [Tag])
createPublicDnsNamespace_tags = Lens.lens (\CreatePublicDnsNamespace' {tags} -> tags) (\s@CreatePublicDnsNamespace' {} a -> s {tags = a} :: CreatePublicDnsNamespace) Prelude.. Lens.mapping Prelude._Coerce

-- | A description for the namespace.
createPublicDnsNamespace_description :: Lens.Lens' CreatePublicDnsNamespace (Prelude.Maybe Prelude.Text)
createPublicDnsNamespace_description = Lens.lens (\CreatePublicDnsNamespace' {description} -> description) (\s@CreatePublicDnsNamespace' {} a -> s {description = a} :: CreatePublicDnsNamespace)

-- | The name that you want to assign to this namespace.
createPublicDnsNamespace_name :: Lens.Lens' CreatePublicDnsNamespace Prelude.Text
createPublicDnsNamespace_name = Lens.lens (\CreatePublicDnsNamespace' {name} -> name) (\s@CreatePublicDnsNamespace' {} a -> s {name = a} :: CreatePublicDnsNamespace)

instance Prelude.AWSRequest CreatePublicDnsNamespace where
  type
    Rs CreatePublicDnsNamespace =
      CreatePublicDnsNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePublicDnsNamespaceResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePublicDnsNamespace

instance Prelude.NFData CreatePublicDnsNamespace

instance Prelude.ToHeaders CreatePublicDnsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.CreatePublicDnsNamespace" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePublicDnsNamespace where
  toJSON CreatePublicDnsNamespace' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Prelude..=)
              Prelude.<$> creatorRequestId,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreatePublicDnsNamespace where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePublicDnsNamespace where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
