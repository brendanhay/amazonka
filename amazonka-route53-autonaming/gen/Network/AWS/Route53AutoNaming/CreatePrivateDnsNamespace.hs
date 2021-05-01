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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53AutoNaming.Types

-- | /See:/ 'newCreatePrivateDnsNamespace' smart constructor.
data CreatePrivateDnsNamespace = CreatePrivateDnsNamespace'
  { -- | A unique string that identifies the request and that allows failed
    -- @CreatePrivateDnsNamespace@ requests to be retried without the risk of
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
    -- | The name that you want to assign to this namespace. When you create a
    -- private DNS namespace, AWS Cloud Map automatically creates an Amazon
    -- Route 53 private hosted zone that has the same name as the namespace.
    name :: Prelude.Text,
    -- | The ID of the Amazon VPC that you want to associate the namespace with.
    vpc :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'vpc'
  Prelude.Text ->
  CreatePrivateDnsNamespace
newCreatePrivateDnsNamespace pName_ pVpc_ =
  CreatePrivateDnsNamespace'
    { creatorRequestId =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      name = pName_,
      vpc = pVpc_
    }

-- | A unique string that identifies the request and that allows failed
-- @CreatePrivateDnsNamespace@ requests to be retried without the risk of
-- executing the operation twice. @CreatorRequestId@ can be any unique
-- string, for example, a date\/time stamp.
createPrivateDnsNamespace_creatorRequestId :: Lens.Lens' CreatePrivateDnsNamespace (Prelude.Maybe Prelude.Text)
createPrivateDnsNamespace_creatorRequestId = Lens.lens (\CreatePrivateDnsNamespace' {creatorRequestId} -> creatorRequestId) (\s@CreatePrivateDnsNamespace' {} a -> s {creatorRequestId = a} :: CreatePrivateDnsNamespace)

-- | The tags to add to the namespace. Each tag consists of a key and an
-- optional value, both of which you define. Tag keys can have a maximum
-- character length of 128 characters, and tag values can have a maximum
-- length of 256 characters.
createPrivateDnsNamespace_tags :: Lens.Lens' CreatePrivateDnsNamespace (Prelude.Maybe [Tag])
createPrivateDnsNamespace_tags = Lens.lens (\CreatePrivateDnsNamespace' {tags} -> tags) (\s@CreatePrivateDnsNamespace' {} a -> s {tags = a} :: CreatePrivateDnsNamespace) Prelude.. Lens.mapping Prelude._Coerce

-- | A description for the namespace.
createPrivateDnsNamespace_description :: Lens.Lens' CreatePrivateDnsNamespace (Prelude.Maybe Prelude.Text)
createPrivateDnsNamespace_description = Lens.lens (\CreatePrivateDnsNamespace' {description} -> description) (\s@CreatePrivateDnsNamespace' {} a -> s {description = a} :: CreatePrivateDnsNamespace)

-- | The name that you want to assign to this namespace. When you create a
-- private DNS namespace, AWS Cloud Map automatically creates an Amazon
-- Route 53 private hosted zone that has the same name as the namespace.
createPrivateDnsNamespace_name :: Lens.Lens' CreatePrivateDnsNamespace Prelude.Text
createPrivateDnsNamespace_name = Lens.lens (\CreatePrivateDnsNamespace' {name} -> name) (\s@CreatePrivateDnsNamespace' {} a -> s {name = a} :: CreatePrivateDnsNamespace)

-- | The ID of the Amazon VPC that you want to associate the namespace with.
createPrivateDnsNamespace_vpc :: Lens.Lens' CreatePrivateDnsNamespace Prelude.Text
createPrivateDnsNamespace_vpc = Lens.lens (\CreatePrivateDnsNamespace' {vpc} -> vpc) (\s@CreatePrivateDnsNamespace' {} a -> s {vpc = a} :: CreatePrivateDnsNamespace)

instance Prelude.AWSRequest CreatePrivateDnsNamespace where
  type
    Rs CreatePrivateDnsNamespace =
      CreatePrivateDnsNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePrivateDnsNamespaceResponse'
            Prelude.<$> (x Prelude..?> "OperationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePrivateDnsNamespace

instance Prelude.NFData CreatePrivateDnsNamespace

instance Prelude.ToHeaders CreatePrivateDnsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Route53AutoNaming_v20170314.CreatePrivateDnsNamespace" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePrivateDnsNamespace where
  toJSON CreatePrivateDnsNamespace' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Prelude..=)
              Prelude.<$> creatorRequestId,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("Description" Prelude..=) Prelude.<$> description,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Vpc" Prelude..= vpc)
          ]
      )

instance Prelude.ToPath CreatePrivateDnsNamespace where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePrivateDnsNamespace where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
