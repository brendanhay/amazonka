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
-- Module      : Network.AWS.AMP.CreateRuleGroupsNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a rule group namespace.
module Network.AWS.AMP.CreateRuleGroupsNamespace
  ( -- * Creating a Request
    CreateRuleGroupsNamespace (..),
    newCreateRuleGroupsNamespace,

    -- * Request Lenses
    createRuleGroupsNamespace_clientToken,
    createRuleGroupsNamespace_tags,
    createRuleGroupsNamespace_data,
    createRuleGroupsNamespace_name,
    createRuleGroupsNamespace_workspaceId,

    -- * Destructuring the Response
    CreateRuleGroupsNamespaceResponse (..),
    newCreateRuleGroupsNamespaceResponse,

    -- * Response Lenses
    createRuleGroupsNamespaceResponse_tags,
    createRuleGroupsNamespaceResponse_httpStatus,
    createRuleGroupsNamespaceResponse_arn,
    createRuleGroupsNamespaceResponse_name,
    createRuleGroupsNamespaceResponse_status,
  )
where

import Network.AWS.AMP.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a CreateRuleGroupsNamespace operation.
--
-- /See:/ 'newCreateRuleGroupsNamespace' smart constructor.
data CreateRuleGroupsNamespace = CreateRuleGroupsNamespace'
  { -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Optional, user-provided tags for this rule groups namespace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The namespace data that define the rule groups.
    data' :: Core.Base64,
    -- | The rule groups namespace name.
    name :: Prelude.Text,
    -- | The ID of the workspace in which to create the rule group namespace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroupsNamespace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createRuleGroupsNamespace_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'tags', 'createRuleGroupsNamespace_tags' - Optional, user-provided tags for this rule groups namespace.
--
-- 'data'', 'createRuleGroupsNamespace_data' - The namespace data that define the rule groups.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'name', 'createRuleGroupsNamespace_name' - The rule groups namespace name.
--
-- 'workspaceId', 'createRuleGroupsNamespace_workspaceId' - The ID of the workspace in which to create the rule group namespace.
newCreateRuleGroupsNamespace ::
  -- | 'data''
  Prelude.ByteString ->
  -- | 'name'
  Prelude.Text ->
  -- | 'workspaceId'
  Prelude.Text ->
  CreateRuleGroupsNamespace
newCreateRuleGroupsNamespace
  pData_
  pName_
  pWorkspaceId_ =
    CreateRuleGroupsNamespace'
      { clientToken =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        data' = Core._Base64 Lens.# pData_,
        name = pName_,
        workspaceId = pWorkspaceId_
      }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
createRuleGroupsNamespace_clientToken :: Lens.Lens' CreateRuleGroupsNamespace (Prelude.Maybe Prelude.Text)
createRuleGroupsNamespace_clientToken = Lens.lens (\CreateRuleGroupsNamespace' {clientToken} -> clientToken) (\s@CreateRuleGroupsNamespace' {} a -> s {clientToken = a} :: CreateRuleGroupsNamespace)

-- | Optional, user-provided tags for this rule groups namespace.
createRuleGroupsNamespace_tags :: Lens.Lens' CreateRuleGroupsNamespace (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRuleGroupsNamespace_tags = Lens.lens (\CreateRuleGroupsNamespace' {tags} -> tags) (\s@CreateRuleGroupsNamespace' {} a -> s {tags = a} :: CreateRuleGroupsNamespace) Prelude.. Lens.mapping Lens.coerced

-- | The namespace data that define the rule groups.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createRuleGroupsNamespace_data :: Lens.Lens' CreateRuleGroupsNamespace Prelude.ByteString
createRuleGroupsNamespace_data = Lens.lens (\CreateRuleGroupsNamespace' {data'} -> data') (\s@CreateRuleGroupsNamespace' {} a -> s {data' = a} :: CreateRuleGroupsNamespace) Prelude.. Core._Base64

-- | The rule groups namespace name.
createRuleGroupsNamespace_name :: Lens.Lens' CreateRuleGroupsNamespace Prelude.Text
createRuleGroupsNamespace_name = Lens.lens (\CreateRuleGroupsNamespace' {name} -> name) (\s@CreateRuleGroupsNamespace' {} a -> s {name = a} :: CreateRuleGroupsNamespace)

-- | The ID of the workspace in which to create the rule group namespace.
createRuleGroupsNamespace_workspaceId :: Lens.Lens' CreateRuleGroupsNamespace Prelude.Text
createRuleGroupsNamespace_workspaceId = Lens.lens (\CreateRuleGroupsNamespace' {workspaceId} -> workspaceId) (\s@CreateRuleGroupsNamespace' {} a -> s {workspaceId = a} :: CreateRuleGroupsNamespace)

instance Core.AWSRequest CreateRuleGroupsNamespace where
  type
    AWSResponse CreateRuleGroupsNamespace =
      CreateRuleGroupsNamespaceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRuleGroupsNamespaceResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "status")
      )

instance Prelude.Hashable CreateRuleGroupsNamespace

instance Prelude.NFData CreateRuleGroupsNamespace

instance Core.ToHeaders CreateRuleGroupsNamespace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateRuleGroupsNamespace where
  toJSON CreateRuleGroupsNamespace' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("data" Core..= data'),
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateRuleGroupsNamespace where
  toPath CreateRuleGroupsNamespace' {..} =
    Prelude.mconcat
      [ "/workspaces/",
        Core.toBS workspaceId,
        "/rulegroupsnamespaces"
      ]

instance Core.ToQuery CreateRuleGroupsNamespace where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a CreateRuleGroupsNamespace operation.
--
-- /See:/ 'newCreateRuleGroupsNamespaceResponse' smart constructor.
data CreateRuleGroupsNamespaceResponse = CreateRuleGroupsNamespaceResponse'
  { -- | The tags of this rule groups namespace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of this rule groups namespace.
    arn :: Prelude.Text,
    -- | The rule groups namespace name.
    name :: Prelude.Text,
    -- | The status of rule groups namespace.
    status :: RuleGroupsNamespaceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleGroupsNamespaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createRuleGroupsNamespaceResponse_tags' - The tags of this rule groups namespace.
--
-- 'httpStatus', 'createRuleGroupsNamespaceResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createRuleGroupsNamespaceResponse_arn' - The Amazon Resource Name (ARN) of this rule groups namespace.
--
-- 'name', 'createRuleGroupsNamespaceResponse_name' - The rule groups namespace name.
--
-- 'status', 'createRuleGroupsNamespaceResponse_status' - The status of rule groups namespace.
newCreateRuleGroupsNamespaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  RuleGroupsNamespaceStatus ->
  CreateRuleGroupsNamespaceResponse
newCreateRuleGroupsNamespaceResponse
  pHttpStatus_
  pArn_
  pName_
  pStatus_ =
    CreateRuleGroupsNamespaceResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        name = pName_,
        status = pStatus_
      }

-- | The tags of this rule groups namespace.
createRuleGroupsNamespaceResponse_tags :: Lens.Lens' CreateRuleGroupsNamespaceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRuleGroupsNamespaceResponse_tags = Lens.lens (\CreateRuleGroupsNamespaceResponse' {tags} -> tags) (\s@CreateRuleGroupsNamespaceResponse' {} a -> s {tags = a} :: CreateRuleGroupsNamespaceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createRuleGroupsNamespaceResponse_httpStatus :: Lens.Lens' CreateRuleGroupsNamespaceResponse Prelude.Int
createRuleGroupsNamespaceResponse_httpStatus = Lens.lens (\CreateRuleGroupsNamespaceResponse' {httpStatus} -> httpStatus) (\s@CreateRuleGroupsNamespaceResponse' {} a -> s {httpStatus = a} :: CreateRuleGroupsNamespaceResponse)

-- | The Amazon Resource Name (ARN) of this rule groups namespace.
createRuleGroupsNamespaceResponse_arn :: Lens.Lens' CreateRuleGroupsNamespaceResponse Prelude.Text
createRuleGroupsNamespaceResponse_arn = Lens.lens (\CreateRuleGroupsNamespaceResponse' {arn} -> arn) (\s@CreateRuleGroupsNamespaceResponse' {} a -> s {arn = a} :: CreateRuleGroupsNamespaceResponse)

-- | The rule groups namespace name.
createRuleGroupsNamespaceResponse_name :: Lens.Lens' CreateRuleGroupsNamespaceResponse Prelude.Text
createRuleGroupsNamespaceResponse_name = Lens.lens (\CreateRuleGroupsNamespaceResponse' {name} -> name) (\s@CreateRuleGroupsNamespaceResponse' {} a -> s {name = a} :: CreateRuleGroupsNamespaceResponse)

-- | The status of rule groups namespace.
createRuleGroupsNamespaceResponse_status :: Lens.Lens' CreateRuleGroupsNamespaceResponse RuleGroupsNamespaceStatus
createRuleGroupsNamespaceResponse_status = Lens.lens (\CreateRuleGroupsNamespaceResponse' {status} -> status) (\s@CreateRuleGroupsNamespaceResponse' {} a -> s {status = a} :: CreateRuleGroupsNamespaceResponse)

instance
  Prelude.NFData
    CreateRuleGroupsNamespaceResponse
