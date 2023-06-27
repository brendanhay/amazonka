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
-- Module      : Amazonka.AMP.CreateWorkspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AMP workspace.
module Amazonka.AMP.CreateWorkspace
  ( -- * Creating a Request
    CreateWorkspace (..),
    newCreateWorkspace,

    -- * Request Lenses
    createWorkspace_alias,
    createWorkspace_clientToken,
    createWorkspace_tags,

    -- * Destructuring the Response
    CreateWorkspaceResponse (..),
    newCreateWorkspaceResponse,

    -- * Response Lenses
    createWorkspaceResponse_tags,
    createWorkspaceResponse_httpStatus,
    createWorkspaceResponse_arn,
    createWorkspaceResponse_status,
    createWorkspaceResponse_workspaceId,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a CreateWorkspace operation.
--
-- /See:/ 'newCreateWorkspace' smart constructor.
data CreateWorkspace = CreateWorkspace'
  { -- | An optional user-assigned alias for this workspace. This alias is for
    -- user reference and does not need to be unique.
    alias :: Prelude.Maybe Prelude.Text,
    -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Optional, user-provided tags for this workspace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'createWorkspace_alias' - An optional user-assigned alias for this workspace. This alias is for
-- user reference and does not need to be unique.
--
-- 'clientToken', 'createWorkspace_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'tags', 'createWorkspace_tags' - Optional, user-provided tags for this workspace.
newCreateWorkspace ::
  CreateWorkspace
newCreateWorkspace =
  CreateWorkspace'
    { alias = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | An optional user-assigned alias for this workspace. This alias is for
-- user reference and does not need to be unique.
createWorkspace_alias :: Lens.Lens' CreateWorkspace (Prelude.Maybe Prelude.Text)
createWorkspace_alias = Lens.lens (\CreateWorkspace' {alias} -> alias) (\s@CreateWorkspace' {} a -> s {alias = a} :: CreateWorkspace)

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
createWorkspace_clientToken :: Lens.Lens' CreateWorkspace (Prelude.Maybe Prelude.Text)
createWorkspace_clientToken = Lens.lens (\CreateWorkspace' {clientToken} -> clientToken) (\s@CreateWorkspace' {} a -> s {clientToken = a} :: CreateWorkspace)

-- | Optional, user-provided tags for this workspace.
createWorkspace_tags :: Lens.Lens' CreateWorkspace (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkspace_tags = Lens.lens (\CreateWorkspace' {tags} -> tags) (\s@CreateWorkspace' {} a -> s {tags = a} :: CreateWorkspace) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest CreateWorkspace where
  type
    AWSResponse CreateWorkspace =
      CreateWorkspaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkspaceResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "status")
            Prelude.<*> (x Data..:> "workspaceId")
      )

instance Prelude.Hashable CreateWorkspace where
  hashWithSalt _salt CreateWorkspace' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` tags

instance Prelude.NFData CreateWorkspace where
  rnf CreateWorkspace' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf tags

instance Data.ToHeaders CreateWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkspace where
  toJSON CreateWorkspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alias" Data..=) Prelude.<$> alias,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )

instance Data.ToPath CreateWorkspace where
  toPath = Prelude.const "/workspaces"

instance Data.ToQuery CreateWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a CreateWorkspace operation.
--
-- /See:/ 'newCreateWorkspaceResponse' smart constructor.
data CreateWorkspaceResponse = CreateWorkspaceResponse'
  { -- | The tags of this workspace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the workspace that was just created.
    arn :: Prelude.Text,
    -- | The status of the workspace that was just created (usually CREATING).
    status :: WorkspaceStatus,
    -- | The generated ID of the workspace that was just created.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWorkspaceResponse_tags' - The tags of this workspace.
--
-- 'httpStatus', 'createWorkspaceResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createWorkspaceResponse_arn' - The ARN of the workspace that was just created.
--
-- 'status', 'createWorkspaceResponse_status' - The status of the workspace that was just created (usually CREATING).
--
-- 'workspaceId', 'createWorkspaceResponse_workspaceId' - The generated ID of the workspace that was just created.
newCreateWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'status'
  WorkspaceStatus ->
  -- | 'workspaceId'
  Prelude.Text ->
  CreateWorkspaceResponse
newCreateWorkspaceResponse
  pHttpStatus_
  pArn_
  pStatus_
  pWorkspaceId_ =
    CreateWorkspaceResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        status = pStatus_,
        workspaceId = pWorkspaceId_
      }

-- | The tags of this workspace.
createWorkspaceResponse_tags :: Lens.Lens' CreateWorkspaceResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkspaceResponse_tags = Lens.lens (\CreateWorkspaceResponse' {tags} -> tags) (\s@CreateWorkspaceResponse' {} a -> s {tags = a} :: CreateWorkspaceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createWorkspaceResponse_httpStatus :: Lens.Lens' CreateWorkspaceResponse Prelude.Int
createWorkspaceResponse_httpStatus = Lens.lens (\CreateWorkspaceResponse' {httpStatus} -> httpStatus) (\s@CreateWorkspaceResponse' {} a -> s {httpStatus = a} :: CreateWorkspaceResponse)

-- | The ARN of the workspace that was just created.
createWorkspaceResponse_arn :: Lens.Lens' CreateWorkspaceResponse Prelude.Text
createWorkspaceResponse_arn = Lens.lens (\CreateWorkspaceResponse' {arn} -> arn) (\s@CreateWorkspaceResponse' {} a -> s {arn = a} :: CreateWorkspaceResponse)

-- | The status of the workspace that was just created (usually CREATING).
createWorkspaceResponse_status :: Lens.Lens' CreateWorkspaceResponse WorkspaceStatus
createWorkspaceResponse_status = Lens.lens (\CreateWorkspaceResponse' {status} -> status) (\s@CreateWorkspaceResponse' {} a -> s {status = a} :: CreateWorkspaceResponse)

-- | The generated ID of the workspace that was just created.
createWorkspaceResponse_workspaceId :: Lens.Lens' CreateWorkspaceResponse Prelude.Text
createWorkspaceResponse_workspaceId = Lens.lens (\CreateWorkspaceResponse' {workspaceId} -> workspaceId) (\s@CreateWorkspaceResponse' {} a -> s {workspaceId = a} :: CreateWorkspaceResponse)

instance Prelude.NFData CreateWorkspaceResponse where
  rnf CreateWorkspaceResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf workspaceId
