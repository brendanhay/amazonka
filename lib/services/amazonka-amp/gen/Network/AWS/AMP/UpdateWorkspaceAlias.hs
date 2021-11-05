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
-- Module      : Network.AWS.AMP.UpdateWorkspaceAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an AMP workspace alias.
module Network.AWS.AMP.UpdateWorkspaceAlias
  ( -- * Creating a Request
    UpdateWorkspaceAlias (..),
    newUpdateWorkspaceAlias,

    -- * Request Lenses
    updateWorkspaceAlias_clientToken,
    updateWorkspaceAlias_alias,
    updateWorkspaceAlias_workspaceId,

    -- * Destructuring the Response
    UpdateWorkspaceAliasResponse (..),
    newUpdateWorkspaceAliasResponse,
  )
where

import Network.AWS.AMP.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of an UpdateWorkspaceAlias operation.
--
-- /See:/ 'newUpdateWorkspaceAlias' smart constructor.
data UpdateWorkspaceAlias = UpdateWorkspaceAlias'
  { -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The new alias of the workspace.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workspace being updated.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateWorkspaceAlias_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'alias', 'updateWorkspaceAlias_alias' - The new alias of the workspace.
--
-- 'workspaceId', 'updateWorkspaceAlias_workspaceId' - The ID of the workspace being updated.
newUpdateWorkspaceAlias ::
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateWorkspaceAlias
newUpdateWorkspaceAlias pWorkspaceId_ =
  UpdateWorkspaceAlias'
    { clientToken =
        Prelude.Nothing,
      alias = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
updateWorkspaceAlias_clientToken :: Lens.Lens' UpdateWorkspaceAlias (Prelude.Maybe Prelude.Text)
updateWorkspaceAlias_clientToken = Lens.lens (\UpdateWorkspaceAlias' {clientToken} -> clientToken) (\s@UpdateWorkspaceAlias' {} a -> s {clientToken = a} :: UpdateWorkspaceAlias)

-- | The new alias of the workspace.
updateWorkspaceAlias_alias :: Lens.Lens' UpdateWorkspaceAlias (Prelude.Maybe Prelude.Text)
updateWorkspaceAlias_alias = Lens.lens (\UpdateWorkspaceAlias' {alias} -> alias) (\s@UpdateWorkspaceAlias' {} a -> s {alias = a} :: UpdateWorkspaceAlias)

-- | The ID of the workspace being updated.
updateWorkspaceAlias_workspaceId :: Lens.Lens' UpdateWorkspaceAlias Prelude.Text
updateWorkspaceAlias_workspaceId = Lens.lens (\UpdateWorkspaceAlias' {workspaceId} -> workspaceId) (\s@UpdateWorkspaceAlias' {} a -> s {workspaceId = a} :: UpdateWorkspaceAlias)

instance Core.AWSRequest UpdateWorkspaceAlias where
  type
    AWSResponse UpdateWorkspaceAlias =
      UpdateWorkspaceAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateWorkspaceAliasResponse'

instance Prelude.Hashable UpdateWorkspaceAlias

instance Prelude.NFData UpdateWorkspaceAlias

instance Core.ToHeaders UpdateWorkspaceAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateWorkspaceAlias where
  toJSON UpdateWorkspaceAlias' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("alias" Core..=) Prelude.<$> alias
          ]
      )

instance Core.ToPath UpdateWorkspaceAlias where
  toPath UpdateWorkspaceAlias' {..} =
    Prelude.mconcat
      ["/workspaces/", Core.toBS workspaceId, "/alias"]

instance Core.ToQuery UpdateWorkspaceAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkspaceAliasResponse' smart constructor.
data UpdateWorkspaceAliasResponse = UpdateWorkspaceAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkspaceAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateWorkspaceAliasResponse ::
  UpdateWorkspaceAliasResponse
newUpdateWorkspaceAliasResponse =
  UpdateWorkspaceAliasResponse'

instance Prelude.NFData UpdateWorkspaceAliasResponse
