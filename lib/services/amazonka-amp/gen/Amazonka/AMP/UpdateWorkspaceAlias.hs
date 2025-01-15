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
-- Module      : Amazonka.AMP.UpdateWorkspaceAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an AMP workspace alias.
module Amazonka.AMP.UpdateWorkspaceAlias
  ( -- * Creating a Request
    UpdateWorkspaceAlias (..),
    newUpdateWorkspaceAlias,

    -- * Request Lenses
    updateWorkspaceAlias_alias,
    updateWorkspaceAlias_clientToken,
    updateWorkspaceAlias_workspaceId,

    -- * Destructuring the Response
    UpdateWorkspaceAliasResponse (..),
    newUpdateWorkspaceAliasResponse,
  )
where

import Amazonka.AMP.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of an UpdateWorkspaceAlias operation.
--
-- /See:/ 'newUpdateWorkspaceAlias' smart constructor.
data UpdateWorkspaceAlias = UpdateWorkspaceAlias'
  { -- | The new alias of the workspace.
    alias :: Prelude.Maybe Prelude.Text,
    -- | Optional, unique, case-sensitive, user-provided identifier to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
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
-- 'alias', 'updateWorkspaceAlias_alias' - The new alias of the workspace.
--
-- 'clientToken', 'updateWorkspaceAlias_clientToken' - Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
--
-- 'workspaceId', 'updateWorkspaceAlias_workspaceId' - The ID of the workspace being updated.
newUpdateWorkspaceAlias ::
  -- | 'workspaceId'
  Prelude.Text ->
  UpdateWorkspaceAlias
newUpdateWorkspaceAlias pWorkspaceId_ =
  UpdateWorkspaceAlias'
    { alias = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      workspaceId = pWorkspaceId_
    }

-- | The new alias of the workspace.
updateWorkspaceAlias_alias :: Lens.Lens' UpdateWorkspaceAlias (Prelude.Maybe Prelude.Text)
updateWorkspaceAlias_alias = Lens.lens (\UpdateWorkspaceAlias' {alias} -> alias) (\s@UpdateWorkspaceAlias' {} a -> s {alias = a} :: UpdateWorkspaceAlias)

-- | Optional, unique, case-sensitive, user-provided identifier to ensure the
-- idempotency of the request.
updateWorkspaceAlias_clientToken :: Lens.Lens' UpdateWorkspaceAlias (Prelude.Maybe Prelude.Text)
updateWorkspaceAlias_clientToken = Lens.lens (\UpdateWorkspaceAlias' {clientToken} -> clientToken) (\s@UpdateWorkspaceAlias' {} a -> s {clientToken = a} :: UpdateWorkspaceAlias)

-- | The ID of the workspace being updated.
updateWorkspaceAlias_workspaceId :: Lens.Lens' UpdateWorkspaceAlias Prelude.Text
updateWorkspaceAlias_workspaceId = Lens.lens (\UpdateWorkspaceAlias' {workspaceId} -> workspaceId) (\s@UpdateWorkspaceAlias' {} a -> s {workspaceId = a} :: UpdateWorkspaceAlias)

instance Core.AWSRequest UpdateWorkspaceAlias where
  type
    AWSResponse UpdateWorkspaceAlias =
      UpdateWorkspaceAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateWorkspaceAliasResponse'

instance Prelude.Hashable UpdateWorkspaceAlias where
  hashWithSalt _salt UpdateWorkspaceAlias' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData UpdateWorkspaceAlias where
  rnf UpdateWorkspaceAlias' {..} =
    Prelude.rnf alias `Prelude.seq`
      Prelude.rnf clientToken `Prelude.seq`
        Prelude.rnf workspaceId

instance Data.ToHeaders UpdateWorkspaceAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkspaceAlias where
  toJSON UpdateWorkspaceAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("alias" Data..=) Prelude.<$> alias,
            ("clientToken" Data..=) Prelude.<$> clientToken
          ]
      )

instance Data.ToPath UpdateWorkspaceAlias where
  toPath UpdateWorkspaceAlias' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId, "/alias"]

instance Data.ToQuery UpdateWorkspaceAlias where
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

instance Prelude.NFData UpdateWorkspaceAliasResponse where
  rnf _ = ()
