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
-- Module      : Amazonka.WorkSpaces.DisassociateConnectionAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a connection alias from a directory. Disassociating a
-- connection alias disables cross-Region redirection between two
-- directories in different Regions. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- Before performing this operation, call
-- <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeConnectionAliases.html DescribeConnectionAliases>
-- to make sure that the current state of the connection alias is
-- @CREATED@.
module Amazonka.WorkSpaces.DisassociateConnectionAlias
  ( -- * Creating a Request
    DisassociateConnectionAlias (..),
    newDisassociateConnectionAlias,

    -- * Request Lenses
    disassociateConnectionAlias_aliasId,

    -- * Destructuring the Response
    DisassociateConnectionAliasResponse (..),
    newDisassociateConnectionAliasResponse,

    -- * Response Lenses
    disassociateConnectionAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDisassociateConnectionAlias' smart constructor.
data DisassociateConnectionAlias = DisassociateConnectionAlias'
  { -- | The identifier of the connection alias to disassociate.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConnectionAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'disassociateConnectionAlias_aliasId' - The identifier of the connection alias to disassociate.
newDisassociateConnectionAlias ::
  -- | 'aliasId'
  Prelude.Text ->
  DisassociateConnectionAlias
newDisassociateConnectionAlias pAliasId_ =
  DisassociateConnectionAlias' {aliasId = pAliasId_}

-- | The identifier of the connection alias to disassociate.
disassociateConnectionAlias_aliasId :: Lens.Lens' DisassociateConnectionAlias Prelude.Text
disassociateConnectionAlias_aliasId = Lens.lens (\DisassociateConnectionAlias' {aliasId} -> aliasId) (\s@DisassociateConnectionAlias' {} a -> s {aliasId = a} :: DisassociateConnectionAlias)

instance Core.AWSRequest DisassociateConnectionAlias where
  type
    AWSResponse DisassociateConnectionAlias =
      DisassociateConnectionAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateConnectionAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateConnectionAlias where
  hashWithSalt _salt DisassociateConnectionAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasId

instance Prelude.NFData DisassociateConnectionAlias where
  rnf DisassociateConnectionAlias' {..} =
    Prelude.rnf aliasId

instance Data.ToHeaders DisassociateConnectionAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DisassociateConnectionAlias" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateConnectionAlias where
  toJSON DisassociateConnectionAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasId" Data..= aliasId)]
      )

instance Data.ToPath DisassociateConnectionAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateConnectionAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateConnectionAliasResponse' smart constructor.
data DisassociateConnectionAliasResponse = DisassociateConnectionAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateConnectionAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateConnectionAliasResponse_httpStatus' - The response's http status code.
newDisassociateConnectionAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateConnectionAliasResponse
newDisassociateConnectionAliasResponse pHttpStatus_ =
  DisassociateConnectionAliasResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateConnectionAliasResponse_httpStatus :: Lens.Lens' DisassociateConnectionAliasResponse Prelude.Int
disassociateConnectionAliasResponse_httpStatus = Lens.lens (\DisassociateConnectionAliasResponse' {httpStatus} -> httpStatus) (\s@DisassociateConnectionAliasResponse' {} a -> s {httpStatus = a} :: DisassociateConnectionAliasResponse)

instance
  Prelude.NFData
    DisassociateConnectionAliasResponse
  where
  rnf DisassociateConnectionAliasResponse' {..} =
    Prelude.rnf httpStatus
