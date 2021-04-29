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
-- Module      : Network.AWS.WorkSpaces.DeleteConnectionAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified connection alias. For more information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html Cross-Region Redirection for Amazon WorkSpaces>.
--
-- __If you will no longer be using a fully qualified domain name (FQDN) as
-- the registration code for your WorkSpaces users, you must take certain
-- precautions to prevent potential security issues.__ For more
-- information, see
-- <https://docs.aws.amazon.com/workspaces/latest/adminguide/cross-region-redirection.html#cross-region-redirection-security-considerations Security Considerations if You Stop Using Cross-Region Redirection>.
--
-- To delete a connection alias that has been shared, the shared account
-- must first disassociate the connection alias from any directories it has
-- been associated with. Then you must unshare the connection alias from
-- the account it has been shared with. You can delete a connection alias
-- only after it is no longer shared with any accounts or associated with
-- any directories.
module Network.AWS.WorkSpaces.DeleteConnectionAlias
  ( -- * Creating a Request
    DeleteConnectionAlias (..),
    newDeleteConnectionAlias,

    -- * Request Lenses
    deleteConnectionAlias_aliasId,

    -- * Destructuring the Response
    DeleteConnectionAliasResponse (..),
    newDeleteConnectionAliasResponse,

    -- * Response Lenses
    deleteConnectionAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'newDeleteConnectionAlias' smart constructor.
data DeleteConnectionAlias = DeleteConnectionAlias'
  { -- | The identifier of the connection alias to delete.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectionAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'deleteConnectionAlias_aliasId' - The identifier of the connection alias to delete.
newDeleteConnectionAlias ::
  -- | 'aliasId'
  Prelude.Text ->
  DeleteConnectionAlias
newDeleteConnectionAlias pAliasId_ =
  DeleteConnectionAlias' {aliasId = pAliasId_}

-- | The identifier of the connection alias to delete.
deleteConnectionAlias_aliasId :: Lens.Lens' DeleteConnectionAlias Prelude.Text
deleteConnectionAlias_aliasId = Lens.lens (\DeleteConnectionAlias' {aliasId} -> aliasId) (\s@DeleteConnectionAlias' {} a -> s {aliasId = a} :: DeleteConnectionAlias)

instance Prelude.AWSRequest DeleteConnectionAlias where
  type
    Rs DeleteConnectionAlias =
      DeleteConnectionAliasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConnectionAliasResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteConnectionAlias

instance Prelude.NFData DeleteConnectionAlias

instance Prelude.ToHeaders DeleteConnectionAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkspacesService.DeleteConnectionAlias" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteConnectionAlias where
  toJSON DeleteConnectionAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasId" Prelude..= aliasId)]
      )

instance Prelude.ToPath DeleteConnectionAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteConnectionAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteConnectionAliasResponse' smart constructor.
data DeleteConnectionAliasResponse = DeleteConnectionAliasResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteConnectionAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConnectionAliasResponse_httpStatus' - The response's http status code.
newDeleteConnectionAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConnectionAliasResponse
newDeleteConnectionAliasResponse pHttpStatus_ =
  DeleteConnectionAliasResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteConnectionAliasResponse_httpStatus :: Lens.Lens' DeleteConnectionAliasResponse Prelude.Int
deleteConnectionAliasResponse_httpStatus = Lens.lens (\DeleteConnectionAliasResponse' {httpStatus} -> httpStatus) (\s@DeleteConnectionAliasResponse' {} a -> s {httpStatus = a} :: DeleteConnectionAliasResponse)

instance Prelude.NFData DeleteConnectionAliasResponse
