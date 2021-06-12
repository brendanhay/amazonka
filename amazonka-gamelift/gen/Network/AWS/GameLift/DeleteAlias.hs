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
-- Module      : Network.AWS.GameLift.DeleteAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias. This operation removes all record of the alias. Game
-- clients attempting to access a server process using the deleted alias
-- receive an error. To delete an alias, specify the alias ID to be
-- deleted.
--
-- -   CreateAlias
--
-- -   ListAliases
--
-- -   DescribeAlias
--
-- -   UpdateAlias
--
-- -   DeleteAlias
--
-- -   ResolveAlias
module Network.AWS.GameLift.DeleteAlias
  ( -- * Creating a Request
    DeleteAlias (..),
    newDeleteAlias,

    -- * Request Lenses
    deleteAlias_aliasId,

    -- * Destructuring the Response
    DeleteAliasResponse (..),
    newDeleteAliasResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | A unique identifier of the alias that you want to delete. You can use
    -- either the alias ID or ARN value.
    aliasId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasId', 'deleteAlias_aliasId' - A unique identifier of the alias that you want to delete. You can use
-- either the alias ID or ARN value.
newDeleteAlias ::
  -- | 'aliasId'
  Core.Text ->
  DeleteAlias
newDeleteAlias pAliasId_ =
  DeleteAlias' {aliasId = pAliasId_}

-- | A unique identifier of the alias that you want to delete. You can use
-- either the alias ID or ARN value.
deleteAlias_aliasId :: Lens.Lens' DeleteAlias Core.Text
deleteAlias_aliasId = Lens.lens (\DeleteAlias' {aliasId} -> aliasId) (\s@DeleteAlias' {} a -> s {aliasId = a} :: DeleteAlias)

instance Core.AWSRequest DeleteAlias where
  type AWSResponse DeleteAlias = DeleteAliasResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteAliasResponse'

instance Core.Hashable DeleteAlias

instance Core.NFData DeleteAlias

instance Core.ToHeaders DeleteAlias where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("GameLift.DeleteAlias" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("AliasId" Core..= aliasId)]
      )

instance Core.ToPath DeleteAlias where
  toPath = Core.const "/"

instance Core.ToQuery DeleteAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAliasResponse ::
  DeleteAliasResponse
newDeleteAliasResponse = DeleteAliasResponse'

instance Core.NFData DeleteAliasResponse
