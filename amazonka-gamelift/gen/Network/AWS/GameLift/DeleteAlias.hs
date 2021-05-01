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

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | A unique identifier of the alias that you want to delete. You can use
    -- either the alias ID or ARN value.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteAlias
newDeleteAlias pAliasId_ =
  DeleteAlias' {aliasId = pAliasId_}

-- | A unique identifier of the alias that you want to delete. You can use
-- either the alias ID or ARN value.
deleteAlias_aliasId :: Lens.Lens' DeleteAlias Prelude.Text
deleteAlias_aliasId = Lens.lens (\DeleteAlias' {aliasId} -> aliasId) (\s@DeleteAlias' {} a -> s {aliasId = a} :: DeleteAlias)

instance Prelude.AWSRequest DeleteAlias where
  type Rs DeleteAlias = DeleteAliasResponse
  request = Request.postJSON defaultService
  response = Response.receiveNull DeleteAliasResponse'

instance Prelude.Hashable DeleteAlias

instance Prelude.NFData DeleteAlias

instance Prelude.ToHeaders DeleteAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("GameLift.DeleteAlias" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasId" Prelude..= aliasId)]
      )

instance Prelude.ToPath DeleteAlias where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAliasResponse ::
  DeleteAliasResponse
newDeleteAliasResponse = DeleteAliasResponse'

instance Prelude.NFData DeleteAliasResponse
