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
-- Module      : Amazonka.GameLift.DeleteAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alias. This operation removes all record of the alias. Game
-- clients attempting to access a server process using the deleted alias
-- receive an error. To delete an alias, specify the alias ID to be
-- deleted.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DeleteAlias
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteAlias' smart constructor.
data DeleteAlias = DeleteAlias'
  { -- | A unique identifier of the alias that you want to delete. You can use
    -- either the alias ID or ARN value.
    aliasId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteAlias where
  type AWSResponse DeleteAlias = DeleteAliasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteAliasResponse'

instance Prelude.Hashable DeleteAlias where
  hashWithSalt _salt DeleteAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasId

instance Prelude.NFData DeleteAlias where
  rnf DeleteAlias' {..} = Prelude.rnf aliasId

instance Data.ToHeaders DeleteAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.DeleteAlias" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAlias where
  toJSON DeleteAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("AliasId" Data..= aliasId)]
      )

instance Data.ToPath DeleteAlias where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAliasResponse' smart constructor.
data DeleteAliasResponse = DeleteAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteAliasResponse ::
  DeleteAliasResponse
newDeleteAliasResponse = DeleteAliasResponse'

instance Prelude.NFData DeleteAliasResponse where
  rnf _ = ()
