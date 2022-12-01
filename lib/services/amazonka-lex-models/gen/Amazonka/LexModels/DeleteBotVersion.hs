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
-- Module      : Amazonka.LexModels.DeleteBotVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a bot. To delete all versions of a bot,
-- use the DeleteBot operation.
--
-- This operation requires permissions for the @lex:DeleteBotVersion@
-- action.
module Amazonka.LexModels.DeleteBotVersion
  ( -- * Creating a Request
    DeleteBotVersion (..),
    newDeleteBotVersion,

    -- * Request Lenses
    deleteBotVersion_name,
    deleteBotVersion_version,

    -- * Destructuring the Response
    DeleteBotVersionResponse (..),
    newDeleteBotVersionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBotVersion' smart constructor.
data DeleteBotVersion = DeleteBotVersion'
  { -- | The name of the bot.
    name :: Prelude.Text,
    -- | The version of the bot to delete. You cannot delete the @$LATEST@
    -- version of the bot. To delete the @$LATEST@ version, use the DeleteBot
    -- operation.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteBotVersion_name' - The name of the bot.
--
-- 'version', 'deleteBotVersion_version' - The version of the bot to delete. You cannot delete the @$LATEST@
-- version of the bot. To delete the @$LATEST@ version, use the DeleteBot
-- operation.
newDeleteBotVersion ::
  -- | 'name'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  DeleteBotVersion
newDeleteBotVersion pName_ pVersion_ =
  DeleteBotVersion'
    { name = pName_,
      version = pVersion_
    }

-- | The name of the bot.
deleteBotVersion_name :: Lens.Lens' DeleteBotVersion Prelude.Text
deleteBotVersion_name = Lens.lens (\DeleteBotVersion' {name} -> name) (\s@DeleteBotVersion' {} a -> s {name = a} :: DeleteBotVersion)

-- | The version of the bot to delete. You cannot delete the @$LATEST@
-- version of the bot. To delete the @$LATEST@ version, use the DeleteBot
-- operation.
deleteBotVersion_version :: Lens.Lens' DeleteBotVersion Prelude.Text
deleteBotVersion_version = Lens.lens (\DeleteBotVersion' {version} -> version) (\s@DeleteBotVersion' {} a -> s {version = a} :: DeleteBotVersion)

instance Core.AWSRequest DeleteBotVersion where
  type
    AWSResponse DeleteBotVersion =
      DeleteBotVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteBotVersionResponse'

instance Prelude.Hashable DeleteBotVersion where
  hashWithSalt _salt DeleteBotVersion' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` version

instance Prelude.NFData DeleteBotVersion where
  rnf DeleteBotVersion' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf version

instance Core.ToHeaders DeleteBotVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteBotVersion where
  toPath DeleteBotVersion' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS name,
        "/versions/",
        Core.toBS version
      ]

instance Core.ToQuery DeleteBotVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBotVersionResponse' smart constructor.
data DeleteBotVersionResponse = DeleteBotVersionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBotVersionResponse ::
  DeleteBotVersionResponse
newDeleteBotVersionResponse =
  DeleteBotVersionResponse'

instance Prelude.NFData DeleteBotVersionResponse where
  rnf _ = ()
