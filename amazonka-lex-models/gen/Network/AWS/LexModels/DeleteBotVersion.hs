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
-- Module      : Network.AWS.LexModels.DeleteBotVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.LexModels.DeleteBotVersion
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBotVersion' smart constructor.
data DeleteBotVersion = DeleteBotVersion'
  { -- | The name of the bot.
    name :: Core.Text,
    -- | The version of the bot to delete. You cannot delete the @$LATEST@
    -- version of the bot. To delete the @$LATEST@ version, use the DeleteBot
    -- operation.
    version :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'version'
  Core.Text ->
  DeleteBotVersion
newDeleteBotVersion pName_ pVersion_ =
  DeleteBotVersion'
    { name = pName_,
      version = pVersion_
    }

-- | The name of the bot.
deleteBotVersion_name :: Lens.Lens' DeleteBotVersion Core.Text
deleteBotVersion_name = Lens.lens (\DeleteBotVersion' {name} -> name) (\s@DeleteBotVersion' {} a -> s {name = a} :: DeleteBotVersion)

-- | The version of the bot to delete. You cannot delete the @$LATEST@
-- version of the bot. To delete the @$LATEST@ version, use the DeleteBot
-- operation.
deleteBotVersion_version :: Lens.Lens' DeleteBotVersion Core.Text
deleteBotVersion_version = Lens.lens (\DeleteBotVersion' {version} -> version) (\s@DeleteBotVersion' {} a -> s {version = a} :: DeleteBotVersion)

instance Core.AWSRequest DeleteBotVersion where
  type
    AWSResponse DeleteBotVersion =
      DeleteBotVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteBotVersionResponse'

instance Core.Hashable DeleteBotVersion

instance Core.NFData DeleteBotVersion

instance Core.ToHeaders DeleteBotVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteBotVersion where
  toPath DeleteBotVersion' {..} =
    Core.mconcat
      [ "/bots/",
        Core.toBS name,
        "/versions/",
        Core.toBS version
      ]

instance Core.ToQuery DeleteBotVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteBotVersionResponse' smart constructor.
data DeleteBotVersionResponse = DeleteBotVersionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBotVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBotVersionResponse ::
  DeleteBotVersionResponse
newDeleteBotVersionResponse =
  DeleteBotVersionResponse'

instance Core.NFData DeleteBotVersionResponse
