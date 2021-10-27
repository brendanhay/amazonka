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
-- Module      : Network.AWS.LexV2Models.DeleteBotVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a bot. To delete all version of a bot, use
-- the DeleteBot operation.
module Network.AWS.LexV2Models.DeleteBotVersion
  ( -- * Creating a Request
    DeleteBotVersion (..),
    newDeleteBotVersion,

    -- * Request Lenses
    deleteBotVersion_skipResourceInUseCheck,
    deleteBotVersion_botId,
    deleteBotVersion_botVersion,

    -- * Destructuring the Response
    DeleteBotVersionResponse (..),
    newDeleteBotVersionResponse,

    -- * Response Lenses
    deleteBotVersionResponse_botStatus,
    deleteBotVersionResponse_botVersion,
    deleteBotVersionResponse_botId,
    deleteBotVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBotVersion' smart constructor.
data DeleteBotVersion = DeleteBotVersion'
  { -- | By default, the @DeleteBotVersion@ operations throws a
    -- @ResourceInUseException@ exception if you try to delete a bot version
    -- that has an alias pointing at it. Set the @skipResourceInUseCheck@
    -- parameter to @true@ to skip this check and remove the version even if an
    -- alias points to it.
    skipResourceInUseCheck :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the bot that contains the version.
    botId :: Prelude.Text,
    -- | The version of the bot to delete.
    botVersion :: Prelude.Text
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
-- 'skipResourceInUseCheck', 'deleteBotVersion_skipResourceInUseCheck' - By default, the @DeleteBotVersion@ operations throws a
-- @ResourceInUseException@ exception if you try to delete a bot version
-- that has an alias pointing at it. Set the @skipResourceInUseCheck@
-- parameter to @true@ to skip this check and remove the version even if an
-- alias points to it.
--
-- 'botId', 'deleteBotVersion_botId' - The identifier of the bot that contains the version.
--
-- 'botVersion', 'deleteBotVersion_botVersion' - The version of the bot to delete.
newDeleteBotVersion ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  DeleteBotVersion
newDeleteBotVersion pBotId_ pBotVersion_ =
  DeleteBotVersion'
    { skipResourceInUseCheck =
        Prelude.Nothing,
      botId = pBotId_,
      botVersion = pBotVersion_
    }

-- | By default, the @DeleteBotVersion@ operations throws a
-- @ResourceInUseException@ exception if you try to delete a bot version
-- that has an alias pointing at it. Set the @skipResourceInUseCheck@
-- parameter to @true@ to skip this check and remove the version even if an
-- alias points to it.
deleteBotVersion_skipResourceInUseCheck :: Lens.Lens' DeleteBotVersion (Prelude.Maybe Prelude.Bool)
deleteBotVersion_skipResourceInUseCheck = Lens.lens (\DeleteBotVersion' {skipResourceInUseCheck} -> skipResourceInUseCheck) (\s@DeleteBotVersion' {} a -> s {skipResourceInUseCheck = a} :: DeleteBotVersion)

-- | The identifier of the bot that contains the version.
deleteBotVersion_botId :: Lens.Lens' DeleteBotVersion Prelude.Text
deleteBotVersion_botId = Lens.lens (\DeleteBotVersion' {botId} -> botId) (\s@DeleteBotVersion' {} a -> s {botId = a} :: DeleteBotVersion)

-- | The version of the bot to delete.
deleteBotVersion_botVersion :: Lens.Lens' DeleteBotVersion Prelude.Text
deleteBotVersion_botVersion = Lens.lens (\DeleteBotVersion' {botVersion} -> botVersion) (\s@DeleteBotVersion' {} a -> s {botVersion = a} :: DeleteBotVersion)

instance Core.AWSRequest DeleteBotVersion where
  type
    AWSResponse DeleteBotVersion =
      DeleteBotVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBotVersionResponse'
            Prelude.<$> (x Core..?> "botStatus")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBotVersion

instance Prelude.NFData DeleteBotVersion

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
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/"
      ]

instance Core.ToQuery DeleteBotVersion where
  toQuery DeleteBotVersion' {..} =
    Prelude.mconcat
      [ "skipResourceInUseCheck"
          Core.=: skipResourceInUseCheck
      ]

-- | /See:/ 'newDeleteBotVersionResponse' smart constructor.
data DeleteBotVersionResponse = DeleteBotVersionResponse'
  { -- | The current status of the bot.
    botStatus :: Prelude.Maybe BotStatus,
    -- | The version of the bot that is being deleted.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that is being deleted.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botStatus', 'deleteBotVersionResponse_botStatus' - The current status of the bot.
--
-- 'botVersion', 'deleteBotVersionResponse_botVersion' - The version of the bot that is being deleted.
--
-- 'botId', 'deleteBotVersionResponse_botId' - The identifier of the bot that is being deleted.
--
-- 'httpStatus', 'deleteBotVersionResponse_httpStatus' - The response's http status code.
newDeleteBotVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBotVersionResponse
newDeleteBotVersionResponse pHttpStatus_ =
  DeleteBotVersionResponse'
    { botStatus =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      botId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current status of the bot.
deleteBotVersionResponse_botStatus :: Lens.Lens' DeleteBotVersionResponse (Prelude.Maybe BotStatus)
deleteBotVersionResponse_botStatus = Lens.lens (\DeleteBotVersionResponse' {botStatus} -> botStatus) (\s@DeleteBotVersionResponse' {} a -> s {botStatus = a} :: DeleteBotVersionResponse)

-- | The version of the bot that is being deleted.
deleteBotVersionResponse_botVersion :: Lens.Lens' DeleteBotVersionResponse (Prelude.Maybe Prelude.Text)
deleteBotVersionResponse_botVersion = Lens.lens (\DeleteBotVersionResponse' {botVersion} -> botVersion) (\s@DeleteBotVersionResponse' {} a -> s {botVersion = a} :: DeleteBotVersionResponse)

-- | The identifier of the bot that is being deleted.
deleteBotVersionResponse_botId :: Lens.Lens' DeleteBotVersionResponse (Prelude.Maybe Prelude.Text)
deleteBotVersionResponse_botId = Lens.lens (\DeleteBotVersionResponse' {botId} -> botId) (\s@DeleteBotVersionResponse' {} a -> s {botId = a} :: DeleteBotVersionResponse)

-- | The response's http status code.
deleteBotVersionResponse_httpStatus :: Lens.Lens' DeleteBotVersionResponse Prelude.Int
deleteBotVersionResponse_httpStatus = Lens.lens (\DeleteBotVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteBotVersionResponse' {} a -> s {httpStatus = a} :: DeleteBotVersionResponse)

instance Prelude.NFData DeleteBotVersionResponse
