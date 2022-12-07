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
-- Module      : Amazonka.LexV2Models.DeleteBotVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific version of a bot. To delete all version of a bot, use
-- the
-- <https://docs.aws.amazon.com/lexv2/latest/dg/API_DeleteBot.html DeleteBot>
-- operation.
module Amazonka.LexV2Models.DeleteBotVersion
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
    deleteBotVersionResponse_botVersion,
    deleteBotVersionResponse_botId,
    deleteBotVersionResponse_botStatus,
    deleteBotVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBotVersionResponse'
            Prelude.<$> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBotVersion where
  hashWithSalt _salt DeleteBotVersion' {..} =
    _salt `Prelude.hashWithSalt` skipResourceInUseCheck
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion

instance Prelude.NFData DeleteBotVersion where
  rnf DeleteBotVersion' {..} =
    Prelude.rnf skipResourceInUseCheck
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion

instance Data.ToHeaders DeleteBotVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBotVersion where
  toPath DeleteBotVersion' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/"
      ]

instance Data.ToQuery DeleteBotVersion where
  toQuery DeleteBotVersion' {..} =
    Prelude.mconcat
      [ "skipResourceInUseCheck"
          Data.=: skipResourceInUseCheck
      ]

-- | /See:/ 'newDeleteBotVersionResponse' smart constructor.
data DeleteBotVersionResponse = DeleteBotVersionResponse'
  { -- | The version of the bot that is being deleted.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot that is being deleted.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the bot.
    botStatus :: Prelude.Maybe BotStatus,
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
-- 'botVersion', 'deleteBotVersionResponse_botVersion' - The version of the bot that is being deleted.
--
-- 'botId', 'deleteBotVersionResponse_botId' - The identifier of the bot that is being deleted.
--
-- 'botStatus', 'deleteBotVersionResponse_botStatus' - The current status of the bot.
--
-- 'httpStatus', 'deleteBotVersionResponse_httpStatus' - The response's http status code.
newDeleteBotVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBotVersionResponse
newDeleteBotVersionResponse pHttpStatus_ =
  DeleteBotVersionResponse'
    { botVersion =
        Prelude.Nothing,
      botId = Prelude.Nothing,
      botStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The version of the bot that is being deleted.
deleteBotVersionResponse_botVersion :: Lens.Lens' DeleteBotVersionResponse (Prelude.Maybe Prelude.Text)
deleteBotVersionResponse_botVersion = Lens.lens (\DeleteBotVersionResponse' {botVersion} -> botVersion) (\s@DeleteBotVersionResponse' {} a -> s {botVersion = a} :: DeleteBotVersionResponse)

-- | The identifier of the bot that is being deleted.
deleteBotVersionResponse_botId :: Lens.Lens' DeleteBotVersionResponse (Prelude.Maybe Prelude.Text)
deleteBotVersionResponse_botId = Lens.lens (\DeleteBotVersionResponse' {botId} -> botId) (\s@DeleteBotVersionResponse' {} a -> s {botId = a} :: DeleteBotVersionResponse)

-- | The current status of the bot.
deleteBotVersionResponse_botStatus :: Lens.Lens' DeleteBotVersionResponse (Prelude.Maybe BotStatus)
deleteBotVersionResponse_botStatus = Lens.lens (\DeleteBotVersionResponse' {botStatus} -> botStatus) (\s@DeleteBotVersionResponse' {} a -> s {botStatus = a} :: DeleteBotVersionResponse)

-- | The response's http status code.
deleteBotVersionResponse_httpStatus :: Lens.Lens' DeleteBotVersionResponse Prelude.Int
deleteBotVersionResponse_httpStatus = Lens.lens (\DeleteBotVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteBotVersionResponse' {} a -> s {httpStatus = a} :: DeleteBotVersionResponse)

instance Prelude.NFData DeleteBotVersionResponse where
  rnf DeleteBotVersionResponse' {..} =
    Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botStatus
      `Prelude.seq` Prelude.rnf httpStatus
