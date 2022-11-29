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
-- Module      : Amazonka.LexV2Models.DeleteBot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all versions of a bot, including the @Draft@ version. To delete
-- a specific version, use the @DeleteBotVersion@ operation.
--
-- When you delete a bot, all of the resources contained in the bot are
-- also deleted. Deleting a bot removes all locales, intents, slot, and
-- slot types defined for the bot.
--
-- If a bot has an alias, the @DeleteBot@ operation returns a
-- @ResourceInUseException@ exception. If you want to delete the bot and
-- the alias, set the @skipResourceInUseCheck@ parameter to @true@.
module Amazonka.LexV2Models.DeleteBot
  ( -- * Creating a Request
    DeleteBot (..),
    newDeleteBot,

    -- * Request Lenses
    deleteBot_skipResourceInUseCheck,
    deleteBot_botId,

    -- * Destructuring the Response
    DeleteBotResponse (..),
    newDeleteBotResponse,

    -- * Response Lenses
    deleteBotResponse_botId,
    deleteBotResponse_botStatus,
    deleteBotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBot' smart constructor.
data DeleteBot = DeleteBot'
  { -- | When @true@, Amazon Lex doesn\'t check to see if another resource, such
    -- as an alias, is using the bot before it is deleted.
    skipResourceInUseCheck :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the bot to delete.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skipResourceInUseCheck', 'deleteBot_skipResourceInUseCheck' - When @true@, Amazon Lex doesn\'t check to see if another resource, such
-- as an alias, is using the bot before it is deleted.
--
-- 'botId', 'deleteBot_botId' - The identifier of the bot to delete.
newDeleteBot ::
  -- | 'botId'
  Prelude.Text ->
  DeleteBot
newDeleteBot pBotId_ =
  DeleteBot'
    { skipResourceInUseCheck =
        Prelude.Nothing,
      botId = pBotId_
    }

-- | When @true@, Amazon Lex doesn\'t check to see if another resource, such
-- as an alias, is using the bot before it is deleted.
deleteBot_skipResourceInUseCheck :: Lens.Lens' DeleteBot (Prelude.Maybe Prelude.Bool)
deleteBot_skipResourceInUseCheck = Lens.lens (\DeleteBot' {skipResourceInUseCheck} -> skipResourceInUseCheck) (\s@DeleteBot' {} a -> s {skipResourceInUseCheck = a} :: DeleteBot)

-- | The identifier of the bot to delete.
deleteBot_botId :: Lens.Lens' DeleteBot Prelude.Text
deleteBot_botId = Lens.lens (\DeleteBot' {botId} -> botId) (\s@DeleteBot' {} a -> s {botId = a} :: DeleteBot)

instance Core.AWSRequest DeleteBot where
  type AWSResponse DeleteBot = DeleteBotResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBotResponse'
            Prelude.<$> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "botStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBot where
  hashWithSalt _salt DeleteBot' {..} =
    _salt `Prelude.hashWithSalt` skipResourceInUseCheck
      `Prelude.hashWithSalt` botId

instance Prelude.NFData DeleteBot where
  rnf DeleteBot' {..} =
    Prelude.rnf skipResourceInUseCheck
      `Prelude.seq` Prelude.rnf botId

instance Core.ToHeaders DeleteBot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteBot where
  toPath DeleteBot' {..} =
    Prelude.mconcat ["/bots/", Core.toBS botId, "/"]

instance Core.ToQuery DeleteBot where
  toQuery DeleteBot' {..} =
    Prelude.mconcat
      [ "skipResourceInUseCheck"
          Core.=: skipResourceInUseCheck
      ]

-- | /See:/ 'newDeleteBotResponse' smart constructor.
data DeleteBotResponse = DeleteBotResponse'
  { -- | The unique identifier of the bot that Amazon Lex is deleting.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the bot. The status is @Deleting@ while the bot
    -- and its associated resources are being deleted.
    botStatus :: Prelude.Maybe BotStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'deleteBotResponse_botId' - The unique identifier of the bot that Amazon Lex is deleting.
--
-- 'botStatus', 'deleteBotResponse_botStatus' - The current status of the bot. The status is @Deleting@ while the bot
-- and its associated resources are being deleted.
--
-- 'httpStatus', 'deleteBotResponse_httpStatus' - The response's http status code.
newDeleteBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteBotResponse
newDeleteBotResponse pHttpStatus_ =
  DeleteBotResponse'
    { botId = Prelude.Nothing,
      botStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot that Amazon Lex is deleting.
deleteBotResponse_botId :: Lens.Lens' DeleteBotResponse (Prelude.Maybe Prelude.Text)
deleteBotResponse_botId = Lens.lens (\DeleteBotResponse' {botId} -> botId) (\s@DeleteBotResponse' {} a -> s {botId = a} :: DeleteBotResponse)

-- | The current status of the bot. The status is @Deleting@ while the bot
-- and its associated resources are being deleted.
deleteBotResponse_botStatus :: Lens.Lens' DeleteBotResponse (Prelude.Maybe BotStatus)
deleteBotResponse_botStatus = Lens.lens (\DeleteBotResponse' {botStatus} -> botStatus) (\s@DeleteBotResponse' {} a -> s {botStatus = a} :: DeleteBotResponse)

-- | The response's http status code.
deleteBotResponse_httpStatus :: Lens.Lens' DeleteBotResponse Prelude.Int
deleteBotResponse_httpStatus = Lens.lens (\DeleteBotResponse' {httpStatus} -> httpStatus) (\s@DeleteBotResponse' {} a -> s {httpStatus = a} :: DeleteBotResponse)

instance Prelude.NFData DeleteBotResponse where
  rnf DeleteBotResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botStatus
      `Prelude.seq` Prelude.rnf httpStatus
