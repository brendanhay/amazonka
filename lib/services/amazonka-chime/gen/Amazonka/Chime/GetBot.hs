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
-- Module      : Amazonka.Chime.GetBot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details for the specified bot, such as bot email address, bot
-- type, status, and display name.
module Amazonka.Chime.GetBot
  ( -- * Creating a Request
    GetBot (..),
    newGetBot,

    -- * Request Lenses
    getBot_accountId,
    getBot_botId,

    -- * Destructuring the Response
    GetBotResponse (..),
    newGetBotResponse,

    -- * Response Lenses
    getBotResponse_bot,
    getBotResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBot' smart constructor.
data GetBot = GetBot'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The bot ID.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getBot_accountId' - The Amazon Chime account ID.
--
-- 'botId', 'getBot_botId' - The bot ID.
newGetBot ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  GetBot
newGetBot pAccountId_ pBotId_ =
  GetBot' {accountId = pAccountId_, botId = pBotId_}

-- | The Amazon Chime account ID.
getBot_accountId :: Lens.Lens' GetBot Prelude.Text
getBot_accountId = Lens.lens (\GetBot' {accountId} -> accountId) (\s@GetBot' {} a -> s {accountId = a} :: GetBot)

-- | The bot ID.
getBot_botId :: Lens.Lens' GetBot Prelude.Text
getBot_botId = Lens.lens (\GetBot' {botId} -> botId) (\s@GetBot' {} a -> s {botId = a} :: GetBot)

instance Core.AWSRequest GetBot where
  type AWSResponse GetBot = GetBotResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotResponse'
            Prelude.<$> (x Data..?> "Bot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBot where
  hashWithSalt _salt GetBot' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` botId

instance Prelude.NFData GetBot where
  rnf GetBot' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders GetBot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetBot where
  toPath GetBot' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/bots/",
        Data.toBS botId
      ]

instance Data.ToQuery GetBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBotResponse' smart constructor.
data GetBotResponse = GetBotResponse'
  { -- | The chat bot details.
    bot :: Prelude.Maybe Bot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bot', 'getBotResponse_bot' - The chat bot details.
--
-- 'httpStatus', 'getBotResponse_httpStatus' - The response's http status code.
newGetBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotResponse
newGetBotResponse pHttpStatus_ =
  GetBotResponse'
    { bot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The chat bot details.
getBotResponse_bot :: Lens.Lens' GetBotResponse (Prelude.Maybe Bot)
getBotResponse_bot = Lens.lens (\GetBotResponse' {bot} -> bot) (\s@GetBotResponse' {} a -> s {bot = a} :: GetBotResponse)

-- | The response's http status code.
getBotResponse_httpStatus :: Lens.Lens' GetBotResponse Prelude.Int
getBotResponse_httpStatus = Lens.lens (\GetBotResponse' {httpStatus} -> httpStatus) (\s@GetBotResponse' {} a -> s {httpStatus = a} :: GetBotResponse)

instance Prelude.NFData GetBotResponse where
  rnf GetBotResponse' {..} =
    Prelude.rnf bot
      `Prelude.seq` Prelude.rnf httpStatus
