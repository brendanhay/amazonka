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
-- Module      : Amazonka.Chime.UpdateBot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the status of the specified bot, such as starting or stopping
-- the bot from running in your Amazon Chime Enterprise account.
module Amazonka.Chime.UpdateBot
  ( -- * Creating a Request
    UpdateBot (..),
    newUpdateBot,

    -- * Request Lenses
    updateBot_disabled,
    updateBot_accountId,
    updateBot_botId,

    -- * Destructuring the Response
    UpdateBotResponse (..),
    newUpdateBotResponse,

    -- * Response Lenses
    updateBotResponse_bot,
    updateBotResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBot' smart constructor.
data UpdateBot = UpdateBot'
  { -- | When true, stops the specified bot from running in your account.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The bot ID.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabled', 'updateBot_disabled' - When true, stops the specified bot from running in your account.
--
-- 'accountId', 'updateBot_accountId' - The Amazon Chime account ID.
--
-- 'botId', 'updateBot_botId' - The bot ID.
newUpdateBot ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  UpdateBot
newUpdateBot pAccountId_ pBotId_ =
  UpdateBot'
    { disabled = Prelude.Nothing,
      accountId = pAccountId_,
      botId = pBotId_
    }

-- | When true, stops the specified bot from running in your account.
updateBot_disabled :: Lens.Lens' UpdateBot (Prelude.Maybe Prelude.Bool)
updateBot_disabled = Lens.lens (\UpdateBot' {disabled} -> disabled) (\s@UpdateBot' {} a -> s {disabled = a} :: UpdateBot)

-- | The Amazon Chime account ID.
updateBot_accountId :: Lens.Lens' UpdateBot Prelude.Text
updateBot_accountId = Lens.lens (\UpdateBot' {accountId} -> accountId) (\s@UpdateBot' {} a -> s {accountId = a} :: UpdateBot)

-- | The bot ID.
updateBot_botId :: Lens.Lens' UpdateBot Prelude.Text
updateBot_botId = Lens.lens (\UpdateBot' {botId} -> botId) (\s@UpdateBot' {} a -> s {botId = a} :: UpdateBot)

instance Core.AWSRequest UpdateBot where
  type AWSResponse UpdateBot = UpdateBotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBotResponse'
            Prelude.<$> (x Data..?> "Bot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBot where
  hashWithSalt _salt UpdateBot' {..} =
    _salt `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` botId

instance Prelude.NFData UpdateBot where
  rnf UpdateBot' {..} =
    Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders UpdateBot where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateBot where
  toJSON UpdateBot' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Disabled" Data..=) Prelude.<$> disabled]
      )

instance Data.ToPath UpdateBot where
  toPath UpdateBot' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/bots/",
        Data.toBS botId
      ]

instance Data.ToQuery UpdateBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBotResponse' smart constructor.
data UpdateBotResponse = UpdateBotResponse'
  { -- | The updated bot details.
    bot :: Prelude.Maybe Bot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bot', 'updateBotResponse_bot' - The updated bot details.
--
-- 'httpStatus', 'updateBotResponse_httpStatus' - The response's http status code.
newUpdateBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBotResponse
newUpdateBotResponse pHttpStatus_ =
  UpdateBotResponse'
    { bot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated bot details.
updateBotResponse_bot :: Lens.Lens' UpdateBotResponse (Prelude.Maybe Bot)
updateBotResponse_bot = Lens.lens (\UpdateBotResponse' {bot} -> bot) (\s@UpdateBotResponse' {} a -> s {bot = a} :: UpdateBotResponse)

-- | The response's http status code.
updateBotResponse_httpStatus :: Lens.Lens' UpdateBotResponse Prelude.Int
updateBotResponse_httpStatus = Lens.lens (\UpdateBotResponse' {httpStatus} -> httpStatus) (\s@UpdateBotResponse' {} a -> s {httpStatus = a} :: UpdateBotResponse)

instance Prelude.NFData UpdateBotResponse where
  rnf UpdateBotResponse' {..} =
    Prelude.rnf bot
      `Prelude.seq` Prelude.rnf httpStatus
