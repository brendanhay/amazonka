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
-- Module      : Amazonka.Chime.CreateBot
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a bot for an Amazon Chime Enterprise account.
module Amazonka.Chime.CreateBot
  ( -- * Creating a Request
    CreateBot (..),
    newCreateBot,

    -- * Request Lenses
    createBot_domain,
    createBot_displayName,
    createBot_accountId,

    -- * Destructuring the Response
    CreateBotResponse (..),
    newCreateBotResponse,

    -- * Response Lenses
    createBotResponse_bot,
    createBotResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBot' smart constructor.
data CreateBot = CreateBot'
  { -- | The domain of the Amazon Chime Enterprise account.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The bot display name.
    displayName :: Core.Sensitive Prelude.Text,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'createBot_domain' - The domain of the Amazon Chime Enterprise account.
--
-- 'displayName', 'createBot_displayName' - The bot display name.
--
-- 'accountId', 'createBot_accountId' - The Amazon Chime account ID.
newCreateBot ::
  -- | 'displayName'
  Prelude.Text ->
  -- | 'accountId'
  Prelude.Text ->
  CreateBot
newCreateBot pDisplayName_ pAccountId_ =
  CreateBot'
    { domain = Prelude.Nothing,
      displayName = Core._Sensitive Lens.# pDisplayName_,
      accountId = pAccountId_
    }

-- | The domain of the Amazon Chime Enterprise account.
createBot_domain :: Lens.Lens' CreateBot (Prelude.Maybe Prelude.Text)
createBot_domain = Lens.lens (\CreateBot' {domain} -> domain) (\s@CreateBot' {} a -> s {domain = a} :: CreateBot)

-- | The bot display name.
createBot_displayName :: Lens.Lens' CreateBot Prelude.Text
createBot_displayName = Lens.lens (\CreateBot' {displayName} -> displayName) (\s@CreateBot' {} a -> s {displayName = a} :: CreateBot) Prelude.. Core._Sensitive

-- | The Amazon Chime account ID.
createBot_accountId :: Lens.Lens' CreateBot Prelude.Text
createBot_accountId = Lens.lens (\CreateBot' {accountId} -> accountId) (\s@CreateBot' {} a -> s {accountId = a} :: CreateBot)

instance Core.AWSRequest CreateBot where
  type AWSResponse CreateBot = CreateBotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBotResponse'
            Prelude.<$> (x Core..?> "Bot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBot where
  hashWithSalt _salt CreateBot' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` accountId

instance Prelude.NFData CreateBot where
  rnf CreateBot' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf accountId

instance Core.ToHeaders CreateBot where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateBot where
  toJSON CreateBot' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Domain" Core..=) Prelude.<$> domain,
            Prelude.Just ("DisplayName" Core..= displayName)
          ]
      )

instance Core.ToPath CreateBot where
  toPath CreateBot' {..} =
    Prelude.mconcat
      ["/accounts/", Core.toBS accountId, "/bots"]

instance Core.ToQuery CreateBot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBotResponse' smart constructor.
data CreateBotResponse = CreateBotResponse'
  { -- | The bot details.
    bot :: Prelude.Maybe Bot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bot', 'createBotResponse_bot' - The bot details.
--
-- 'httpStatus', 'createBotResponse_httpStatus' - The response's http status code.
newCreateBotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBotResponse
newCreateBotResponse pHttpStatus_ =
  CreateBotResponse'
    { bot = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The bot details.
createBotResponse_bot :: Lens.Lens' CreateBotResponse (Prelude.Maybe Bot)
createBotResponse_bot = Lens.lens (\CreateBotResponse' {bot} -> bot) (\s@CreateBotResponse' {} a -> s {bot = a} :: CreateBotResponse)

-- | The response's http status code.
createBotResponse_httpStatus :: Lens.Lens' CreateBotResponse Prelude.Int
createBotResponse_httpStatus = Lens.lens (\CreateBotResponse' {httpStatus} -> httpStatus) (\s@CreateBotResponse' {} a -> s {httpStatus = a} :: CreateBotResponse)

instance Prelude.NFData CreateBotResponse where
  rnf CreateBotResponse' {..} =
    Prelude.rnf bot
      `Prelude.seq` Prelude.rnf httpStatus
