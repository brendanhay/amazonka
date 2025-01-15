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
-- Module      : Amazonka.Chime.RegenerateSecurityToken
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Regenerates the security token for a bot.
module Amazonka.Chime.RegenerateSecurityToken
  ( -- * Creating a Request
    RegenerateSecurityToken (..),
    newRegenerateSecurityToken,

    -- * Request Lenses
    regenerateSecurityToken_accountId,
    regenerateSecurityToken_botId,

    -- * Destructuring the Response
    RegenerateSecurityTokenResponse (..),
    newRegenerateSecurityTokenResponse,

    -- * Response Lenses
    regenerateSecurityTokenResponse_bot,
    regenerateSecurityTokenResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRegenerateSecurityToken' smart constructor.
data RegenerateSecurityToken = RegenerateSecurityToken'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The bot ID.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegenerateSecurityToken' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'regenerateSecurityToken_accountId' - The Amazon Chime account ID.
--
-- 'botId', 'regenerateSecurityToken_botId' - The bot ID.
newRegenerateSecurityToken ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  RegenerateSecurityToken
newRegenerateSecurityToken pAccountId_ pBotId_ =
  RegenerateSecurityToken'
    { accountId = pAccountId_,
      botId = pBotId_
    }

-- | The Amazon Chime account ID.
regenerateSecurityToken_accountId :: Lens.Lens' RegenerateSecurityToken Prelude.Text
regenerateSecurityToken_accountId = Lens.lens (\RegenerateSecurityToken' {accountId} -> accountId) (\s@RegenerateSecurityToken' {} a -> s {accountId = a} :: RegenerateSecurityToken)

-- | The bot ID.
regenerateSecurityToken_botId :: Lens.Lens' RegenerateSecurityToken Prelude.Text
regenerateSecurityToken_botId = Lens.lens (\RegenerateSecurityToken' {botId} -> botId) (\s@RegenerateSecurityToken' {} a -> s {botId = a} :: RegenerateSecurityToken)

instance Core.AWSRequest RegenerateSecurityToken where
  type
    AWSResponse RegenerateSecurityToken =
      RegenerateSecurityTokenResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RegenerateSecurityTokenResponse'
            Prelude.<$> (x Data..?> "Bot")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RegenerateSecurityToken where
  hashWithSalt _salt RegenerateSecurityToken' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` botId

instance Prelude.NFData RegenerateSecurityToken where
  rnf RegenerateSecurityToken' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf botId

instance Data.ToHeaders RegenerateSecurityToken where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON RegenerateSecurityToken where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath RegenerateSecurityToken where
  toPath RegenerateSecurityToken' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/bots/",
        Data.toBS botId
      ]

instance Data.ToQuery RegenerateSecurityToken where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          ["operation=regenerate-security-token"]
      )

-- | /See:/ 'newRegenerateSecurityTokenResponse' smart constructor.
data RegenerateSecurityTokenResponse = RegenerateSecurityTokenResponse'
  { bot :: Prelude.Maybe Bot,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegenerateSecurityTokenResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bot', 'regenerateSecurityTokenResponse_bot' - Undocumented member.
--
-- 'httpStatus', 'regenerateSecurityTokenResponse_httpStatus' - The response's http status code.
newRegenerateSecurityTokenResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RegenerateSecurityTokenResponse
newRegenerateSecurityTokenResponse pHttpStatus_ =
  RegenerateSecurityTokenResponse'
    { bot =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
regenerateSecurityTokenResponse_bot :: Lens.Lens' RegenerateSecurityTokenResponse (Prelude.Maybe Bot)
regenerateSecurityTokenResponse_bot = Lens.lens (\RegenerateSecurityTokenResponse' {bot} -> bot) (\s@RegenerateSecurityTokenResponse' {} a -> s {bot = a} :: RegenerateSecurityTokenResponse)

-- | The response's http status code.
regenerateSecurityTokenResponse_httpStatus :: Lens.Lens' RegenerateSecurityTokenResponse Prelude.Int
regenerateSecurityTokenResponse_httpStatus = Lens.lens (\RegenerateSecurityTokenResponse' {httpStatus} -> httpStatus) (\s@RegenerateSecurityTokenResponse' {} a -> s {httpStatus = a} :: RegenerateSecurityTokenResponse)

instance
  Prelude.NFData
    RegenerateSecurityTokenResponse
  where
  rnf RegenerateSecurityTokenResponse' {..} =
    Prelude.rnf bot `Prelude.seq`
      Prelude.rnf httpStatus
