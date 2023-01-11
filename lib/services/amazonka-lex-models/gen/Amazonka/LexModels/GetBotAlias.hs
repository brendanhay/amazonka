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
-- Module      : Amazonka.LexModels.GetBotAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon Lex bot alias. For more information
-- about aliases, see versioning-aliases.
--
-- This operation requires permissions for the @lex:GetBotAlias@ action.
module Amazonka.LexModels.GetBotAlias
  ( -- * Creating a Request
    GetBotAlias (..),
    newGetBotAlias,

    -- * Request Lenses
    getBotAlias_name,
    getBotAlias_botName,

    -- * Destructuring the Response
    GetBotAliasResponse (..),
    newGetBotAliasResponse,

    -- * Response Lenses
    getBotAliasResponse_botName,
    getBotAliasResponse_botVersion,
    getBotAliasResponse_checksum,
    getBotAliasResponse_conversationLogs,
    getBotAliasResponse_createdDate,
    getBotAliasResponse_description,
    getBotAliasResponse_lastUpdatedDate,
    getBotAliasResponse_name,
    getBotAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBotAlias' smart constructor.
data GetBotAlias = GetBotAlias'
  { -- | The name of the bot alias. The name is case sensitive.
    name :: Prelude.Text,
    -- | The name of the bot.
    botName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getBotAlias_name' - The name of the bot alias. The name is case sensitive.
--
-- 'botName', 'getBotAlias_botName' - The name of the bot.
newGetBotAlias ::
  -- | 'name'
  Prelude.Text ->
  -- | 'botName'
  Prelude.Text ->
  GetBotAlias
newGetBotAlias pName_ pBotName_ =
  GetBotAlias' {name = pName_, botName = pBotName_}

-- | The name of the bot alias. The name is case sensitive.
getBotAlias_name :: Lens.Lens' GetBotAlias Prelude.Text
getBotAlias_name = Lens.lens (\GetBotAlias' {name} -> name) (\s@GetBotAlias' {} a -> s {name = a} :: GetBotAlias)

-- | The name of the bot.
getBotAlias_botName :: Lens.Lens' GetBotAlias Prelude.Text
getBotAlias_botName = Lens.lens (\GetBotAlias' {botName} -> botName) (\s@GetBotAlias' {} a -> s {botName = a} :: GetBotAlias)

instance Core.AWSRequest GetBotAlias where
  type AWSResponse GetBotAlias = GetBotAliasResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotAliasResponse'
            Prelude.<$> (x Data..?> "botName")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "checksum")
            Prelude.<*> (x Data..?> "conversationLogs")
            Prelude.<*> (x Data..?> "createdDate")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "lastUpdatedDate")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBotAlias where
  hashWithSalt _salt GetBotAlias' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` botName

instance Prelude.NFData GetBotAlias where
  rnf GetBotAlias' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf botName

instance Data.ToHeaders GetBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBotAlias where
  toPath GetBotAlias' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botName,
        "/aliases/",
        Data.toBS name
      ]

instance Data.ToQuery GetBotAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBotAliasResponse' smart constructor.
data GetBotAliasResponse = GetBotAliasResponse'
  { -- | The name of the bot that the alias points to.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that the alias points to.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Checksum of the bot alias.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The settings that determine how Amazon Lex uses conversation logs for
    -- the alias.
    conversationLogs :: Prelude.Maybe ConversationLogsResponse,
    -- | The date that the bot alias was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the bot alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date that the bot alias was updated. When you create a resource, the
    -- creation date and the last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the bot alias.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botName', 'getBotAliasResponse_botName' - The name of the bot that the alias points to.
--
-- 'botVersion', 'getBotAliasResponse_botVersion' - The version of the bot that the alias points to.
--
-- 'checksum', 'getBotAliasResponse_checksum' - Checksum of the bot alias.
--
-- 'conversationLogs', 'getBotAliasResponse_conversationLogs' - The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
--
-- 'createdDate', 'getBotAliasResponse_createdDate' - The date that the bot alias was created.
--
-- 'description', 'getBotAliasResponse_description' - A description of the bot alias.
--
-- 'lastUpdatedDate', 'getBotAliasResponse_lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
--
-- 'name', 'getBotAliasResponse_name' - The name of the bot alias.
--
-- 'httpStatus', 'getBotAliasResponse_httpStatus' - The response's http status code.
newGetBotAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBotAliasResponse
newGetBotAliasResponse pHttpStatus_ =
  GetBotAliasResponse'
    { botName = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      checksum = Prelude.Nothing,
      conversationLogs = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the bot that the alias points to.
getBotAliasResponse_botName :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe Prelude.Text)
getBotAliasResponse_botName = Lens.lens (\GetBotAliasResponse' {botName} -> botName) (\s@GetBotAliasResponse' {} a -> s {botName = a} :: GetBotAliasResponse)

-- | The version of the bot that the alias points to.
getBotAliasResponse_botVersion :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe Prelude.Text)
getBotAliasResponse_botVersion = Lens.lens (\GetBotAliasResponse' {botVersion} -> botVersion) (\s@GetBotAliasResponse' {} a -> s {botVersion = a} :: GetBotAliasResponse)

-- | Checksum of the bot alias.
getBotAliasResponse_checksum :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe Prelude.Text)
getBotAliasResponse_checksum = Lens.lens (\GetBotAliasResponse' {checksum} -> checksum) (\s@GetBotAliasResponse' {} a -> s {checksum = a} :: GetBotAliasResponse)

-- | The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
getBotAliasResponse_conversationLogs :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe ConversationLogsResponse)
getBotAliasResponse_conversationLogs = Lens.lens (\GetBotAliasResponse' {conversationLogs} -> conversationLogs) (\s@GetBotAliasResponse' {} a -> s {conversationLogs = a} :: GetBotAliasResponse)

-- | The date that the bot alias was created.
getBotAliasResponse_createdDate :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
getBotAliasResponse_createdDate = Lens.lens (\GetBotAliasResponse' {createdDate} -> createdDate) (\s@GetBotAliasResponse' {} a -> s {createdDate = a} :: GetBotAliasResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the bot alias.
getBotAliasResponse_description :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe Prelude.Text)
getBotAliasResponse_description = Lens.lens (\GetBotAliasResponse' {description} -> description) (\s@GetBotAliasResponse' {} a -> s {description = a} :: GetBotAliasResponse)

-- | The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
getBotAliasResponse_lastUpdatedDate :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
getBotAliasResponse_lastUpdatedDate = Lens.lens (\GetBotAliasResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetBotAliasResponse' {} a -> s {lastUpdatedDate = a} :: GetBotAliasResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the bot alias.
getBotAliasResponse_name :: Lens.Lens' GetBotAliasResponse (Prelude.Maybe Prelude.Text)
getBotAliasResponse_name = Lens.lens (\GetBotAliasResponse' {name} -> name) (\s@GetBotAliasResponse' {} a -> s {name = a} :: GetBotAliasResponse)

-- | The response's http status code.
getBotAliasResponse_httpStatus :: Lens.Lens' GetBotAliasResponse Prelude.Int
getBotAliasResponse_httpStatus = Lens.lens (\GetBotAliasResponse' {httpStatus} -> httpStatus) (\s@GetBotAliasResponse' {} a -> s {httpStatus = a} :: GetBotAliasResponse)

instance Prelude.NFData GetBotAliasResponse where
  rnf GetBotAliasResponse' {..} =
    Prelude.rnf botName
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf conversationLogs
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
