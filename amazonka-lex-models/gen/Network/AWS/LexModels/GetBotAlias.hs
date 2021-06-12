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
-- Module      : Network.AWS.LexModels.GetBotAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an Amazon Lex bot alias. For more information
-- about aliases, see versioning-aliases.
--
-- This operation requires permissions for the @lex:GetBotAlias@ action.
module Network.AWS.LexModels.GetBotAlias
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
    getBotAliasResponse_createdDate,
    getBotAliasResponse_botName,
    getBotAliasResponse_lastUpdatedDate,
    getBotAliasResponse_botVersion,
    getBotAliasResponse_name,
    getBotAliasResponse_description,
    getBotAliasResponse_checksum,
    getBotAliasResponse_conversationLogs,
    getBotAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetBotAlias' smart constructor.
data GetBotAlias = GetBotAlias'
  { -- | The name of the bot alias. The name is case sensitive.
    name :: Core.Text,
    -- | The name of the bot.
    botName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'botName'
  Core.Text ->
  GetBotAlias
newGetBotAlias pName_ pBotName_ =
  GetBotAlias' {name = pName_, botName = pBotName_}

-- | The name of the bot alias. The name is case sensitive.
getBotAlias_name :: Lens.Lens' GetBotAlias Core.Text
getBotAlias_name = Lens.lens (\GetBotAlias' {name} -> name) (\s@GetBotAlias' {} a -> s {name = a} :: GetBotAlias)

-- | The name of the bot.
getBotAlias_botName :: Lens.Lens' GetBotAlias Core.Text
getBotAlias_botName = Lens.lens (\GetBotAlias' {botName} -> botName) (\s@GetBotAlias' {} a -> s {botName = a} :: GetBotAlias)

instance Core.AWSRequest GetBotAlias where
  type AWSResponse GetBotAlias = GetBotAliasResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBotAliasResponse'
            Core.<$> (x Core..?> "createdDate")
            Core.<*> (x Core..?> "botName")
            Core.<*> (x Core..?> "lastUpdatedDate")
            Core.<*> (x Core..?> "botVersion")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "checksum")
            Core.<*> (x Core..?> "conversationLogs")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBotAlias

instance Core.NFData GetBotAlias

instance Core.ToHeaders GetBotAlias where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetBotAlias where
  toPath GetBotAlias' {..} =
    Core.mconcat
      [ "/bots/",
        Core.toBS botName,
        "/aliases/",
        Core.toBS name
      ]

instance Core.ToQuery GetBotAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetBotAliasResponse' smart constructor.
data GetBotAliasResponse = GetBotAliasResponse'
  { -- | The date that the bot alias was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The name of the bot that the alias points to.
    botName :: Core.Maybe Core.Text,
    -- | The date that the bot alias was updated. When you create a resource, the
    -- creation date and the last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | The version of the bot that the alias points to.
    botVersion :: Core.Maybe Core.Text,
    -- | The name of the bot alias.
    name :: Core.Maybe Core.Text,
    -- | A description of the bot alias.
    description :: Core.Maybe Core.Text,
    -- | Checksum of the bot alias.
    checksum :: Core.Maybe Core.Text,
    -- | The settings that determine how Amazon Lex uses conversation logs for
    -- the alias.
    conversationLogs :: Core.Maybe ConversationLogsResponse,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'getBotAliasResponse_createdDate' - The date that the bot alias was created.
--
-- 'botName', 'getBotAliasResponse_botName' - The name of the bot that the alias points to.
--
-- 'lastUpdatedDate', 'getBotAliasResponse_lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
--
-- 'botVersion', 'getBotAliasResponse_botVersion' - The version of the bot that the alias points to.
--
-- 'name', 'getBotAliasResponse_name' - The name of the bot alias.
--
-- 'description', 'getBotAliasResponse_description' - A description of the bot alias.
--
-- 'checksum', 'getBotAliasResponse_checksum' - Checksum of the bot alias.
--
-- 'conversationLogs', 'getBotAliasResponse_conversationLogs' - The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
--
-- 'httpStatus', 'getBotAliasResponse_httpStatus' - The response's http status code.
newGetBotAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBotAliasResponse
newGetBotAliasResponse pHttpStatus_ =
  GetBotAliasResponse'
    { createdDate = Core.Nothing,
      botName = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      botVersion = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      checksum = Core.Nothing,
      conversationLogs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date that the bot alias was created.
getBotAliasResponse_createdDate :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.UTCTime)
getBotAliasResponse_createdDate = Lens.lens (\GetBotAliasResponse' {createdDate} -> createdDate) (\s@GetBotAliasResponse' {} a -> s {createdDate = a} :: GetBotAliasResponse) Core.. Lens.mapping Core._Time

-- | The name of the bot that the alias points to.
getBotAliasResponse_botName :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.Text)
getBotAliasResponse_botName = Lens.lens (\GetBotAliasResponse' {botName} -> botName) (\s@GetBotAliasResponse' {} a -> s {botName = a} :: GetBotAliasResponse)

-- | The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
getBotAliasResponse_lastUpdatedDate :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.UTCTime)
getBotAliasResponse_lastUpdatedDate = Lens.lens (\GetBotAliasResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@GetBotAliasResponse' {} a -> s {lastUpdatedDate = a} :: GetBotAliasResponse) Core.. Lens.mapping Core._Time

-- | The version of the bot that the alias points to.
getBotAliasResponse_botVersion :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.Text)
getBotAliasResponse_botVersion = Lens.lens (\GetBotAliasResponse' {botVersion} -> botVersion) (\s@GetBotAliasResponse' {} a -> s {botVersion = a} :: GetBotAliasResponse)

-- | The name of the bot alias.
getBotAliasResponse_name :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.Text)
getBotAliasResponse_name = Lens.lens (\GetBotAliasResponse' {name} -> name) (\s@GetBotAliasResponse' {} a -> s {name = a} :: GetBotAliasResponse)

-- | A description of the bot alias.
getBotAliasResponse_description :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.Text)
getBotAliasResponse_description = Lens.lens (\GetBotAliasResponse' {description} -> description) (\s@GetBotAliasResponse' {} a -> s {description = a} :: GetBotAliasResponse)

-- | Checksum of the bot alias.
getBotAliasResponse_checksum :: Lens.Lens' GetBotAliasResponse (Core.Maybe Core.Text)
getBotAliasResponse_checksum = Lens.lens (\GetBotAliasResponse' {checksum} -> checksum) (\s@GetBotAliasResponse' {} a -> s {checksum = a} :: GetBotAliasResponse)

-- | The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
getBotAliasResponse_conversationLogs :: Lens.Lens' GetBotAliasResponse (Core.Maybe ConversationLogsResponse)
getBotAliasResponse_conversationLogs = Lens.lens (\GetBotAliasResponse' {conversationLogs} -> conversationLogs) (\s@GetBotAliasResponse' {} a -> s {conversationLogs = a} :: GetBotAliasResponse)

-- | The response's http status code.
getBotAliasResponse_httpStatus :: Lens.Lens' GetBotAliasResponse Core.Int
getBotAliasResponse_httpStatus = Lens.lens (\GetBotAliasResponse' {httpStatus} -> httpStatus) (\s@GetBotAliasResponse' {} a -> s {httpStatus = a} :: GetBotAliasResponse)

instance Core.NFData GetBotAliasResponse
