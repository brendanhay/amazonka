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
-- Module      : Network.AWS.LexModels.PutBotAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for the specified version of the bot or replaces an
-- alias for the specified bot. To change the version of the bot that the
-- alias points to, replace the alias. For more information about aliases,
-- see versioning-aliases.
--
-- This operation requires permissions for the @lex:PutBotAlias@ action.
module Network.AWS.LexModels.PutBotAlias
  ( -- * Creating a Request
    PutBotAlias (..),
    newPutBotAlias,

    -- * Request Lenses
    putBotAlias_tags,
    putBotAlias_description,
    putBotAlias_checksum,
    putBotAlias_conversationLogs,
    putBotAlias_name,
    putBotAlias_botVersion,
    putBotAlias_botName,

    -- * Destructuring the Response
    PutBotAliasResponse (..),
    newPutBotAliasResponse,

    -- * Response Lenses
    putBotAliasResponse_createdDate,
    putBotAliasResponse_botName,
    putBotAliasResponse_lastUpdatedDate,
    putBotAliasResponse_botVersion,
    putBotAliasResponse_name,
    putBotAliasResponse_tags,
    putBotAliasResponse_description,
    putBotAliasResponse_checksum,
    putBotAliasResponse_conversationLogs,
    putBotAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutBotAlias' smart constructor.
data PutBotAlias = PutBotAlias'
  { -- | A list of tags to add to the bot alias. You can only add tags when you
    -- create an alias, you can\'t use the @PutBotAlias@ operation to update
    -- the tags on a bot alias. To update tags, use the @TagResource@
    -- operation.
    tags :: Core.Maybe [Tag],
    -- | A description of the alias.
    description :: Core.Maybe Core.Text,
    -- | Identifies a specific revision of the @$LATEST@ version.
    --
    -- When you create a new bot alias, leave the @checksum@ field blank. If
    -- you specify a checksum you get a @BadRequestException@ exception.
    --
    -- When you want to update a bot alias, set the @checksum@ field to the
    -- checksum of the most recent revision of the @$LATEST@ version. If you
    -- don\'t specify the @ checksum@ field, or if the checksum does not match
    -- the @$LATEST@ version, you get a @PreconditionFailedException@
    -- exception.
    checksum :: Core.Maybe Core.Text,
    -- | Settings for conversation logs for the alias.
    conversationLogs :: Core.Maybe ConversationLogsRequest,
    -- | The name of the alias. The name is /not/ case sensitive.
    name :: Core.Text,
    -- | The version of the bot.
    botVersion :: Core.Text,
    -- | The name of the bot.
    botName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutBotAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putBotAlias_tags' - A list of tags to add to the bot alias. You can only add tags when you
-- create an alias, you can\'t use the @PutBotAlias@ operation to update
-- the tags on a bot alias. To update tags, use the @TagResource@
-- operation.
--
-- 'description', 'putBotAlias_description' - A description of the alias.
--
-- 'checksum', 'putBotAlias_checksum' - Identifies a specific revision of the @$LATEST@ version.
--
-- When you create a new bot alias, leave the @checksum@ field blank. If
-- you specify a checksum you get a @BadRequestException@ exception.
--
-- When you want to update a bot alias, set the @checksum@ field to the
-- checksum of the most recent revision of the @$LATEST@ version. If you
-- don\'t specify the @ checksum@ field, or if the checksum does not match
-- the @$LATEST@ version, you get a @PreconditionFailedException@
-- exception.
--
-- 'conversationLogs', 'putBotAlias_conversationLogs' - Settings for conversation logs for the alias.
--
-- 'name', 'putBotAlias_name' - The name of the alias. The name is /not/ case sensitive.
--
-- 'botVersion', 'putBotAlias_botVersion' - The version of the bot.
--
-- 'botName', 'putBotAlias_botName' - The name of the bot.
newPutBotAlias ::
  -- | 'name'
  Core.Text ->
  -- | 'botVersion'
  Core.Text ->
  -- | 'botName'
  Core.Text ->
  PutBotAlias
newPutBotAlias pName_ pBotVersion_ pBotName_ =
  PutBotAlias'
    { tags = Core.Nothing,
      description = Core.Nothing,
      checksum = Core.Nothing,
      conversationLogs = Core.Nothing,
      name = pName_,
      botVersion = pBotVersion_,
      botName = pBotName_
    }

-- | A list of tags to add to the bot alias. You can only add tags when you
-- create an alias, you can\'t use the @PutBotAlias@ operation to update
-- the tags on a bot alias. To update tags, use the @TagResource@
-- operation.
putBotAlias_tags :: Lens.Lens' PutBotAlias (Core.Maybe [Tag])
putBotAlias_tags = Lens.lens (\PutBotAlias' {tags} -> tags) (\s@PutBotAlias' {} a -> s {tags = a} :: PutBotAlias) Core.. Lens.mapping Lens._Coerce

-- | A description of the alias.
putBotAlias_description :: Lens.Lens' PutBotAlias (Core.Maybe Core.Text)
putBotAlias_description = Lens.lens (\PutBotAlias' {description} -> description) (\s@PutBotAlias' {} a -> s {description = a} :: PutBotAlias)

-- | Identifies a specific revision of the @$LATEST@ version.
--
-- When you create a new bot alias, leave the @checksum@ field blank. If
-- you specify a checksum you get a @BadRequestException@ exception.
--
-- When you want to update a bot alias, set the @checksum@ field to the
-- checksum of the most recent revision of the @$LATEST@ version. If you
-- don\'t specify the @ checksum@ field, or if the checksum does not match
-- the @$LATEST@ version, you get a @PreconditionFailedException@
-- exception.
putBotAlias_checksum :: Lens.Lens' PutBotAlias (Core.Maybe Core.Text)
putBotAlias_checksum = Lens.lens (\PutBotAlias' {checksum} -> checksum) (\s@PutBotAlias' {} a -> s {checksum = a} :: PutBotAlias)

-- | Settings for conversation logs for the alias.
putBotAlias_conversationLogs :: Lens.Lens' PutBotAlias (Core.Maybe ConversationLogsRequest)
putBotAlias_conversationLogs = Lens.lens (\PutBotAlias' {conversationLogs} -> conversationLogs) (\s@PutBotAlias' {} a -> s {conversationLogs = a} :: PutBotAlias)

-- | The name of the alias. The name is /not/ case sensitive.
putBotAlias_name :: Lens.Lens' PutBotAlias Core.Text
putBotAlias_name = Lens.lens (\PutBotAlias' {name} -> name) (\s@PutBotAlias' {} a -> s {name = a} :: PutBotAlias)

-- | The version of the bot.
putBotAlias_botVersion :: Lens.Lens' PutBotAlias Core.Text
putBotAlias_botVersion = Lens.lens (\PutBotAlias' {botVersion} -> botVersion) (\s@PutBotAlias' {} a -> s {botVersion = a} :: PutBotAlias)

-- | The name of the bot.
putBotAlias_botName :: Lens.Lens' PutBotAlias Core.Text
putBotAlias_botName = Lens.lens (\PutBotAlias' {botName} -> botName) (\s@PutBotAlias' {} a -> s {botName = a} :: PutBotAlias)

instance Core.AWSRequest PutBotAlias where
  type AWSResponse PutBotAlias = PutBotAliasResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutBotAliasResponse'
            Core.<$> (x Core..?> "createdDate")
            Core.<*> (x Core..?> "botName")
            Core.<*> (x Core..?> "lastUpdatedDate")
            Core.<*> (x Core..?> "botVersion")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "checksum")
            Core.<*> (x Core..?> "conversationLogs")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutBotAlias

instance Core.NFData PutBotAlias

instance Core.ToHeaders PutBotAlias where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutBotAlias where
  toJSON PutBotAlias' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            ("checksum" Core..=) Core.<$> checksum,
            ("conversationLogs" Core..=)
              Core.<$> conversationLogs,
            Core.Just ("botVersion" Core..= botVersion)
          ]
      )

instance Core.ToPath PutBotAlias where
  toPath PutBotAlias' {..} =
    Core.mconcat
      [ "/bots/",
        Core.toBS botName,
        "/aliases/",
        Core.toBS name
      ]

instance Core.ToQuery PutBotAlias where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutBotAliasResponse' smart constructor.
data PutBotAliasResponse = PutBotAliasResponse'
  { -- | The date that the bot alias was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The name of the bot that the alias points to.
    botName :: Core.Maybe Core.Text,
    -- | The date that the bot alias was updated. When you create a resource, the
    -- creation date and the last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | The version of the bot that the alias points to.
    botVersion :: Core.Maybe Core.Text,
    -- | The name of the alias.
    name :: Core.Maybe Core.Text,
    -- | A list of tags associated with a bot.
    tags :: Core.Maybe [Tag],
    -- | A description of the alias.
    description :: Core.Maybe Core.Text,
    -- | The checksum for the current version of the alias.
    checksum :: Core.Maybe Core.Text,
    -- | The settings that determine how Amazon Lex uses conversation logs for
    -- the alias.
    conversationLogs :: Core.Maybe ConversationLogsResponse,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'putBotAliasResponse_createdDate' - The date that the bot alias was created.
--
-- 'botName', 'putBotAliasResponse_botName' - The name of the bot that the alias points to.
--
-- 'lastUpdatedDate', 'putBotAliasResponse_lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
--
-- 'botVersion', 'putBotAliasResponse_botVersion' - The version of the bot that the alias points to.
--
-- 'name', 'putBotAliasResponse_name' - The name of the alias.
--
-- 'tags', 'putBotAliasResponse_tags' - A list of tags associated with a bot.
--
-- 'description', 'putBotAliasResponse_description' - A description of the alias.
--
-- 'checksum', 'putBotAliasResponse_checksum' - The checksum for the current version of the alias.
--
-- 'conversationLogs', 'putBotAliasResponse_conversationLogs' - The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
--
-- 'httpStatus', 'putBotAliasResponse_httpStatus' - The response's http status code.
newPutBotAliasResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutBotAliasResponse
newPutBotAliasResponse pHttpStatus_ =
  PutBotAliasResponse'
    { createdDate = Core.Nothing,
      botName = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      botVersion = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      checksum = Core.Nothing,
      conversationLogs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date that the bot alias was created.
putBotAliasResponse_createdDate :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.UTCTime)
putBotAliasResponse_createdDate = Lens.lens (\PutBotAliasResponse' {createdDate} -> createdDate) (\s@PutBotAliasResponse' {} a -> s {createdDate = a} :: PutBotAliasResponse) Core.. Lens.mapping Core._Time

-- | The name of the bot that the alias points to.
putBotAliasResponse_botName :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.Text)
putBotAliasResponse_botName = Lens.lens (\PutBotAliasResponse' {botName} -> botName) (\s@PutBotAliasResponse' {} a -> s {botName = a} :: PutBotAliasResponse)

-- | The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
putBotAliasResponse_lastUpdatedDate :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.UTCTime)
putBotAliasResponse_lastUpdatedDate = Lens.lens (\PutBotAliasResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@PutBotAliasResponse' {} a -> s {lastUpdatedDate = a} :: PutBotAliasResponse) Core.. Lens.mapping Core._Time

-- | The version of the bot that the alias points to.
putBotAliasResponse_botVersion :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.Text)
putBotAliasResponse_botVersion = Lens.lens (\PutBotAliasResponse' {botVersion} -> botVersion) (\s@PutBotAliasResponse' {} a -> s {botVersion = a} :: PutBotAliasResponse)

-- | The name of the alias.
putBotAliasResponse_name :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.Text)
putBotAliasResponse_name = Lens.lens (\PutBotAliasResponse' {name} -> name) (\s@PutBotAliasResponse' {} a -> s {name = a} :: PutBotAliasResponse)

-- | A list of tags associated with a bot.
putBotAliasResponse_tags :: Lens.Lens' PutBotAliasResponse (Core.Maybe [Tag])
putBotAliasResponse_tags = Lens.lens (\PutBotAliasResponse' {tags} -> tags) (\s@PutBotAliasResponse' {} a -> s {tags = a} :: PutBotAliasResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the alias.
putBotAliasResponse_description :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.Text)
putBotAliasResponse_description = Lens.lens (\PutBotAliasResponse' {description} -> description) (\s@PutBotAliasResponse' {} a -> s {description = a} :: PutBotAliasResponse)

-- | The checksum for the current version of the alias.
putBotAliasResponse_checksum :: Lens.Lens' PutBotAliasResponse (Core.Maybe Core.Text)
putBotAliasResponse_checksum = Lens.lens (\PutBotAliasResponse' {checksum} -> checksum) (\s@PutBotAliasResponse' {} a -> s {checksum = a} :: PutBotAliasResponse)

-- | The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
putBotAliasResponse_conversationLogs :: Lens.Lens' PutBotAliasResponse (Core.Maybe ConversationLogsResponse)
putBotAliasResponse_conversationLogs = Lens.lens (\PutBotAliasResponse' {conversationLogs} -> conversationLogs) (\s@PutBotAliasResponse' {} a -> s {conversationLogs = a} :: PutBotAliasResponse)

-- | The response's http status code.
putBotAliasResponse_httpStatus :: Lens.Lens' PutBotAliasResponse Core.Int
putBotAliasResponse_httpStatus = Lens.lens (\PutBotAliasResponse' {httpStatus} -> httpStatus) (\s@PutBotAliasResponse' {} a -> s {httpStatus = a} :: PutBotAliasResponse)

instance Core.NFData PutBotAliasResponse
