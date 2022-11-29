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
-- Module      : Amazonka.LexModels.PutBotAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.LexModels.PutBotAlias
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
    putBotAliasResponse_tags,
    putBotAliasResponse_name,
    putBotAliasResponse_botVersion,
    putBotAliasResponse_lastUpdatedDate,
    putBotAliasResponse_description,
    putBotAliasResponse_checksum,
    putBotAliasResponse_botName,
    putBotAliasResponse_conversationLogs,
    putBotAliasResponse_createdDate,
    putBotAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutBotAlias' smart constructor.
data PutBotAlias = PutBotAlias'
  { -- | A list of tags to add to the bot alias. You can only add tags when you
    -- create an alias, you can\'t use the @PutBotAlias@ operation to update
    -- the tags on a bot alias. To update tags, use the @TagResource@
    -- operation.
    tags :: Prelude.Maybe [Tag],
    -- | A description of the alias.
    description :: Prelude.Maybe Prelude.Text,
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
    checksum :: Prelude.Maybe Prelude.Text,
    -- | Settings for conversation logs for the alias.
    conversationLogs :: Prelude.Maybe ConversationLogsRequest,
    -- | The name of the alias. The name is /not/ case sensitive.
    name :: Prelude.Text,
    -- | The version of the bot.
    botVersion :: Prelude.Text,
    -- | The name of the bot.
    botName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'botName'
  Prelude.Text ->
  PutBotAlias
newPutBotAlias pName_ pBotVersion_ pBotName_ =
  PutBotAlias'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      checksum = Prelude.Nothing,
      conversationLogs = Prelude.Nothing,
      name = pName_,
      botVersion = pBotVersion_,
      botName = pBotName_
    }

-- | A list of tags to add to the bot alias. You can only add tags when you
-- create an alias, you can\'t use the @PutBotAlias@ operation to update
-- the tags on a bot alias. To update tags, use the @TagResource@
-- operation.
putBotAlias_tags :: Lens.Lens' PutBotAlias (Prelude.Maybe [Tag])
putBotAlias_tags = Lens.lens (\PutBotAlias' {tags} -> tags) (\s@PutBotAlias' {} a -> s {tags = a} :: PutBotAlias) Prelude.. Lens.mapping Lens.coerced

-- | A description of the alias.
putBotAlias_description :: Lens.Lens' PutBotAlias (Prelude.Maybe Prelude.Text)
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
putBotAlias_checksum :: Lens.Lens' PutBotAlias (Prelude.Maybe Prelude.Text)
putBotAlias_checksum = Lens.lens (\PutBotAlias' {checksum} -> checksum) (\s@PutBotAlias' {} a -> s {checksum = a} :: PutBotAlias)

-- | Settings for conversation logs for the alias.
putBotAlias_conversationLogs :: Lens.Lens' PutBotAlias (Prelude.Maybe ConversationLogsRequest)
putBotAlias_conversationLogs = Lens.lens (\PutBotAlias' {conversationLogs} -> conversationLogs) (\s@PutBotAlias' {} a -> s {conversationLogs = a} :: PutBotAlias)

-- | The name of the alias. The name is /not/ case sensitive.
putBotAlias_name :: Lens.Lens' PutBotAlias Prelude.Text
putBotAlias_name = Lens.lens (\PutBotAlias' {name} -> name) (\s@PutBotAlias' {} a -> s {name = a} :: PutBotAlias)

-- | The version of the bot.
putBotAlias_botVersion :: Lens.Lens' PutBotAlias Prelude.Text
putBotAlias_botVersion = Lens.lens (\PutBotAlias' {botVersion} -> botVersion) (\s@PutBotAlias' {} a -> s {botVersion = a} :: PutBotAlias)

-- | The name of the bot.
putBotAlias_botName :: Lens.Lens' PutBotAlias Prelude.Text
putBotAlias_botName = Lens.lens (\PutBotAlias' {botName} -> botName) (\s@PutBotAlias' {} a -> s {botName = a} :: PutBotAlias)

instance Core.AWSRequest PutBotAlias where
  type AWSResponse PutBotAlias = PutBotAliasResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutBotAliasResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "lastUpdatedDate")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "checksum")
            Prelude.<*> (x Core..?> "botName")
            Prelude.<*> (x Core..?> "conversationLogs")
            Prelude.<*> (x Core..?> "createdDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutBotAlias where
  hashWithSalt _salt PutBotAlias' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` checksum
      `Prelude.hashWithSalt` conversationLogs
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` botName

instance Prelude.NFData PutBotAlias where
  rnf PutBotAlias' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf conversationLogs
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf botName

instance Core.ToHeaders PutBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutBotAlias where
  toJSON PutBotAlias' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("description" Core..=) Prelude.<$> description,
            ("checksum" Core..=) Prelude.<$> checksum,
            ("conversationLogs" Core..=)
              Prelude.<$> conversationLogs,
            Prelude.Just ("botVersion" Core..= botVersion)
          ]
      )

instance Core.ToPath PutBotAlias where
  toPath PutBotAlias' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botName,
        "/aliases/",
        Core.toBS name
      ]

instance Core.ToQuery PutBotAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutBotAliasResponse' smart constructor.
data PutBotAliasResponse = PutBotAliasResponse'
  { -- | A list of tags associated with a bot.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the alias.
    name :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot that the alias points to.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The date that the bot alias was updated. When you create a resource, the
    -- creation date and the last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | A description of the alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | The checksum for the current version of the alias.
    checksum :: Prelude.Maybe Prelude.Text,
    -- | The name of the bot that the alias points to.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The settings that determine how Amazon Lex uses conversation logs for
    -- the alias.
    conversationLogs :: Prelude.Maybe ConversationLogsResponse,
    -- | The date that the bot alias was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putBotAliasResponse_tags' - A list of tags associated with a bot.
--
-- 'name', 'putBotAliasResponse_name' - The name of the alias.
--
-- 'botVersion', 'putBotAliasResponse_botVersion' - The version of the bot that the alias points to.
--
-- 'lastUpdatedDate', 'putBotAliasResponse_lastUpdatedDate' - The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
--
-- 'description', 'putBotAliasResponse_description' - A description of the alias.
--
-- 'checksum', 'putBotAliasResponse_checksum' - The checksum for the current version of the alias.
--
-- 'botName', 'putBotAliasResponse_botName' - The name of the bot that the alias points to.
--
-- 'conversationLogs', 'putBotAliasResponse_conversationLogs' - The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
--
-- 'createdDate', 'putBotAliasResponse_createdDate' - The date that the bot alias was created.
--
-- 'httpStatus', 'putBotAliasResponse_httpStatus' - The response's http status code.
newPutBotAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutBotAliasResponse
newPutBotAliasResponse pHttpStatus_ =
  PutBotAliasResponse'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      checksum = Prelude.Nothing,
      botName = Prelude.Nothing,
      conversationLogs = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of tags associated with a bot.
putBotAliasResponse_tags :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe [Tag])
putBotAliasResponse_tags = Lens.lens (\PutBotAliasResponse' {tags} -> tags) (\s@PutBotAliasResponse' {} a -> s {tags = a} :: PutBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the alias.
putBotAliasResponse_name :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe Prelude.Text)
putBotAliasResponse_name = Lens.lens (\PutBotAliasResponse' {name} -> name) (\s@PutBotAliasResponse' {} a -> s {name = a} :: PutBotAliasResponse)

-- | The version of the bot that the alias points to.
putBotAliasResponse_botVersion :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe Prelude.Text)
putBotAliasResponse_botVersion = Lens.lens (\PutBotAliasResponse' {botVersion} -> botVersion) (\s@PutBotAliasResponse' {} a -> s {botVersion = a} :: PutBotAliasResponse)

-- | The date that the bot alias was updated. When you create a resource, the
-- creation date and the last updated date are the same.
putBotAliasResponse_lastUpdatedDate :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
putBotAliasResponse_lastUpdatedDate = Lens.lens (\PutBotAliasResponse' {lastUpdatedDate} -> lastUpdatedDate) (\s@PutBotAliasResponse' {} a -> s {lastUpdatedDate = a} :: PutBotAliasResponse) Prelude.. Lens.mapping Core._Time

-- | A description of the alias.
putBotAliasResponse_description :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe Prelude.Text)
putBotAliasResponse_description = Lens.lens (\PutBotAliasResponse' {description} -> description) (\s@PutBotAliasResponse' {} a -> s {description = a} :: PutBotAliasResponse)

-- | The checksum for the current version of the alias.
putBotAliasResponse_checksum :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe Prelude.Text)
putBotAliasResponse_checksum = Lens.lens (\PutBotAliasResponse' {checksum} -> checksum) (\s@PutBotAliasResponse' {} a -> s {checksum = a} :: PutBotAliasResponse)

-- | The name of the bot that the alias points to.
putBotAliasResponse_botName :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe Prelude.Text)
putBotAliasResponse_botName = Lens.lens (\PutBotAliasResponse' {botName} -> botName) (\s@PutBotAliasResponse' {} a -> s {botName = a} :: PutBotAliasResponse)

-- | The settings that determine how Amazon Lex uses conversation logs for
-- the alias.
putBotAliasResponse_conversationLogs :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe ConversationLogsResponse)
putBotAliasResponse_conversationLogs = Lens.lens (\PutBotAliasResponse' {conversationLogs} -> conversationLogs) (\s@PutBotAliasResponse' {} a -> s {conversationLogs = a} :: PutBotAliasResponse)

-- | The date that the bot alias was created.
putBotAliasResponse_createdDate :: Lens.Lens' PutBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
putBotAliasResponse_createdDate = Lens.lens (\PutBotAliasResponse' {createdDate} -> createdDate) (\s@PutBotAliasResponse' {} a -> s {createdDate = a} :: PutBotAliasResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
putBotAliasResponse_httpStatus :: Lens.Lens' PutBotAliasResponse Prelude.Int
putBotAliasResponse_httpStatus = Lens.lens (\PutBotAliasResponse' {httpStatus} -> httpStatus) (\s@PutBotAliasResponse' {} a -> s {httpStatus = a} :: PutBotAliasResponse)

instance Prelude.NFData PutBotAliasResponse where
  rnf PutBotAliasResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf checksum
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf conversationLogs
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf httpStatus
