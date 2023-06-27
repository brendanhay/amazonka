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
-- Module      : Amazonka.LexV2Models.CreateBotAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an alias for the specified version of a bot. Use an alias to
-- enable you to change the version of a bot without updating applications
-- that use the bot.
--
-- For example, you can create an alias called \"PROD\" that your
-- applications use to call the Amazon Lex bot.
module Amazonka.LexV2Models.CreateBotAlias
  ( -- * Creating a Request
    CreateBotAlias (..),
    newCreateBotAlias,

    -- * Request Lenses
    createBotAlias_botAliasLocaleSettings,
    createBotAlias_botVersion,
    createBotAlias_conversationLogSettings,
    createBotAlias_description,
    createBotAlias_sentimentAnalysisSettings,
    createBotAlias_tags,
    createBotAlias_botAliasName,
    createBotAlias_botId,

    -- * Destructuring the Response
    CreateBotAliasResponse (..),
    newCreateBotAliasResponse,

    -- * Response Lenses
    createBotAliasResponse_botAliasId,
    createBotAliasResponse_botAliasLocaleSettings,
    createBotAliasResponse_botAliasName,
    createBotAliasResponse_botAliasStatus,
    createBotAliasResponse_botId,
    createBotAliasResponse_botVersion,
    createBotAliasResponse_conversationLogSettings,
    createBotAliasResponse_creationDateTime,
    createBotAliasResponse_description,
    createBotAliasResponse_sentimentAnalysisSettings,
    createBotAliasResponse_tags,
    createBotAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBotAlias' smart constructor.
data CreateBotAlias = CreateBotAlias'
  { -- | Maps configuration information to a specific locale. You can use this
    -- parameter to specify a specific Lambda function to run different
    -- functions in different locales.
    botAliasLocaleSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings),
    -- | The version of the bot that this alias points to. You can use the
    -- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_UpdateBotAlias.html UpdateBotAlias>
    -- operation to change the bot version associated with the alias.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether Amazon Lex logs text and audio for a conversation with
    -- the bot. When you enable conversation logs, text logs store text input,
    -- transcripts of audio input, and associated metadata in Amazon CloudWatch
    -- Logs. Audio logs store audio input in Amazon S3.
    conversationLogSettings :: Prelude.Maybe ConversationLogSettings,
    -- | A description of the alias. Use this description to help identify the
    -- alias.
    description :: Prelude.Maybe Prelude.Text,
    sentimentAnalysisSettings :: Prelude.Maybe SentimentAnalysisSettings,
    -- | A list of tags to add to the bot alias. You can only add tags when you
    -- create an alias, you can\'t use the @UpdateBotAlias@ operation to update
    -- the tags on a bot alias. To update tags, use the @TagResource@
    -- operation.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The alias to create. The name must be unique for the bot.
    botAliasName :: Prelude.Text,
    -- | The unique identifier of the bot that the alias applies to.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasLocaleSettings', 'createBotAlias_botAliasLocaleSettings' - Maps configuration information to a specific locale. You can use this
-- parameter to specify a specific Lambda function to run different
-- functions in different locales.
--
-- 'botVersion', 'createBotAlias_botVersion' - The version of the bot that this alias points to. You can use the
-- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_UpdateBotAlias.html UpdateBotAlias>
-- operation to change the bot version associated with the alias.
--
-- 'conversationLogSettings', 'createBotAlias_conversationLogSettings' - Specifies whether Amazon Lex logs text and audio for a conversation with
-- the bot. When you enable conversation logs, text logs store text input,
-- transcripts of audio input, and associated metadata in Amazon CloudWatch
-- Logs. Audio logs store audio input in Amazon S3.
--
-- 'description', 'createBotAlias_description' - A description of the alias. Use this description to help identify the
-- alias.
--
-- 'sentimentAnalysisSettings', 'createBotAlias_sentimentAnalysisSettings' - Undocumented member.
--
-- 'tags', 'createBotAlias_tags' - A list of tags to add to the bot alias. You can only add tags when you
-- create an alias, you can\'t use the @UpdateBotAlias@ operation to update
-- the tags on a bot alias. To update tags, use the @TagResource@
-- operation.
--
-- 'botAliasName', 'createBotAlias_botAliasName' - The alias to create. The name must be unique for the bot.
--
-- 'botId', 'createBotAlias_botId' - The unique identifier of the bot that the alias applies to.
newCreateBotAlias ::
  -- | 'botAliasName'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  CreateBotAlias
newCreateBotAlias pBotAliasName_ pBotId_ =
  CreateBotAlias'
    { botAliasLocaleSettings =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      conversationLogSettings = Prelude.Nothing,
      description = Prelude.Nothing,
      sentimentAnalysisSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      botAliasName = pBotAliasName_,
      botId = pBotId_
    }

-- | Maps configuration information to a specific locale. You can use this
-- parameter to specify a specific Lambda function to run different
-- functions in different locales.
createBotAlias_botAliasLocaleSettings :: Lens.Lens' CreateBotAlias (Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings))
createBotAlias_botAliasLocaleSettings = Lens.lens (\CreateBotAlias' {botAliasLocaleSettings} -> botAliasLocaleSettings) (\s@CreateBotAlias' {} a -> s {botAliasLocaleSettings = a} :: CreateBotAlias) Prelude.. Lens.mapping Lens.coerced

-- | The version of the bot that this alias points to. You can use the
-- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_UpdateBotAlias.html UpdateBotAlias>
-- operation to change the bot version associated with the alias.
createBotAlias_botVersion :: Lens.Lens' CreateBotAlias (Prelude.Maybe Prelude.Text)
createBotAlias_botVersion = Lens.lens (\CreateBotAlias' {botVersion} -> botVersion) (\s@CreateBotAlias' {} a -> s {botVersion = a} :: CreateBotAlias)

-- | Specifies whether Amazon Lex logs text and audio for a conversation with
-- the bot. When you enable conversation logs, text logs store text input,
-- transcripts of audio input, and associated metadata in Amazon CloudWatch
-- Logs. Audio logs store audio input in Amazon S3.
createBotAlias_conversationLogSettings :: Lens.Lens' CreateBotAlias (Prelude.Maybe ConversationLogSettings)
createBotAlias_conversationLogSettings = Lens.lens (\CreateBotAlias' {conversationLogSettings} -> conversationLogSettings) (\s@CreateBotAlias' {} a -> s {conversationLogSettings = a} :: CreateBotAlias)

-- | A description of the alias. Use this description to help identify the
-- alias.
createBotAlias_description :: Lens.Lens' CreateBotAlias (Prelude.Maybe Prelude.Text)
createBotAlias_description = Lens.lens (\CreateBotAlias' {description} -> description) (\s@CreateBotAlias' {} a -> s {description = a} :: CreateBotAlias)

-- | Undocumented member.
createBotAlias_sentimentAnalysisSettings :: Lens.Lens' CreateBotAlias (Prelude.Maybe SentimentAnalysisSettings)
createBotAlias_sentimentAnalysisSettings = Lens.lens (\CreateBotAlias' {sentimentAnalysisSettings} -> sentimentAnalysisSettings) (\s@CreateBotAlias' {} a -> s {sentimentAnalysisSettings = a} :: CreateBotAlias)

-- | A list of tags to add to the bot alias. You can only add tags when you
-- create an alias, you can\'t use the @UpdateBotAlias@ operation to update
-- the tags on a bot alias. To update tags, use the @TagResource@
-- operation.
createBotAlias_tags :: Lens.Lens' CreateBotAlias (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBotAlias_tags = Lens.lens (\CreateBotAlias' {tags} -> tags) (\s@CreateBotAlias' {} a -> s {tags = a} :: CreateBotAlias) Prelude.. Lens.mapping Lens.coerced

-- | The alias to create. The name must be unique for the bot.
createBotAlias_botAliasName :: Lens.Lens' CreateBotAlias Prelude.Text
createBotAlias_botAliasName = Lens.lens (\CreateBotAlias' {botAliasName} -> botAliasName) (\s@CreateBotAlias' {} a -> s {botAliasName = a} :: CreateBotAlias)

-- | The unique identifier of the bot that the alias applies to.
createBotAlias_botId :: Lens.Lens' CreateBotAlias Prelude.Text
createBotAlias_botId = Lens.lens (\CreateBotAlias' {botId} -> botId) (\s@CreateBotAlias' {} a -> s {botId = a} :: CreateBotAlias)

instance Core.AWSRequest CreateBotAlias where
  type
    AWSResponse CreateBotAlias =
      CreateBotAliasResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBotAliasResponse'
            Prelude.<$> (x Data..?> "botAliasId")
            Prelude.<*> ( x
                            Data..?> "botAliasLocaleSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "botAliasName")
            Prelude.<*> (x Data..?> "botAliasStatus")
            Prelude.<*> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "conversationLogSettings")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "sentimentAnalysisSettings")
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBotAlias where
  hashWithSalt _salt CreateBotAlias' {..} =
    _salt
      `Prelude.hashWithSalt` botAliasLocaleSettings
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` conversationLogSettings
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sentimentAnalysisSettings
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` botAliasName
      `Prelude.hashWithSalt` botId

instance Prelude.NFData CreateBotAlias where
  rnf CreateBotAlias' {..} =
    Prelude.rnf botAliasLocaleSettings
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf conversationLogSettings
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sentimentAnalysisSettings
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf botAliasName
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders CreateBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBotAlias where
  toJSON CreateBotAlias' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("botAliasLocaleSettings" Data..=)
              Prelude.<$> botAliasLocaleSettings,
            ("botVersion" Data..=) Prelude.<$> botVersion,
            ("conversationLogSettings" Data..=)
              Prelude.<$> conversationLogSettings,
            ("description" Data..=) Prelude.<$> description,
            ("sentimentAnalysisSettings" Data..=)
              Prelude.<$> sentimentAnalysisSettings,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("botAliasName" Data..= botAliasName)
          ]
      )

instance Data.ToPath CreateBotAlias where
  toPath CreateBotAlias' {..} =
    Prelude.mconcat
      ["/bots/", Data.toBS botId, "/botaliases/"]

instance Data.ToQuery CreateBotAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBotAliasResponse' smart constructor.
data CreateBotAliasResponse = CreateBotAliasResponse'
  { -- | The unique identifier of the bot alias.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | Configuration information for a specific locale.
    botAliasLocaleSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings),
    -- | The name specified for the bot alias.
    botAliasName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the alias. The alias is first put into the
    -- @Creating@ state. When the alias is ready to be used, it is put into the
    -- @Available@ state. You can use the @DescribeBotAlias@ operation to get
    -- the current state of an alias.
    botAliasStatus :: Prelude.Maybe BotAliasStatus,
    -- | The unique identifier of the bot that this alias applies to.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot associated with this alias.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The conversation log settings specified for the alias.
    conversationLogSettings :: Prelude.Maybe ConversationLogSettings,
    -- | A Unix timestamp indicating the date and time that the bot alias was
    -- created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description specified for the bot alias.
    description :: Prelude.Maybe Prelude.Text,
    sentimentAnalysisSettings :: Prelude.Maybe SentimentAnalysisSettings,
    -- | A list of tags associated with the bot alias.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasId', 'createBotAliasResponse_botAliasId' - The unique identifier of the bot alias.
--
-- 'botAliasLocaleSettings', 'createBotAliasResponse_botAliasLocaleSettings' - Configuration information for a specific locale.
--
-- 'botAliasName', 'createBotAliasResponse_botAliasName' - The name specified for the bot alias.
--
-- 'botAliasStatus', 'createBotAliasResponse_botAliasStatus' - The current status of the alias. The alias is first put into the
-- @Creating@ state. When the alias is ready to be used, it is put into the
-- @Available@ state. You can use the @DescribeBotAlias@ operation to get
-- the current state of an alias.
--
-- 'botId', 'createBotAliasResponse_botId' - The unique identifier of the bot that this alias applies to.
--
-- 'botVersion', 'createBotAliasResponse_botVersion' - The version of the bot associated with this alias.
--
-- 'conversationLogSettings', 'createBotAliasResponse_conversationLogSettings' - The conversation log settings specified for the alias.
--
-- 'creationDateTime', 'createBotAliasResponse_creationDateTime' - A Unix timestamp indicating the date and time that the bot alias was
-- created.
--
-- 'description', 'createBotAliasResponse_description' - The description specified for the bot alias.
--
-- 'sentimentAnalysisSettings', 'createBotAliasResponse_sentimentAnalysisSettings' - Undocumented member.
--
-- 'tags', 'createBotAliasResponse_tags' - A list of tags associated with the bot alias.
--
-- 'httpStatus', 'createBotAliasResponse_httpStatus' - The response's http status code.
newCreateBotAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBotAliasResponse
newCreateBotAliasResponse pHttpStatus_ =
  CreateBotAliasResponse'
    { botAliasId =
        Prelude.Nothing,
      botAliasLocaleSettings = Prelude.Nothing,
      botAliasName = Prelude.Nothing,
      botAliasStatus = Prelude.Nothing,
      botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      conversationLogSettings = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      sentimentAnalysisSettings = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the bot alias.
createBotAliasResponse_botAliasId :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe Prelude.Text)
createBotAliasResponse_botAliasId = Lens.lens (\CreateBotAliasResponse' {botAliasId} -> botAliasId) (\s@CreateBotAliasResponse' {} a -> s {botAliasId = a} :: CreateBotAliasResponse)

-- | Configuration information for a specific locale.
createBotAliasResponse_botAliasLocaleSettings :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings))
createBotAliasResponse_botAliasLocaleSettings = Lens.lens (\CreateBotAliasResponse' {botAliasLocaleSettings} -> botAliasLocaleSettings) (\s@CreateBotAliasResponse' {} a -> s {botAliasLocaleSettings = a} :: CreateBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name specified for the bot alias.
createBotAliasResponse_botAliasName :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe Prelude.Text)
createBotAliasResponse_botAliasName = Lens.lens (\CreateBotAliasResponse' {botAliasName} -> botAliasName) (\s@CreateBotAliasResponse' {} a -> s {botAliasName = a} :: CreateBotAliasResponse)

-- | The current status of the alias. The alias is first put into the
-- @Creating@ state. When the alias is ready to be used, it is put into the
-- @Available@ state. You can use the @DescribeBotAlias@ operation to get
-- the current state of an alias.
createBotAliasResponse_botAliasStatus :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe BotAliasStatus)
createBotAliasResponse_botAliasStatus = Lens.lens (\CreateBotAliasResponse' {botAliasStatus} -> botAliasStatus) (\s@CreateBotAliasResponse' {} a -> s {botAliasStatus = a} :: CreateBotAliasResponse)

-- | The unique identifier of the bot that this alias applies to.
createBotAliasResponse_botId :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe Prelude.Text)
createBotAliasResponse_botId = Lens.lens (\CreateBotAliasResponse' {botId} -> botId) (\s@CreateBotAliasResponse' {} a -> s {botId = a} :: CreateBotAliasResponse)

-- | The version of the bot associated with this alias.
createBotAliasResponse_botVersion :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe Prelude.Text)
createBotAliasResponse_botVersion = Lens.lens (\CreateBotAliasResponse' {botVersion} -> botVersion) (\s@CreateBotAliasResponse' {} a -> s {botVersion = a} :: CreateBotAliasResponse)

-- | The conversation log settings specified for the alias.
createBotAliasResponse_conversationLogSettings :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe ConversationLogSettings)
createBotAliasResponse_conversationLogSettings = Lens.lens (\CreateBotAliasResponse' {conversationLogSettings} -> conversationLogSettings) (\s@CreateBotAliasResponse' {} a -> s {conversationLogSettings = a} :: CreateBotAliasResponse)

-- | A Unix timestamp indicating the date and time that the bot alias was
-- created.
createBotAliasResponse_creationDateTime :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
createBotAliasResponse_creationDateTime = Lens.lens (\CreateBotAliasResponse' {creationDateTime} -> creationDateTime) (\s@CreateBotAliasResponse' {} a -> s {creationDateTime = a} :: CreateBotAliasResponse) Prelude.. Lens.mapping Data._Time

-- | The description specified for the bot alias.
createBotAliasResponse_description :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe Prelude.Text)
createBotAliasResponse_description = Lens.lens (\CreateBotAliasResponse' {description} -> description) (\s@CreateBotAliasResponse' {} a -> s {description = a} :: CreateBotAliasResponse)

-- | Undocumented member.
createBotAliasResponse_sentimentAnalysisSettings :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe SentimentAnalysisSettings)
createBotAliasResponse_sentimentAnalysisSettings = Lens.lens (\CreateBotAliasResponse' {sentimentAnalysisSettings} -> sentimentAnalysisSettings) (\s@CreateBotAliasResponse' {} a -> s {sentimentAnalysisSettings = a} :: CreateBotAliasResponse)

-- | A list of tags associated with the bot alias.
createBotAliasResponse_tags :: Lens.Lens' CreateBotAliasResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createBotAliasResponse_tags = Lens.lens (\CreateBotAliasResponse' {tags} -> tags) (\s@CreateBotAliasResponse' {} a -> s {tags = a} :: CreateBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createBotAliasResponse_httpStatus :: Lens.Lens' CreateBotAliasResponse Prelude.Int
createBotAliasResponse_httpStatus = Lens.lens (\CreateBotAliasResponse' {httpStatus} -> httpStatus) (\s@CreateBotAliasResponse' {} a -> s {httpStatus = a} :: CreateBotAliasResponse)

instance Prelude.NFData CreateBotAliasResponse where
  rnf CreateBotAliasResponse' {..} =
    Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf botAliasLocaleSettings
      `Prelude.seq` Prelude.rnf botAliasName
      `Prelude.seq` Prelude.rnf botAliasStatus
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf conversationLogSettings
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sentimentAnalysisSettings
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
