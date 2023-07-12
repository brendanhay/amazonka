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
-- Module      : Amazonka.LexV2Models.DescribeBotAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a specific bot alias.
module Amazonka.LexV2Models.DescribeBotAlias
  ( -- * Creating a Request
    DescribeBotAlias (..),
    newDescribeBotAlias,

    -- * Request Lenses
    describeBotAlias_botAliasId,
    describeBotAlias_botId,

    -- * Destructuring the Response
    DescribeBotAliasResponse (..),
    newDescribeBotAliasResponse,

    -- * Response Lenses
    describeBotAliasResponse_botAliasHistoryEvents,
    describeBotAliasResponse_botAliasId,
    describeBotAliasResponse_botAliasLocaleSettings,
    describeBotAliasResponse_botAliasName,
    describeBotAliasResponse_botAliasStatus,
    describeBotAliasResponse_botId,
    describeBotAliasResponse_botVersion,
    describeBotAliasResponse_conversationLogSettings,
    describeBotAliasResponse_creationDateTime,
    describeBotAliasResponse_description,
    describeBotAliasResponse_lastUpdatedDateTime,
    describeBotAliasResponse_sentimentAnalysisSettings,
    describeBotAliasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBotAlias' smart constructor.
data DescribeBotAlias = DescribeBotAlias'
  { -- | The identifier of the bot alias to describe.
    botAliasId :: Prelude.Text,
    -- | The identifier of the bot associated with the bot alias to describe.
    botId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBotAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasId', 'describeBotAlias_botAliasId' - The identifier of the bot alias to describe.
--
-- 'botId', 'describeBotAlias_botId' - The identifier of the bot associated with the bot alias to describe.
newDescribeBotAlias ::
  -- | 'botAliasId'
  Prelude.Text ->
  -- | 'botId'
  Prelude.Text ->
  DescribeBotAlias
newDescribeBotAlias pBotAliasId_ pBotId_ =
  DescribeBotAlias'
    { botAliasId = pBotAliasId_,
      botId = pBotId_
    }

-- | The identifier of the bot alias to describe.
describeBotAlias_botAliasId :: Lens.Lens' DescribeBotAlias Prelude.Text
describeBotAlias_botAliasId = Lens.lens (\DescribeBotAlias' {botAliasId} -> botAliasId) (\s@DescribeBotAlias' {} a -> s {botAliasId = a} :: DescribeBotAlias)

-- | The identifier of the bot associated with the bot alias to describe.
describeBotAlias_botId :: Lens.Lens' DescribeBotAlias Prelude.Text
describeBotAlias_botId = Lens.lens (\DescribeBotAlias' {botId} -> botId) (\s@DescribeBotAlias' {} a -> s {botId = a} :: DescribeBotAlias)

instance Core.AWSRequest DescribeBotAlias where
  type
    AWSResponse DescribeBotAlias =
      DescribeBotAliasResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBotAliasResponse'
            Prelude.<$> ( x
                            Data..?> "botAliasHistoryEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "botAliasId")
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
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "sentimentAnalysisSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBotAlias where
  hashWithSalt _salt DescribeBotAlias' {..} =
    _salt
      `Prelude.hashWithSalt` botAliasId
      `Prelude.hashWithSalt` botId

instance Prelude.NFData DescribeBotAlias where
  rnf DescribeBotAlias' {..} =
    Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf botId

instance Data.ToHeaders DescribeBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeBotAlias where
  toPath DescribeBotAlias' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botaliases/",
        Data.toBS botAliasId,
        "/"
      ]

instance Data.ToQuery DescribeBotAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBotAliasResponse' smart constructor.
data DescribeBotAliasResponse = DescribeBotAliasResponse'
  { -- | A list of events that affect a bot alias. For example, an event is
    -- recorded when the version that the alias points to changes.
    botAliasHistoryEvents :: Prelude.Maybe [BotAliasHistoryEvent],
    -- | The identifier of the bot alias.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | The locale settings that are unique to the alias.
    botAliasLocaleSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings),
    -- | The name of the bot alias.
    botAliasName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the alias. When the alias is @Available@, the
    -- alias is ready for use with your bot.
    botAliasStatus :: Prelude.Maybe BotAliasStatus,
    -- | The identifier of the bot associated with the bot alias.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The version of the bot associated with the bot alias.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifics of how Amazon Lex logs text and audio conversations with the
    -- bot associated with the alias.
    conversationLogSettings :: Prelude.Maybe ConversationLogSettings,
    -- | A timestamp of the date and time that the alias was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the bot alias.
    description :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the alias was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    sentimentAnalysisSettings :: Prelude.Maybe SentimentAnalysisSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBotAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasHistoryEvents', 'describeBotAliasResponse_botAliasHistoryEvents' - A list of events that affect a bot alias. For example, an event is
-- recorded when the version that the alias points to changes.
--
-- 'botAliasId', 'describeBotAliasResponse_botAliasId' - The identifier of the bot alias.
--
-- 'botAliasLocaleSettings', 'describeBotAliasResponse_botAliasLocaleSettings' - The locale settings that are unique to the alias.
--
-- 'botAliasName', 'describeBotAliasResponse_botAliasName' - The name of the bot alias.
--
-- 'botAliasStatus', 'describeBotAliasResponse_botAliasStatus' - The current status of the alias. When the alias is @Available@, the
-- alias is ready for use with your bot.
--
-- 'botId', 'describeBotAliasResponse_botId' - The identifier of the bot associated with the bot alias.
--
-- 'botVersion', 'describeBotAliasResponse_botVersion' - The version of the bot associated with the bot alias.
--
-- 'conversationLogSettings', 'describeBotAliasResponse_conversationLogSettings' - Specifics of how Amazon Lex logs text and audio conversations with the
-- bot associated with the alias.
--
-- 'creationDateTime', 'describeBotAliasResponse_creationDateTime' - A timestamp of the date and time that the alias was created.
--
-- 'description', 'describeBotAliasResponse_description' - The description of the bot alias.
--
-- 'lastUpdatedDateTime', 'describeBotAliasResponse_lastUpdatedDateTime' - A timestamp of the date and time that the alias was last updated.
--
-- 'sentimentAnalysisSettings', 'describeBotAliasResponse_sentimentAnalysisSettings' - Undocumented member.
--
-- 'httpStatus', 'describeBotAliasResponse_httpStatus' - The response's http status code.
newDescribeBotAliasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBotAliasResponse
newDescribeBotAliasResponse pHttpStatus_ =
  DescribeBotAliasResponse'
    { botAliasHistoryEvents =
        Prelude.Nothing,
      botAliasId = Prelude.Nothing,
      botAliasLocaleSettings = Prelude.Nothing,
      botAliasName = Prelude.Nothing,
      botAliasStatus = Prelude.Nothing,
      botId = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      conversationLogSettings = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      sentimentAnalysisSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of events that affect a bot alias. For example, an event is
-- recorded when the version that the alias points to changes.
describeBotAliasResponse_botAliasHistoryEvents :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe [BotAliasHistoryEvent])
describeBotAliasResponse_botAliasHistoryEvents = Lens.lens (\DescribeBotAliasResponse' {botAliasHistoryEvents} -> botAliasHistoryEvents) (\s@DescribeBotAliasResponse' {} a -> s {botAliasHistoryEvents = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The identifier of the bot alias.
describeBotAliasResponse_botAliasId :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botAliasId = Lens.lens (\DescribeBotAliasResponse' {botAliasId} -> botAliasId) (\s@DescribeBotAliasResponse' {} a -> s {botAliasId = a} :: DescribeBotAliasResponse)

-- | The locale settings that are unique to the alias.
describeBotAliasResponse_botAliasLocaleSettings :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings))
describeBotAliasResponse_botAliasLocaleSettings = Lens.lens (\DescribeBotAliasResponse' {botAliasLocaleSettings} -> botAliasLocaleSettings) (\s@DescribeBotAliasResponse' {} a -> s {botAliasLocaleSettings = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the bot alias.
describeBotAliasResponse_botAliasName :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botAliasName = Lens.lens (\DescribeBotAliasResponse' {botAliasName} -> botAliasName) (\s@DescribeBotAliasResponse' {} a -> s {botAliasName = a} :: DescribeBotAliasResponse)

-- | The current status of the alias. When the alias is @Available@, the
-- alias is ready for use with your bot.
describeBotAliasResponse_botAliasStatus :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe BotAliasStatus)
describeBotAliasResponse_botAliasStatus = Lens.lens (\DescribeBotAliasResponse' {botAliasStatus} -> botAliasStatus) (\s@DescribeBotAliasResponse' {} a -> s {botAliasStatus = a} :: DescribeBotAliasResponse)

-- | The identifier of the bot associated with the bot alias.
describeBotAliasResponse_botId :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botId = Lens.lens (\DescribeBotAliasResponse' {botId} -> botId) (\s@DescribeBotAliasResponse' {} a -> s {botId = a} :: DescribeBotAliasResponse)

-- | The version of the bot associated with the bot alias.
describeBotAliasResponse_botVersion :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botVersion = Lens.lens (\DescribeBotAliasResponse' {botVersion} -> botVersion) (\s@DescribeBotAliasResponse' {} a -> s {botVersion = a} :: DescribeBotAliasResponse)

-- | Specifics of how Amazon Lex logs text and audio conversations with the
-- bot associated with the alias.
describeBotAliasResponse_conversationLogSettings :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe ConversationLogSettings)
describeBotAliasResponse_conversationLogSettings = Lens.lens (\DescribeBotAliasResponse' {conversationLogSettings} -> conversationLogSettings) (\s@DescribeBotAliasResponse' {} a -> s {conversationLogSettings = a} :: DescribeBotAliasResponse)

-- | A timestamp of the date and time that the alias was created.
describeBotAliasResponse_creationDateTime :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
describeBotAliasResponse_creationDateTime = Lens.lens (\DescribeBotAliasResponse' {creationDateTime} -> creationDateTime) (\s@DescribeBotAliasResponse' {} a -> s {creationDateTime = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the bot alias.
describeBotAliasResponse_description :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_description = Lens.lens (\DescribeBotAliasResponse' {description} -> description) (\s@DescribeBotAliasResponse' {} a -> s {description = a} :: DescribeBotAliasResponse)

-- | A timestamp of the date and time that the alias was last updated.
describeBotAliasResponse_lastUpdatedDateTime :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
describeBotAliasResponse_lastUpdatedDateTime = Lens.lens (\DescribeBotAliasResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeBotAliasResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
describeBotAliasResponse_sentimentAnalysisSettings :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe SentimentAnalysisSettings)
describeBotAliasResponse_sentimentAnalysisSettings = Lens.lens (\DescribeBotAliasResponse' {sentimentAnalysisSettings} -> sentimentAnalysisSettings) (\s@DescribeBotAliasResponse' {} a -> s {sentimentAnalysisSettings = a} :: DescribeBotAliasResponse)

-- | The response's http status code.
describeBotAliasResponse_httpStatus :: Lens.Lens' DescribeBotAliasResponse Prelude.Int
describeBotAliasResponse_httpStatus = Lens.lens (\DescribeBotAliasResponse' {httpStatus} -> httpStatus) (\s@DescribeBotAliasResponse' {} a -> s {httpStatus = a} :: DescribeBotAliasResponse)

instance Prelude.NFData DescribeBotAliasResponse where
  rnf DescribeBotAliasResponse' {..} =
    Prelude.rnf botAliasHistoryEvents
      `Prelude.seq` Prelude.rnf botAliasId
      `Prelude.seq` Prelude.rnf botAliasLocaleSettings
      `Prelude.seq` Prelude.rnf botAliasName
      `Prelude.seq` Prelude.rnf botAliasStatus
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf conversationLogSettings
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf sentimentAnalysisSettings
      `Prelude.seq` Prelude.rnf httpStatus
