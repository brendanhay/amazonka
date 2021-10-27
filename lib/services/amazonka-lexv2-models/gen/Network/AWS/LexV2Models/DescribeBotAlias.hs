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
-- Module      : Network.AWS.LexV2Models.DescribeBotAlias
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a specific bot alias.
module Network.AWS.LexV2Models.DescribeBotAlias
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
    describeBotAliasResponse_botAliasLocaleSettings,
    describeBotAliasResponse_botAliasStatus,
    describeBotAliasResponse_botVersion,
    describeBotAliasResponse_lastUpdatedDateTime,
    describeBotAliasResponse_conversationLogSettings,
    describeBotAliasResponse_botId,
    describeBotAliasResponse_botAliasId,
    describeBotAliasResponse_creationDateTime,
    describeBotAliasResponse_sentimentAnalysisSettings,
    describeBotAliasResponse_botAliasName,
    describeBotAliasResponse_description,
    describeBotAliasResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBotAliasResponse'
            Prelude.<$> ( x Core..?> "botAliasHistoryEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "botAliasLocaleSettings"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "botAliasStatus")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "conversationLogSettings")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "botAliasId")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "sentimentAnalysisSettings")
            Prelude.<*> (x Core..?> "botAliasName")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBotAlias

instance Prelude.NFData DescribeBotAlias

instance Core.ToHeaders DescribeBotAlias where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeBotAlias where
  toPath DescribeBotAlias' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botaliases/",
        Core.toBS botAliasId,
        "/"
      ]

instance Core.ToQuery DescribeBotAlias where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBotAliasResponse' smart constructor.
data DescribeBotAliasResponse = DescribeBotAliasResponse'
  { -- | A list of events that affect a bot alias. For example, an event is
    -- recorded when the version that the alias points to changes.
    botAliasHistoryEvents :: Prelude.Maybe [BotAliasHistoryEvent],
    -- | The locale settings that are unique to the alias.
    botAliasLocaleSettings :: Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings),
    -- | The current status of the alias. When the alias is @Available@, the
    -- alias is ready for use with your bot.
    botAliasStatus :: Prelude.Maybe BotAliasStatus,
    -- | The version of the bot associated with the bot alias.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the alias was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | Specifics of how Amazon Lex logs text and audio conversations with the
    -- bot associated with the alias.
    conversationLogSettings :: Prelude.Maybe ConversationLogSettings,
    -- | The identifier of the bot associated with the bot alias.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot alias.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the alias was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    sentimentAnalysisSettings :: Prelude.Maybe SentimentAnalysisSettings,
    -- | The name of the bot alias.
    botAliasName :: Prelude.Maybe Prelude.Text,
    -- | The description of the bot alias.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'botAliasLocaleSettings', 'describeBotAliasResponse_botAliasLocaleSettings' - The locale settings that are unique to the alias.
--
-- 'botAliasStatus', 'describeBotAliasResponse_botAliasStatus' - The current status of the alias. When the alias is @Available@, the
-- alias is ready for use with your bot.
--
-- 'botVersion', 'describeBotAliasResponse_botVersion' - The version of the bot associated with the bot alias.
--
-- 'lastUpdatedDateTime', 'describeBotAliasResponse_lastUpdatedDateTime' - A timestamp of the date and time that the alias was last updated.
--
-- 'conversationLogSettings', 'describeBotAliasResponse_conversationLogSettings' - Specifics of how Amazon Lex logs text and audio conversations with the
-- bot associated with the alias.
--
-- 'botId', 'describeBotAliasResponse_botId' - The identifier of the bot associated with the bot alias.
--
-- 'botAliasId', 'describeBotAliasResponse_botAliasId' - The identifier of the bot alias.
--
-- 'creationDateTime', 'describeBotAliasResponse_creationDateTime' - A timestamp of the date and time that the alias was created.
--
-- 'sentimentAnalysisSettings', 'describeBotAliasResponse_sentimentAnalysisSettings' - Undocumented member.
--
-- 'botAliasName', 'describeBotAliasResponse_botAliasName' - The name of the bot alias.
--
-- 'description', 'describeBotAliasResponse_description' - The description of the bot alias.
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
      botAliasLocaleSettings = Prelude.Nothing,
      botAliasStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      conversationLogSettings = Prelude.Nothing,
      botId = Prelude.Nothing,
      botAliasId = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      sentimentAnalysisSettings = Prelude.Nothing,
      botAliasName = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of events that affect a bot alias. For example, an event is
-- recorded when the version that the alias points to changes.
describeBotAliasResponse_botAliasHistoryEvents :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe [BotAliasHistoryEvent])
describeBotAliasResponse_botAliasHistoryEvents = Lens.lens (\DescribeBotAliasResponse' {botAliasHistoryEvents} -> botAliasHistoryEvents) (\s@DescribeBotAliasResponse' {} a -> s {botAliasHistoryEvents = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The locale settings that are unique to the alias.
describeBotAliasResponse_botAliasLocaleSettings :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text BotAliasLocaleSettings))
describeBotAliasResponse_botAliasLocaleSettings = Lens.lens (\DescribeBotAliasResponse' {botAliasLocaleSettings} -> botAliasLocaleSettings) (\s@DescribeBotAliasResponse' {} a -> s {botAliasLocaleSettings = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current status of the alias. When the alias is @Available@, the
-- alias is ready for use with your bot.
describeBotAliasResponse_botAliasStatus :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe BotAliasStatus)
describeBotAliasResponse_botAliasStatus = Lens.lens (\DescribeBotAliasResponse' {botAliasStatus} -> botAliasStatus) (\s@DescribeBotAliasResponse' {} a -> s {botAliasStatus = a} :: DescribeBotAliasResponse)

-- | The version of the bot associated with the bot alias.
describeBotAliasResponse_botVersion :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botVersion = Lens.lens (\DescribeBotAliasResponse' {botVersion} -> botVersion) (\s@DescribeBotAliasResponse' {} a -> s {botVersion = a} :: DescribeBotAliasResponse)

-- | A timestamp of the date and time that the alias was last updated.
describeBotAliasResponse_lastUpdatedDateTime :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
describeBotAliasResponse_lastUpdatedDateTime = Lens.lens (\DescribeBotAliasResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeBotAliasResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Core._Time

-- | Specifics of how Amazon Lex logs text and audio conversations with the
-- bot associated with the alias.
describeBotAliasResponse_conversationLogSettings :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe ConversationLogSettings)
describeBotAliasResponse_conversationLogSettings = Lens.lens (\DescribeBotAliasResponse' {conversationLogSettings} -> conversationLogSettings) (\s@DescribeBotAliasResponse' {} a -> s {conversationLogSettings = a} :: DescribeBotAliasResponse)

-- | The identifier of the bot associated with the bot alias.
describeBotAliasResponse_botId :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botId = Lens.lens (\DescribeBotAliasResponse' {botId} -> botId) (\s@DescribeBotAliasResponse' {} a -> s {botId = a} :: DescribeBotAliasResponse)

-- | The identifier of the bot alias.
describeBotAliasResponse_botAliasId :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botAliasId = Lens.lens (\DescribeBotAliasResponse' {botAliasId} -> botAliasId) (\s@DescribeBotAliasResponse' {} a -> s {botAliasId = a} :: DescribeBotAliasResponse)

-- | A timestamp of the date and time that the alias was created.
describeBotAliasResponse_creationDateTime :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.UTCTime)
describeBotAliasResponse_creationDateTime = Lens.lens (\DescribeBotAliasResponse' {creationDateTime} -> creationDateTime) (\s@DescribeBotAliasResponse' {} a -> s {creationDateTime = a} :: DescribeBotAliasResponse) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
describeBotAliasResponse_sentimentAnalysisSettings :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe SentimentAnalysisSettings)
describeBotAliasResponse_sentimentAnalysisSettings = Lens.lens (\DescribeBotAliasResponse' {sentimentAnalysisSettings} -> sentimentAnalysisSettings) (\s@DescribeBotAliasResponse' {} a -> s {sentimentAnalysisSettings = a} :: DescribeBotAliasResponse)

-- | The name of the bot alias.
describeBotAliasResponse_botAliasName :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_botAliasName = Lens.lens (\DescribeBotAliasResponse' {botAliasName} -> botAliasName) (\s@DescribeBotAliasResponse' {} a -> s {botAliasName = a} :: DescribeBotAliasResponse)

-- | The description of the bot alias.
describeBotAliasResponse_description :: Lens.Lens' DescribeBotAliasResponse (Prelude.Maybe Prelude.Text)
describeBotAliasResponse_description = Lens.lens (\DescribeBotAliasResponse' {description} -> description) (\s@DescribeBotAliasResponse' {} a -> s {description = a} :: DescribeBotAliasResponse)

-- | The response's http status code.
describeBotAliasResponse_httpStatus :: Lens.Lens' DescribeBotAliasResponse Prelude.Int
describeBotAliasResponse_httpStatus = Lens.lens (\DescribeBotAliasResponse' {httpStatus} -> httpStatus) (\s@DescribeBotAliasResponse' {} a -> s {httpStatus = a} :: DescribeBotAliasResponse)

instance Prelude.NFData DescribeBotAliasResponse
