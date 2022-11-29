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
-- Module      : Amazonka.LexV2Models.DescribeBotLocale
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the settings that a bot has for a specific locale.
module Amazonka.LexV2Models.DescribeBotLocale
  ( -- * Creating a Request
    DescribeBotLocale (..),
    newDescribeBotLocale,

    -- * Request Lenses
    describeBotLocale_botId,
    describeBotLocale_botVersion,
    describeBotLocale_localeId,

    -- * Destructuring the Response
    DescribeBotLocaleResponse (..),
    newDescribeBotLocaleResponse,

    -- * Response Lenses
    describeBotLocaleResponse_nluIntentConfidenceThreshold,
    describeBotLocaleResponse_botVersion,
    describeBotLocaleResponse_slotTypesCount,
    describeBotLocaleResponse_creationDateTime,
    describeBotLocaleResponse_localeName,
    describeBotLocaleResponse_localeId,
    describeBotLocaleResponse_botLocaleHistoryEvents,
    describeBotLocaleResponse_recommendedActions,
    describeBotLocaleResponse_description,
    describeBotLocaleResponse_botId,
    describeBotLocaleResponse_lastBuildSubmittedDateTime,
    describeBotLocaleResponse_botLocaleStatus,
    describeBotLocaleResponse_voiceSettings,
    describeBotLocaleResponse_failureReasons,
    describeBotLocaleResponse_lastUpdatedDateTime,
    describeBotLocaleResponse_intentsCount,
    describeBotLocaleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeBotLocale' smart constructor.
data DescribeBotLocale = DescribeBotLocale'
  { -- | The identifier of the bot associated with the locale.
    botId :: Prelude.Text,
    -- | The identifier of the version of the bot associated with the locale.
    botVersion :: Prelude.Text,
    -- | The unique identifier of the locale to describe. The string must match
    -- one of the supported locales. For more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBotLocale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'describeBotLocale_botId' - The identifier of the bot associated with the locale.
--
-- 'botVersion', 'describeBotLocale_botVersion' - The identifier of the version of the bot associated with the locale.
--
-- 'localeId', 'describeBotLocale_localeId' - The unique identifier of the locale to describe. The string must match
-- one of the supported locales. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newDescribeBotLocale ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  DescribeBotLocale
newDescribeBotLocale pBotId_ pBotVersion_ pLocaleId_ =
  DescribeBotLocale'
    { botId = pBotId_,
      botVersion = pBotVersion_,
      localeId = pLocaleId_
    }

-- | The identifier of the bot associated with the locale.
describeBotLocale_botId :: Lens.Lens' DescribeBotLocale Prelude.Text
describeBotLocale_botId = Lens.lens (\DescribeBotLocale' {botId} -> botId) (\s@DescribeBotLocale' {} a -> s {botId = a} :: DescribeBotLocale)

-- | The identifier of the version of the bot associated with the locale.
describeBotLocale_botVersion :: Lens.Lens' DescribeBotLocale Prelude.Text
describeBotLocale_botVersion = Lens.lens (\DescribeBotLocale' {botVersion} -> botVersion) (\s@DescribeBotLocale' {} a -> s {botVersion = a} :: DescribeBotLocale)

-- | The unique identifier of the locale to describe. The string must match
-- one of the supported locales. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
describeBotLocale_localeId :: Lens.Lens' DescribeBotLocale Prelude.Text
describeBotLocale_localeId = Lens.lens (\DescribeBotLocale' {localeId} -> localeId) (\s@DescribeBotLocale' {} a -> s {localeId = a} :: DescribeBotLocale)

instance Core.AWSRequest DescribeBotLocale where
  type
    AWSResponse DescribeBotLocale =
      DescribeBotLocaleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBotLocaleResponse'
            Prelude.<$> (x Core..?> "nluIntentConfidenceThreshold")
            Prelude.<*> (x Core..?> "botVersion")
            Prelude.<*> (x Core..?> "slotTypesCount")
            Prelude.<*> (x Core..?> "creationDateTime")
            Prelude.<*> (x Core..?> "localeName")
            Prelude.<*> (x Core..?> "localeId")
            Prelude.<*> ( x Core..?> "botLocaleHistoryEvents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Core..?> "recommendedActions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "botId")
            Prelude.<*> (x Core..?> "lastBuildSubmittedDateTime")
            Prelude.<*> (x Core..?> "botLocaleStatus")
            Prelude.<*> (x Core..?> "voiceSettings")
            Prelude.<*> (x Core..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "lastUpdatedDateTime")
            Prelude.<*> (x Core..?> "intentsCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBotLocale where
  hashWithSalt _salt DescribeBotLocale' {..} =
    _salt `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData DescribeBotLocale where
  rnf DescribeBotLocale' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Core.ToHeaders DescribeBotLocale where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeBotLocale where
  toPath DescribeBotLocale' {..} =
    Prelude.mconcat
      [ "/bots/",
        Core.toBS botId,
        "/botversions/",
        Core.toBS botVersion,
        "/botlocales/",
        Core.toBS localeId,
        "/"
      ]

instance Core.ToQuery DescribeBotLocale where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBotLocaleResponse' smart constructor.
data DescribeBotLocaleResponse = DescribeBotLocaleResponse'
  { -- | The confidence threshold where Amazon Lex inserts the
    -- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
    -- list of possible intents for an utterance.
    nluIntentConfidenceThreshold :: Prelude.Maybe Prelude.Double,
    -- | The identifier of the version of the bot associated with the locale.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of slot types defined for the locale.
    slotTypesCount :: Prelude.Maybe Prelude.Int,
    -- | The date and time that the locale was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the locale.
    localeName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the described locale.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | History of changes, such as when a locale is used in an alias, that have
    -- taken place for the locale.
    botLocaleHistoryEvents :: Prelude.Maybe [BotLocaleHistoryEvent],
    -- | Recommended actions to take to resolve an error in the @failureReasons@
    -- field.
    recommendedActions :: Prelude.Maybe [Prelude.Text],
    -- | The description of the locale.
    description :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the bot associated with the locale.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the locale was last submitted for building.
    lastBuildSubmittedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The status of the bot. If the status is @Failed@, the reasons for the
    -- failure are listed in the @failureReasons@ field.
    botLocaleStatus :: Prelude.Maybe BotLocaleStatus,
    -- | The Amazon Polly voice Amazon Lex uses for voice interaction with the
    -- user.
    voiceSettings :: Prelude.Maybe VoiceSettings,
    -- | if @botLocaleStatus@ is @Failed@, Amazon Lex explains why it failed to
    -- build the bot.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | The date and time that the locale was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The number of intents defined for the locale.
    intentsCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBotLocaleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nluIntentConfidenceThreshold', 'describeBotLocaleResponse_nluIntentConfidenceThreshold' - The confidence threshold where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
-- list of possible intents for an utterance.
--
-- 'botVersion', 'describeBotLocaleResponse_botVersion' - The identifier of the version of the bot associated with the locale.
--
-- 'slotTypesCount', 'describeBotLocaleResponse_slotTypesCount' - The number of slot types defined for the locale.
--
-- 'creationDateTime', 'describeBotLocaleResponse_creationDateTime' - The date and time that the locale was created.
--
-- 'localeName', 'describeBotLocaleResponse_localeName' - The name of the locale.
--
-- 'localeId', 'describeBotLocaleResponse_localeId' - The unique identifier of the described locale.
--
-- 'botLocaleHistoryEvents', 'describeBotLocaleResponse_botLocaleHistoryEvents' - History of changes, such as when a locale is used in an alias, that have
-- taken place for the locale.
--
-- 'recommendedActions', 'describeBotLocaleResponse_recommendedActions' - Recommended actions to take to resolve an error in the @failureReasons@
-- field.
--
-- 'description', 'describeBotLocaleResponse_description' - The description of the locale.
--
-- 'botId', 'describeBotLocaleResponse_botId' - The identifier of the bot associated with the locale.
--
-- 'lastBuildSubmittedDateTime', 'describeBotLocaleResponse_lastBuildSubmittedDateTime' - The date and time that the locale was last submitted for building.
--
-- 'botLocaleStatus', 'describeBotLocaleResponse_botLocaleStatus' - The status of the bot. If the status is @Failed@, the reasons for the
-- failure are listed in the @failureReasons@ field.
--
-- 'voiceSettings', 'describeBotLocaleResponse_voiceSettings' - The Amazon Polly voice Amazon Lex uses for voice interaction with the
-- user.
--
-- 'failureReasons', 'describeBotLocaleResponse_failureReasons' - if @botLocaleStatus@ is @Failed@, Amazon Lex explains why it failed to
-- build the bot.
--
-- 'lastUpdatedDateTime', 'describeBotLocaleResponse_lastUpdatedDateTime' - The date and time that the locale was last updated.
--
-- 'intentsCount', 'describeBotLocaleResponse_intentsCount' - The number of intents defined for the locale.
--
-- 'httpStatus', 'describeBotLocaleResponse_httpStatus' - The response's http status code.
newDescribeBotLocaleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBotLocaleResponse
newDescribeBotLocaleResponse pHttpStatus_ =
  DescribeBotLocaleResponse'
    { nluIntentConfidenceThreshold =
        Prelude.Nothing,
      botVersion = Prelude.Nothing,
      slotTypesCount = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      localeName = Prelude.Nothing,
      localeId = Prelude.Nothing,
      botLocaleHistoryEvents = Prelude.Nothing,
      recommendedActions = Prelude.Nothing,
      description = Prelude.Nothing,
      botId = Prelude.Nothing,
      lastBuildSubmittedDateTime = Prelude.Nothing,
      botLocaleStatus = Prelude.Nothing,
      voiceSettings = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      intentsCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The confidence threshold where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
-- list of possible intents for an utterance.
describeBotLocaleResponse_nluIntentConfidenceThreshold :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Double)
describeBotLocaleResponse_nluIntentConfidenceThreshold = Lens.lens (\DescribeBotLocaleResponse' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@DescribeBotLocaleResponse' {} a -> s {nluIntentConfidenceThreshold = a} :: DescribeBotLocaleResponse)

-- | The identifier of the version of the bot associated with the locale.
describeBotLocaleResponse_botVersion :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Text)
describeBotLocaleResponse_botVersion = Lens.lens (\DescribeBotLocaleResponse' {botVersion} -> botVersion) (\s@DescribeBotLocaleResponse' {} a -> s {botVersion = a} :: DescribeBotLocaleResponse)

-- | The number of slot types defined for the locale.
describeBotLocaleResponse_slotTypesCount :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Int)
describeBotLocaleResponse_slotTypesCount = Lens.lens (\DescribeBotLocaleResponse' {slotTypesCount} -> slotTypesCount) (\s@DescribeBotLocaleResponse' {} a -> s {slotTypesCount = a} :: DescribeBotLocaleResponse)

-- | The date and time that the locale was created.
describeBotLocaleResponse_creationDateTime :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.UTCTime)
describeBotLocaleResponse_creationDateTime = Lens.lens (\DescribeBotLocaleResponse' {creationDateTime} -> creationDateTime) (\s@DescribeBotLocaleResponse' {} a -> s {creationDateTime = a} :: DescribeBotLocaleResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the locale.
describeBotLocaleResponse_localeName :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Text)
describeBotLocaleResponse_localeName = Lens.lens (\DescribeBotLocaleResponse' {localeName} -> localeName) (\s@DescribeBotLocaleResponse' {} a -> s {localeName = a} :: DescribeBotLocaleResponse)

-- | The unique identifier of the described locale.
describeBotLocaleResponse_localeId :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Text)
describeBotLocaleResponse_localeId = Lens.lens (\DescribeBotLocaleResponse' {localeId} -> localeId) (\s@DescribeBotLocaleResponse' {} a -> s {localeId = a} :: DescribeBotLocaleResponse)

-- | History of changes, such as when a locale is used in an alias, that have
-- taken place for the locale.
describeBotLocaleResponse_botLocaleHistoryEvents :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe [BotLocaleHistoryEvent])
describeBotLocaleResponse_botLocaleHistoryEvents = Lens.lens (\DescribeBotLocaleResponse' {botLocaleHistoryEvents} -> botLocaleHistoryEvents) (\s@DescribeBotLocaleResponse' {} a -> s {botLocaleHistoryEvents = a} :: DescribeBotLocaleResponse) Prelude.. Lens.mapping Lens.coerced

-- | Recommended actions to take to resolve an error in the @failureReasons@
-- field.
describeBotLocaleResponse_recommendedActions :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe [Prelude.Text])
describeBotLocaleResponse_recommendedActions = Lens.lens (\DescribeBotLocaleResponse' {recommendedActions} -> recommendedActions) (\s@DescribeBotLocaleResponse' {} a -> s {recommendedActions = a} :: DescribeBotLocaleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The description of the locale.
describeBotLocaleResponse_description :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Text)
describeBotLocaleResponse_description = Lens.lens (\DescribeBotLocaleResponse' {description} -> description) (\s@DescribeBotLocaleResponse' {} a -> s {description = a} :: DescribeBotLocaleResponse)

-- | The identifier of the bot associated with the locale.
describeBotLocaleResponse_botId :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Text)
describeBotLocaleResponse_botId = Lens.lens (\DescribeBotLocaleResponse' {botId} -> botId) (\s@DescribeBotLocaleResponse' {} a -> s {botId = a} :: DescribeBotLocaleResponse)

-- | The date and time that the locale was last submitted for building.
describeBotLocaleResponse_lastBuildSubmittedDateTime :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.UTCTime)
describeBotLocaleResponse_lastBuildSubmittedDateTime = Lens.lens (\DescribeBotLocaleResponse' {lastBuildSubmittedDateTime} -> lastBuildSubmittedDateTime) (\s@DescribeBotLocaleResponse' {} a -> s {lastBuildSubmittedDateTime = a} :: DescribeBotLocaleResponse) Prelude.. Lens.mapping Core._Time

-- | The status of the bot. If the status is @Failed@, the reasons for the
-- failure are listed in the @failureReasons@ field.
describeBotLocaleResponse_botLocaleStatus :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe BotLocaleStatus)
describeBotLocaleResponse_botLocaleStatus = Lens.lens (\DescribeBotLocaleResponse' {botLocaleStatus} -> botLocaleStatus) (\s@DescribeBotLocaleResponse' {} a -> s {botLocaleStatus = a} :: DescribeBotLocaleResponse)

-- | The Amazon Polly voice Amazon Lex uses for voice interaction with the
-- user.
describeBotLocaleResponse_voiceSettings :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe VoiceSettings)
describeBotLocaleResponse_voiceSettings = Lens.lens (\DescribeBotLocaleResponse' {voiceSettings} -> voiceSettings) (\s@DescribeBotLocaleResponse' {} a -> s {voiceSettings = a} :: DescribeBotLocaleResponse)

-- | if @botLocaleStatus@ is @Failed@, Amazon Lex explains why it failed to
-- build the bot.
describeBotLocaleResponse_failureReasons :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe [Prelude.Text])
describeBotLocaleResponse_failureReasons = Lens.lens (\DescribeBotLocaleResponse' {failureReasons} -> failureReasons) (\s@DescribeBotLocaleResponse' {} a -> s {failureReasons = a} :: DescribeBotLocaleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time that the locale was last updated.
describeBotLocaleResponse_lastUpdatedDateTime :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.UTCTime)
describeBotLocaleResponse_lastUpdatedDateTime = Lens.lens (\DescribeBotLocaleResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@DescribeBotLocaleResponse' {} a -> s {lastUpdatedDateTime = a} :: DescribeBotLocaleResponse) Prelude.. Lens.mapping Core._Time

-- | The number of intents defined for the locale.
describeBotLocaleResponse_intentsCount :: Lens.Lens' DescribeBotLocaleResponse (Prelude.Maybe Prelude.Int)
describeBotLocaleResponse_intentsCount = Lens.lens (\DescribeBotLocaleResponse' {intentsCount} -> intentsCount) (\s@DescribeBotLocaleResponse' {} a -> s {intentsCount = a} :: DescribeBotLocaleResponse)

-- | The response's http status code.
describeBotLocaleResponse_httpStatus :: Lens.Lens' DescribeBotLocaleResponse Prelude.Int
describeBotLocaleResponse_httpStatus = Lens.lens (\DescribeBotLocaleResponse' {httpStatus} -> httpStatus) (\s@DescribeBotLocaleResponse' {} a -> s {httpStatus = a} :: DescribeBotLocaleResponse)

instance Prelude.NFData DescribeBotLocaleResponse where
  rnf DescribeBotLocaleResponse' {..} =
    Prelude.rnf nluIntentConfidenceThreshold
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf slotTypesCount
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf localeName
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf botLocaleHistoryEvents
      `Prelude.seq` Prelude.rnf recommendedActions
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf lastBuildSubmittedDateTime
      `Prelude.seq` Prelude.rnf botLocaleStatus
      `Prelude.seq` Prelude.rnf voiceSettings
      `Prelude.seq` Prelude.rnf failureReasons
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf intentsCount
      `Prelude.seq` Prelude.rnf httpStatus
