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
-- Module      : Amazonka.LexV2Models.UpdateBotLocale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings that a bot has for a specific locale.
module Amazonka.LexV2Models.UpdateBotLocale
  ( -- * Creating a Request
    UpdateBotLocale (..),
    newUpdateBotLocale,

    -- * Request Lenses
    updateBotLocale_description,
    updateBotLocale_voiceSettings,
    updateBotLocale_botId,
    updateBotLocale_botVersion,
    updateBotLocale_localeId,
    updateBotLocale_nluIntentConfidenceThreshold,

    -- * Destructuring the Response
    UpdateBotLocaleResponse (..),
    newUpdateBotLocaleResponse,

    -- * Response Lenses
    updateBotLocaleResponse_botId,
    updateBotLocaleResponse_botLocaleStatus,
    updateBotLocaleResponse_botVersion,
    updateBotLocaleResponse_creationDateTime,
    updateBotLocaleResponse_description,
    updateBotLocaleResponse_failureReasons,
    updateBotLocaleResponse_lastUpdatedDateTime,
    updateBotLocaleResponse_localeId,
    updateBotLocaleResponse_localeName,
    updateBotLocaleResponse_nluIntentConfidenceThreshold,
    updateBotLocaleResponse_recommendedActions,
    updateBotLocaleResponse_voiceSettings,
    updateBotLocaleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBotLocale' smart constructor.
data UpdateBotLocale = UpdateBotLocale'
  { -- | The new description of the locale.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new Amazon Polly voice Amazon Lex should use for voice interaction
    -- with the user.
    voiceSettings :: Prelude.Maybe VoiceSettings,
    -- | The unique identifier of the bot that contains the locale.
    botId :: Prelude.Text,
    -- | The version of the bot that contains the locale to be updated. The
    -- version can only be the @DRAFT@ version.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale to update. The string must
    -- match one of the supported locales. For more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text,
    -- | The new confidence threshold where Amazon Lex inserts the
    -- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
    -- list of possible intents for an utterance.
    nluIntentConfidenceThreshold :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBotLocale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateBotLocale_description' - The new description of the locale.
--
-- 'voiceSettings', 'updateBotLocale_voiceSettings' - The new Amazon Polly voice Amazon Lex should use for voice interaction
-- with the user.
--
-- 'botId', 'updateBotLocale_botId' - The unique identifier of the bot that contains the locale.
--
-- 'botVersion', 'updateBotLocale_botVersion' - The version of the bot that contains the locale to be updated. The
-- version can only be the @DRAFT@ version.
--
-- 'localeId', 'updateBotLocale_localeId' - The identifier of the language and locale to update. The string must
-- match one of the supported locales. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'nluIntentConfidenceThreshold', 'updateBotLocale_nluIntentConfidenceThreshold' - The new confidence threshold where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
-- list of possible intents for an utterance.
newUpdateBotLocale ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'nluIntentConfidenceThreshold'
  Prelude.Double ->
  UpdateBotLocale
newUpdateBotLocale
  pBotId_
  pBotVersion_
  pLocaleId_
  pNluIntentConfidenceThreshold_ =
    UpdateBotLocale'
      { description = Prelude.Nothing,
        voiceSettings = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        nluIntentConfidenceThreshold =
          pNluIntentConfidenceThreshold_
      }

-- | The new description of the locale.
updateBotLocale_description :: Lens.Lens' UpdateBotLocale (Prelude.Maybe Prelude.Text)
updateBotLocale_description = Lens.lens (\UpdateBotLocale' {description} -> description) (\s@UpdateBotLocale' {} a -> s {description = a} :: UpdateBotLocale)

-- | The new Amazon Polly voice Amazon Lex should use for voice interaction
-- with the user.
updateBotLocale_voiceSettings :: Lens.Lens' UpdateBotLocale (Prelude.Maybe VoiceSettings)
updateBotLocale_voiceSettings = Lens.lens (\UpdateBotLocale' {voiceSettings} -> voiceSettings) (\s@UpdateBotLocale' {} a -> s {voiceSettings = a} :: UpdateBotLocale)

-- | The unique identifier of the bot that contains the locale.
updateBotLocale_botId :: Lens.Lens' UpdateBotLocale Prelude.Text
updateBotLocale_botId = Lens.lens (\UpdateBotLocale' {botId} -> botId) (\s@UpdateBotLocale' {} a -> s {botId = a} :: UpdateBotLocale)

-- | The version of the bot that contains the locale to be updated. The
-- version can only be the @DRAFT@ version.
updateBotLocale_botVersion :: Lens.Lens' UpdateBotLocale Prelude.Text
updateBotLocale_botVersion = Lens.lens (\UpdateBotLocale' {botVersion} -> botVersion) (\s@UpdateBotLocale' {} a -> s {botVersion = a} :: UpdateBotLocale)

-- | The identifier of the language and locale to update. The string must
-- match one of the supported locales. For more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
updateBotLocale_localeId :: Lens.Lens' UpdateBotLocale Prelude.Text
updateBotLocale_localeId = Lens.lens (\UpdateBotLocale' {localeId} -> localeId) (\s@UpdateBotLocale' {} a -> s {localeId = a} :: UpdateBotLocale)

-- | The new confidence threshold where Amazon Lex inserts the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
-- list of possible intents for an utterance.
updateBotLocale_nluIntentConfidenceThreshold :: Lens.Lens' UpdateBotLocale Prelude.Double
updateBotLocale_nluIntentConfidenceThreshold = Lens.lens (\UpdateBotLocale' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@UpdateBotLocale' {} a -> s {nluIntentConfidenceThreshold = a} :: UpdateBotLocale)

instance Core.AWSRequest UpdateBotLocale where
  type
    AWSResponse UpdateBotLocale =
      UpdateBotLocaleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBotLocaleResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botLocaleStatus")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "failureReasons" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "lastUpdatedDateTime")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "localeName")
            Prelude.<*> (x Data..?> "nluIntentConfidenceThreshold")
            Prelude.<*> ( x
                            Data..?> "recommendedActions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "voiceSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBotLocale where
  hashWithSalt _salt UpdateBotLocale' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` voiceSettings
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` nluIntentConfidenceThreshold

instance Prelude.NFData UpdateBotLocale where
  rnf UpdateBotLocale' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf voiceSettings
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nluIntentConfidenceThreshold

instance Data.ToHeaders UpdateBotLocale where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBotLocale where
  toJSON UpdateBotLocale' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("voiceSettings" Data..=) Prelude.<$> voiceSettings,
            Prelude.Just
              ( "nluIntentConfidenceThreshold"
                  Data..= nluIntentConfidenceThreshold
              )
          ]
      )

instance Data.ToPath UpdateBotLocale where
  toPath UpdateBotLocale' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/",
        Data.toBS localeId,
        "/"
      ]

instance Data.ToQuery UpdateBotLocale where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBotLocaleResponse' smart constructor.
data UpdateBotLocaleResponse = UpdateBotLocaleResponse'
  { -- | The identifier of the bot that contains the updated locale.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the locale. When the bot status is @Built@ the
    -- locale is ready for use.
    botLocaleStatus :: Prelude.Maybe BotLocaleStatus,
    -- | The version of the bot that contains the updated locale.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the locale was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The updated description of the locale.
    description :: Prelude.Maybe Prelude.Text,
    -- | If the @botLocaleStatus@ is @Failed@, the @failureReasons@ field lists
    -- the errors that occurred while building the bot.
    failureReasons :: Prelude.Maybe [Prelude.Text],
    -- | A timestamp of the date and time that the locale was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The language and locale of the updated bot locale.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The updated locale name for the locale.
    localeName :: Prelude.Maybe Prelude.Text,
    -- | The updated confidence threshold for inserting the
    -- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
    -- list of possible intents for an utterance.
    nluIntentConfidenceThreshold :: Prelude.Maybe Prelude.Double,
    -- | Recommended actions to take to resolve an error in the @failureReasons@
    -- field.
    recommendedActions :: Prelude.Maybe [Prelude.Text],
    -- | The updated Amazon Polly voice to use for voice interaction with the
    -- user.
    voiceSettings :: Prelude.Maybe VoiceSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBotLocaleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'updateBotLocaleResponse_botId' - The identifier of the bot that contains the updated locale.
--
-- 'botLocaleStatus', 'updateBotLocaleResponse_botLocaleStatus' - The current status of the locale. When the bot status is @Built@ the
-- locale is ready for use.
--
-- 'botVersion', 'updateBotLocaleResponse_botVersion' - The version of the bot that contains the updated locale.
--
-- 'creationDateTime', 'updateBotLocaleResponse_creationDateTime' - A timestamp of the date and time that the locale was created.
--
-- 'description', 'updateBotLocaleResponse_description' - The updated description of the locale.
--
-- 'failureReasons', 'updateBotLocaleResponse_failureReasons' - If the @botLocaleStatus@ is @Failed@, the @failureReasons@ field lists
-- the errors that occurred while building the bot.
--
-- 'lastUpdatedDateTime', 'updateBotLocaleResponse_lastUpdatedDateTime' - A timestamp of the date and time that the locale was last updated.
--
-- 'localeId', 'updateBotLocaleResponse_localeId' - The language and locale of the updated bot locale.
--
-- 'localeName', 'updateBotLocaleResponse_localeName' - The updated locale name for the locale.
--
-- 'nluIntentConfidenceThreshold', 'updateBotLocaleResponse_nluIntentConfidenceThreshold' - The updated confidence threshold for inserting the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
-- list of possible intents for an utterance.
--
-- 'recommendedActions', 'updateBotLocaleResponse_recommendedActions' - Recommended actions to take to resolve an error in the @failureReasons@
-- field.
--
-- 'voiceSettings', 'updateBotLocaleResponse_voiceSettings' - The updated Amazon Polly voice to use for voice interaction with the
-- user.
--
-- 'httpStatus', 'updateBotLocaleResponse_httpStatus' - The response's http status code.
newUpdateBotLocaleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBotLocaleResponse
newUpdateBotLocaleResponse pHttpStatus_ =
  UpdateBotLocaleResponse'
    { botId = Prelude.Nothing,
      botLocaleStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      failureReasons = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      localeId = Prelude.Nothing,
      localeName = Prelude.Nothing,
      nluIntentConfidenceThreshold = Prelude.Nothing,
      recommendedActions = Prelude.Nothing,
      voiceSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the bot that contains the updated locale.
updateBotLocaleResponse_botId :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.Text)
updateBotLocaleResponse_botId = Lens.lens (\UpdateBotLocaleResponse' {botId} -> botId) (\s@UpdateBotLocaleResponse' {} a -> s {botId = a} :: UpdateBotLocaleResponse)

-- | The current status of the locale. When the bot status is @Built@ the
-- locale is ready for use.
updateBotLocaleResponse_botLocaleStatus :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe BotLocaleStatus)
updateBotLocaleResponse_botLocaleStatus = Lens.lens (\UpdateBotLocaleResponse' {botLocaleStatus} -> botLocaleStatus) (\s@UpdateBotLocaleResponse' {} a -> s {botLocaleStatus = a} :: UpdateBotLocaleResponse)

-- | The version of the bot that contains the updated locale.
updateBotLocaleResponse_botVersion :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.Text)
updateBotLocaleResponse_botVersion = Lens.lens (\UpdateBotLocaleResponse' {botVersion} -> botVersion) (\s@UpdateBotLocaleResponse' {} a -> s {botVersion = a} :: UpdateBotLocaleResponse)

-- | A timestamp of the date and time that the locale was created.
updateBotLocaleResponse_creationDateTime :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.UTCTime)
updateBotLocaleResponse_creationDateTime = Lens.lens (\UpdateBotLocaleResponse' {creationDateTime} -> creationDateTime) (\s@UpdateBotLocaleResponse' {} a -> s {creationDateTime = a} :: UpdateBotLocaleResponse) Prelude.. Lens.mapping Data._Time

-- | The updated description of the locale.
updateBotLocaleResponse_description :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.Text)
updateBotLocaleResponse_description = Lens.lens (\UpdateBotLocaleResponse' {description} -> description) (\s@UpdateBotLocaleResponse' {} a -> s {description = a} :: UpdateBotLocaleResponse)

-- | If the @botLocaleStatus@ is @Failed@, the @failureReasons@ field lists
-- the errors that occurred while building the bot.
updateBotLocaleResponse_failureReasons :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe [Prelude.Text])
updateBotLocaleResponse_failureReasons = Lens.lens (\UpdateBotLocaleResponse' {failureReasons} -> failureReasons) (\s@UpdateBotLocaleResponse' {} a -> s {failureReasons = a} :: UpdateBotLocaleResponse) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp of the date and time that the locale was last updated.
updateBotLocaleResponse_lastUpdatedDateTime :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.UTCTime)
updateBotLocaleResponse_lastUpdatedDateTime = Lens.lens (\UpdateBotLocaleResponse' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@UpdateBotLocaleResponse' {} a -> s {lastUpdatedDateTime = a} :: UpdateBotLocaleResponse) Prelude.. Lens.mapping Data._Time

-- | The language and locale of the updated bot locale.
updateBotLocaleResponse_localeId :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.Text)
updateBotLocaleResponse_localeId = Lens.lens (\UpdateBotLocaleResponse' {localeId} -> localeId) (\s@UpdateBotLocaleResponse' {} a -> s {localeId = a} :: UpdateBotLocaleResponse)

-- | The updated locale name for the locale.
updateBotLocaleResponse_localeName :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.Text)
updateBotLocaleResponse_localeName = Lens.lens (\UpdateBotLocaleResponse' {localeName} -> localeName) (\s@UpdateBotLocaleResponse' {} a -> s {localeName = a} :: UpdateBotLocaleResponse)

-- | The updated confidence threshold for inserting the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents in the
-- list of possible intents for an utterance.
updateBotLocaleResponse_nluIntentConfidenceThreshold :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe Prelude.Double)
updateBotLocaleResponse_nluIntentConfidenceThreshold = Lens.lens (\UpdateBotLocaleResponse' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@UpdateBotLocaleResponse' {} a -> s {nluIntentConfidenceThreshold = a} :: UpdateBotLocaleResponse)

-- | Recommended actions to take to resolve an error in the @failureReasons@
-- field.
updateBotLocaleResponse_recommendedActions :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe [Prelude.Text])
updateBotLocaleResponse_recommendedActions = Lens.lens (\UpdateBotLocaleResponse' {recommendedActions} -> recommendedActions) (\s@UpdateBotLocaleResponse' {} a -> s {recommendedActions = a} :: UpdateBotLocaleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The updated Amazon Polly voice to use for voice interaction with the
-- user.
updateBotLocaleResponse_voiceSettings :: Lens.Lens' UpdateBotLocaleResponse (Prelude.Maybe VoiceSettings)
updateBotLocaleResponse_voiceSettings = Lens.lens (\UpdateBotLocaleResponse' {voiceSettings} -> voiceSettings) (\s@UpdateBotLocaleResponse' {} a -> s {voiceSettings = a} :: UpdateBotLocaleResponse)

-- | The response's http status code.
updateBotLocaleResponse_httpStatus :: Lens.Lens' UpdateBotLocaleResponse Prelude.Int
updateBotLocaleResponse_httpStatus = Lens.lens (\UpdateBotLocaleResponse' {httpStatus} -> httpStatus) (\s@UpdateBotLocaleResponse' {} a -> s {httpStatus = a} :: UpdateBotLocaleResponse)

instance Prelude.NFData UpdateBotLocaleResponse where
  rnf UpdateBotLocaleResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botLocaleStatus
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf failureReasons
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf localeName
      `Prelude.seq` Prelude.rnf nluIntentConfidenceThreshold
      `Prelude.seq` Prelude.rnf recommendedActions
      `Prelude.seq` Prelude.rnf voiceSettings
      `Prelude.seq` Prelude.rnf httpStatus
