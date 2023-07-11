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
-- Module      : Amazonka.LexV2Models.CreateBotLocale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a locale in the bot. The locale contains the intents and slot
-- types that the bot uses in conversations with users in the specified
-- language and locale. You must add a locale to a bot before you can add
-- intents and slot types to the bot.
module Amazonka.LexV2Models.CreateBotLocale
  ( -- * Creating a Request
    CreateBotLocale (..),
    newCreateBotLocale,

    -- * Request Lenses
    createBotLocale_description,
    createBotLocale_voiceSettings,
    createBotLocale_botId,
    createBotLocale_botVersion,
    createBotLocale_localeId,
    createBotLocale_nluIntentConfidenceThreshold,

    -- * Destructuring the Response
    CreateBotLocaleResponse (..),
    newCreateBotLocaleResponse,

    -- * Response Lenses
    createBotLocaleResponse_botId,
    createBotLocaleResponse_botLocaleStatus,
    createBotLocaleResponse_botVersion,
    createBotLocaleResponse_creationDateTime,
    createBotLocaleResponse_description,
    createBotLocaleResponse_localeId,
    createBotLocaleResponse_localeName,
    createBotLocaleResponse_nluIntentConfidenceThreshold,
    createBotLocaleResponse_voiceSettings,
    createBotLocaleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBotLocale' smart constructor.
data CreateBotLocale = CreateBotLocale'
  { -- | A description of the bot locale. Use this to help identify the bot
    -- locale in lists.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
    -- with the user.
    voiceSettings :: Prelude.Maybe VoiceSettings,
    -- | The identifier of the bot to create the locale for.
    botId :: Prelude.Text,
    -- | The version of the bot to create the locale for. This can only be the
    -- draft version of the bot.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that the bot will be used in.
    -- The string must match one of the supported locales. All of the intents,
    -- slot types, and slots used in the bot must have the same locale. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text,
    -- | Determines the threshold where Amazon Lex will insert the
    -- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
    -- returning alternative intents. @AMAZON.FallbackIntent@ and
    -- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
    -- the bot.
    --
    -- For example, suppose a bot is configured with the confidence threshold
    -- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
    -- alternative intents with the following confidence scores: IntentA
    -- (0.70), IntentB (0.60), IntentC (0.50). The response from the
    -- @RecognizeText@ operation would be:
    --
    -- -   AMAZON.FallbackIntent
    --
    -- -   IntentA
    --
    -- -   IntentB
    --
    -- -   IntentC
    nluIntentConfidenceThreshold :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotLocale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createBotLocale_description' - A description of the bot locale. Use this to help identify the bot
-- locale in lists.
--
-- 'voiceSettings', 'createBotLocale_voiceSettings' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user.
--
-- 'botId', 'createBotLocale_botId' - The identifier of the bot to create the locale for.
--
-- 'botVersion', 'createBotLocale_botVersion' - The version of the bot to create the locale for. This can only be the
-- draft version of the bot.
--
-- 'localeId', 'createBotLocale_localeId' - The identifier of the language and locale that the bot will be used in.
-- The string must match one of the supported locales. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
--
-- 'nluIntentConfidenceThreshold', 'createBotLocale_nluIntentConfidenceThreshold' - Determines the threshold where Amazon Lex will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents. @AMAZON.FallbackIntent@ and
-- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
-- the bot.
--
-- For example, suppose a bot is configured with the confidence threshold
-- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
-- alternative intents with the following confidence scores: IntentA
-- (0.70), IntentB (0.60), IntentC (0.50). The response from the
-- @RecognizeText@ operation would be:
--
-- -   AMAZON.FallbackIntent
--
-- -   IntentA
--
-- -   IntentB
--
-- -   IntentC
newCreateBotLocale ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  -- | 'nluIntentConfidenceThreshold'
  Prelude.Double ->
  CreateBotLocale
newCreateBotLocale
  pBotId_
  pBotVersion_
  pLocaleId_
  pNluIntentConfidenceThreshold_ =
    CreateBotLocale'
      { description = Prelude.Nothing,
        voiceSettings = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_,
        nluIntentConfidenceThreshold =
          pNluIntentConfidenceThreshold_
      }

-- | A description of the bot locale. Use this to help identify the bot
-- locale in lists.
createBotLocale_description :: Lens.Lens' CreateBotLocale (Prelude.Maybe Prelude.Text)
createBotLocale_description = Lens.lens (\CreateBotLocale' {description} -> description) (\s@CreateBotLocale' {} a -> s {description = a} :: CreateBotLocale)

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user.
createBotLocale_voiceSettings :: Lens.Lens' CreateBotLocale (Prelude.Maybe VoiceSettings)
createBotLocale_voiceSettings = Lens.lens (\CreateBotLocale' {voiceSettings} -> voiceSettings) (\s@CreateBotLocale' {} a -> s {voiceSettings = a} :: CreateBotLocale)

-- | The identifier of the bot to create the locale for.
createBotLocale_botId :: Lens.Lens' CreateBotLocale Prelude.Text
createBotLocale_botId = Lens.lens (\CreateBotLocale' {botId} -> botId) (\s@CreateBotLocale' {} a -> s {botId = a} :: CreateBotLocale)

-- | The version of the bot to create the locale for. This can only be the
-- draft version of the bot.
createBotLocale_botVersion :: Lens.Lens' CreateBotLocale Prelude.Text
createBotLocale_botVersion = Lens.lens (\CreateBotLocale' {botVersion} -> botVersion) (\s@CreateBotLocale' {} a -> s {botVersion = a} :: CreateBotLocale)

-- | The identifier of the language and locale that the bot will be used in.
-- The string must match one of the supported locales. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
createBotLocale_localeId :: Lens.Lens' CreateBotLocale Prelude.Text
createBotLocale_localeId = Lens.lens (\CreateBotLocale' {localeId} -> localeId) (\s@CreateBotLocale' {} a -> s {localeId = a} :: CreateBotLocale)

-- | Determines the threshold where Amazon Lex will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents. @AMAZON.FallbackIntent@ and
-- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
-- the bot.
--
-- For example, suppose a bot is configured with the confidence threshold
-- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
-- alternative intents with the following confidence scores: IntentA
-- (0.70), IntentB (0.60), IntentC (0.50). The response from the
-- @RecognizeText@ operation would be:
--
-- -   AMAZON.FallbackIntent
--
-- -   IntentA
--
-- -   IntentB
--
-- -   IntentC
createBotLocale_nluIntentConfidenceThreshold :: Lens.Lens' CreateBotLocale Prelude.Double
createBotLocale_nluIntentConfidenceThreshold = Lens.lens (\CreateBotLocale' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@CreateBotLocale' {} a -> s {nluIntentConfidenceThreshold = a} :: CreateBotLocale)

instance Core.AWSRequest CreateBotLocale where
  type
    AWSResponse CreateBotLocale =
      CreateBotLocaleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBotLocaleResponse'
            Prelude.<$> (x Data..?> "botId")
            Prelude.<*> (x Data..?> "botLocaleStatus")
            Prelude.<*> (x Data..?> "botVersion")
            Prelude.<*> (x Data..?> "creationDateTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "localeId")
            Prelude.<*> (x Data..?> "localeName")
            Prelude.<*> (x Data..?> "nluIntentConfidenceThreshold")
            Prelude.<*> (x Data..?> "voiceSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBotLocale where
  hashWithSalt _salt CreateBotLocale' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` voiceSettings
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId
      `Prelude.hashWithSalt` nluIntentConfidenceThreshold

instance Prelude.NFData CreateBotLocale where
  rnf CreateBotLocale' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf voiceSettings
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf nluIntentConfidenceThreshold

instance Data.ToHeaders CreateBotLocale where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBotLocale where
  toJSON CreateBotLocale' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("voiceSettings" Data..=) Prelude.<$> voiceSettings,
            Prelude.Just ("localeId" Data..= localeId),
            Prelude.Just
              ( "nluIntentConfidenceThreshold"
                  Data..= nluIntentConfidenceThreshold
              )
          ]
      )

instance Data.ToPath CreateBotLocale where
  toPath CreateBotLocale' {..} =
    Prelude.mconcat
      [ "/bots/",
        Data.toBS botId,
        "/botversions/",
        Data.toBS botVersion,
        "/botlocales/"
      ]

instance Data.ToQuery CreateBotLocale where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBotLocaleResponse' smart constructor.
data CreateBotLocaleResponse = CreateBotLocaleResponse'
  { -- | The specified bot identifier.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The status of the bot.
    --
    -- When the status is @Creating@ the bot locale is being configured. When
    -- the status is @Building@ Amazon Lex is building the bot for testing and
    -- use.
    --
    -- If the status of the bot is @ReadyExpressTesting@, you can test the bot
    -- using the exact utterances specified in the bots\' intents. When the bot
    -- is ready for full testing or to run, the status is @Built@.
    --
    -- If there was a problem with building the bot, the status is @Failed@. If
    -- the bot was saved but not built, the status is @NotBuilt@.
    botLocaleStatus :: Prelude.Maybe BotLocaleStatus,
    -- | The specified bot version.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp specifying the date and time that the bot locale was
    -- created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The specified description of the bot locale.
    description :: Prelude.Maybe Prelude.Text,
    -- | The specified locale identifier.
    localeId :: Prelude.Maybe Prelude.Text,
    -- | The specified locale name.
    localeName :: Prelude.Maybe Prelude.Text,
    -- | The specified confidence threshold for inserting the
    -- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents.
    nluIntentConfidenceThreshold :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
    -- with the user.
    voiceSettings :: Prelude.Maybe VoiceSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBotLocaleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botId', 'createBotLocaleResponse_botId' - The specified bot identifier.
--
-- 'botLocaleStatus', 'createBotLocaleResponse_botLocaleStatus' - The status of the bot.
--
-- When the status is @Creating@ the bot locale is being configured. When
-- the status is @Building@ Amazon Lex is building the bot for testing and
-- use.
--
-- If the status of the bot is @ReadyExpressTesting@, you can test the bot
-- using the exact utterances specified in the bots\' intents. When the bot
-- is ready for full testing or to run, the status is @Built@.
--
-- If there was a problem with building the bot, the status is @Failed@. If
-- the bot was saved but not built, the status is @NotBuilt@.
--
-- 'botVersion', 'createBotLocaleResponse_botVersion' - The specified bot version.
--
-- 'creationDateTime', 'createBotLocaleResponse_creationDateTime' - A timestamp specifying the date and time that the bot locale was
-- created.
--
-- 'description', 'createBotLocaleResponse_description' - The specified description of the bot locale.
--
-- 'localeId', 'createBotLocaleResponse_localeId' - The specified locale identifier.
--
-- 'localeName', 'createBotLocaleResponse_localeName' - The specified locale name.
--
-- 'nluIntentConfidenceThreshold', 'createBotLocaleResponse_nluIntentConfidenceThreshold' - The specified confidence threshold for inserting the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents.
--
-- 'voiceSettings', 'createBotLocaleResponse_voiceSettings' - The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user.
--
-- 'httpStatus', 'createBotLocaleResponse_httpStatus' - The response's http status code.
newCreateBotLocaleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBotLocaleResponse
newCreateBotLocaleResponse pHttpStatus_ =
  CreateBotLocaleResponse'
    { botId = Prelude.Nothing,
      botLocaleStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      localeId = Prelude.Nothing,
      localeName = Prelude.Nothing,
      nluIntentConfidenceThreshold = Prelude.Nothing,
      voiceSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The specified bot identifier.
createBotLocaleResponse_botId :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe Prelude.Text)
createBotLocaleResponse_botId = Lens.lens (\CreateBotLocaleResponse' {botId} -> botId) (\s@CreateBotLocaleResponse' {} a -> s {botId = a} :: CreateBotLocaleResponse)

-- | The status of the bot.
--
-- When the status is @Creating@ the bot locale is being configured. When
-- the status is @Building@ Amazon Lex is building the bot for testing and
-- use.
--
-- If the status of the bot is @ReadyExpressTesting@, you can test the bot
-- using the exact utterances specified in the bots\' intents. When the bot
-- is ready for full testing or to run, the status is @Built@.
--
-- If there was a problem with building the bot, the status is @Failed@. If
-- the bot was saved but not built, the status is @NotBuilt@.
createBotLocaleResponse_botLocaleStatus :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe BotLocaleStatus)
createBotLocaleResponse_botLocaleStatus = Lens.lens (\CreateBotLocaleResponse' {botLocaleStatus} -> botLocaleStatus) (\s@CreateBotLocaleResponse' {} a -> s {botLocaleStatus = a} :: CreateBotLocaleResponse)

-- | The specified bot version.
createBotLocaleResponse_botVersion :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe Prelude.Text)
createBotLocaleResponse_botVersion = Lens.lens (\CreateBotLocaleResponse' {botVersion} -> botVersion) (\s@CreateBotLocaleResponse' {} a -> s {botVersion = a} :: CreateBotLocaleResponse)

-- | A timestamp specifying the date and time that the bot locale was
-- created.
createBotLocaleResponse_creationDateTime :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe Prelude.UTCTime)
createBotLocaleResponse_creationDateTime = Lens.lens (\CreateBotLocaleResponse' {creationDateTime} -> creationDateTime) (\s@CreateBotLocaleResponse' {} a -> s {creationDateTime = a} :: CreateBotLocaleResponse) Prelude.. Lens.mapping Data._Time

-- | The specified description of the bot locale.
createBotLocaleResponse_description :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe Prelude.Text)
createBotLocaleResponse_description = Lens.lens (\CreateBotLocaleResponse' {description} -> description) (\s@CreateBotLocaleResponse' {} a -> s {description = a} :: CreateBotLocaleResponse)

-- | The specified locale identifier.
createBotLocaleResponse_localeId :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe Prelude.Text)
createBotLocaleResponse_localeId = Lens.lens (\CreateBotLocaleResponse' {localeId} -> localeId) (\s@CreateBotLocaleResponse' {} a -> s {localeId = a} :: CreateBotLocaleResponse)

-- | The specified locale name.
createBotLocaleResponse_localeName :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe Prelude.Text)
createBotLocaleResponse_localeName = Lens.lens (\CreateBotLocaleResponse' {localeName} -> localeName) (\s@CreateBotLocaleResponse' {} a -> s {localeName = a} :: CreateBotLocaleResponse)

-- | The specified confidence threshold for inserting the
-- @AMAZON.FallbackIntent@ and @AMAZON.KendraSearchIntent@ intents.
createBotLocaleResponse_nluIntentConfidenceThreshold :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe Prelude.Double)
createBotLocaleResponse_nluIntentConfidenceThreshold = Lens.lens (\CreateBotLocaleResponse' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@CreateBotLocaleResponse' {} a -> s {nluIntentConfidenceThreshold = a} :: CreateBotLocaleResponse)

-- | The Amazon Polly voice ID that Amazon Lex uses for voice interaction
-- with the user.
createBotLocaleResponse_voiceSettings :: Lens.Lens' CreateBotLocaleResponse (Prelude.Maybe VoiceSettings)
createBotLocaleResponse_voiceSettings = Lens.lens (\CreateBotLocaleResponse' {voiceSettings} -> voiceSettings) (\s@CreateBotLocaleResponse' {} a -> s {voiceSettings = a} :: CreateBotLocaleResponse)

-- | The response's http status code.
createBotLocaleResponse_httpStatus :: Lens.Lens' CreateBotLocaleResponse Prelude.Int
createBotLocaleResponse_httpStatus = Lens.lens (\CreateBotLocaleResponse' {httpStatus} -> httpStatus) (\s@CreateBotLocaleResponse' {} a -> s {httpStatus = a} :: CreateBotLocaleResponse)

instance Prelude.NFData CreateBotLocaleResponse where
  rnf CreateBotLocaleResponse' {..} =
    Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botLocaleStatus
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf localeId
      `Prelude.seq` Prelude.rnf localeName
      `Prelude.seq` Prelude.rnf nluIntentConfidenceThreshold
      `Prelude.seq` Prelude.rnf voiceSettings
      `Prelude.seq` Prelude.rnf httpStatus
