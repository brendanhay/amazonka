{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.LexV2Models.Types.BotLocaleImportSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotLocaleImportSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.VoiceSettings
import qualified Amazonka.Prelude as Prelude

-- | Provides the bot locale parameters required for importing a bot locale.
--
-- /See:/ 'newBotLocaleImportSpecification' smart constructor.
data BotLocaleImportSpecification = BotLocaleImportSpecification'
  { -- | Determines the threshold where Amazon Lex will insert the
    -- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
    -- returning alternative intents. @AMAZON.FallbackIntent@ and
    -- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
    -- the bot.
    --
    -- For example, suppose a bot is configured with the confidence threshold
    -- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
    -- alternative intents with the following confidence scores: IntentA
    -- (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@
    -- operation would be:
    --
    -- -   @AMAZON.FallbackIntent@
    --
    -- -   @IntentA@
    --
    -- -   @IntentB@
    --
    -- -   @IntentC@
    nluIntentConfidenceThreshold :: Prelude.Maybe Prelude.Double,
    voiceSettings :: Prelude.Maybe VoiceSettings,
    -- | The identifier of the bot to import the locale to.
    botId :: Prelude.Text,
    -- | The version of the bot to import the locale to. This can only be the
    -- @DRAFT@ version of the bot.
    botVersion :: Prelude.Text,
    -- | The identifier of the language and locale that the bot will be used in.
    -- The string must match one of the supported locales. All of the intents,
    -- slot types, and slots used in the bot must have the same locale. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotLocaleImportSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nluIntentConfidenceThreshold', 'botLocaleImportSpecification_nluIntentConfidenceThreshold' - Determines the threshold where Amazon Lex will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents. @AMAZON.FallbackIntent@ and
-- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
-- the bot.
--
-- For example, suppose a bot is configured with the confidence threshold
-- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
-- alternative intents with the following confidence scores: IntentA
-- (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@
-- operation would be:
--
-- -   @AMAZON.FallbackIntent@
--
-- -   @IntentA@
--
-- -   @IntentB@
--
-- -   @IntentC@
--
-- 'voiceSettings', 'botLocaleImportSpecification_voiceSettings' - Undocumented member.
--
-- 'botId', 'botLocaleImportSpecification_botId' - The identifier of the bot to import the locale to.
--
-- 'botVersion', 'botLocaleImportSpecification_botVersion' - The version of the bot to import the locale to. This can only be the
-- @DRAFT@ version of the bot.
--
-- 'localeId', 'botLocaleImportSpecification_localeId' - The identifier of the language and locale that the bot will be used in.
-- The string must match one of the supported locales. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
newBotLocaleImportSpecification ::
  -- | 'botId'
  Prelude.Text ->
  -- | 'botVersion'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  BotLocaleImportSpecification
newBotLocaleImportSpecification
  pBotId_
  pBotVersion_
  pLocaleId_ =
    BotLocaleImportSpecification'
      { nluIntentConfidenceThreshold =
          Prelude.Nothing,
        voiceSettings = Prelude.Nothing,
        botId = pBotId_,
        botVersion = pBotVersion_,
        localeId = pLocaleId_
      }

-- | Determines the threshold where Amazon Lex will insert the
-- @AMAZON.FallbackIntent@, @AMAZON.KendraSearchIntent@, or both when
-- returning alternative intents. @AMAZON.FallbackIntent@ and
-- @AMAZON.KendraSearchIntent@ are only inserted if they are configured for
-- the bot.
--
-- For example, suppose a bot is configured with the confidence threshold
-- of 0.80 and the @AMAZON.FallbackIntent@. Amazon Lex returns three
-- alternative intents with the following confidence scores: IntentA
-- (0.70), IntentB (0.60), IntentC (0.50). The response from the @PostText@
-- operation would be:
--
-- -   @AMAZON.FallbackIntent@
--
-- -   @IntentA@
--
-- -   @IntentB@
--
-- -   @IntentC@
botLocaleImportSpecification_nluIntentConfidenceThreshold :: Lens.Lens' BotLocaleImportSpecification (Prelude.Maybe Prelude.Double)
botLocaleImportSpecification_nluIntentConfidenceThreshold = Lens.lens (\BotLocaleImportSpecification' {nluIntentConfidenceThreshold} -> nluIntentConfidenceThreshold) (\s@BotLocaleImportSpecification' {} a -> s {nluIntentConfidenceThreshold = a} :: BotLocaleImportSpecification)

-- | Undocumented member.
botLocaleImportSpecification_voiceSettings :: Lens.Lens' BotLocaleImportSpecification (Prelude.Maybe VoiceSettings)
botLocaleImportSpecification_voiceSettings = Lens.lens (\BotLocaleImportSpecification' {voiceSettings} -> voiceSettings) (\s@BotLocaleImportSpecification' {} a -> s {voiceSettings = a} :: BotLocaleImportSpecification)

-- | The identifier of the bot to import the locale to.
botLocaleImportSpecification_botId :: Lens.Lens' BotLocaleImportSpecification Prelude.Text
botLocaleImportSpecification_botId = Lens.lens (\BotLocaleImportSpecification' {botId} -> botId) (\s@BotLocaleImportSpecification' {} a -> s {botId = a} :: BotLocaleImportSpecification)

-- | The version of the bot to import the locale to. This can only be the
-- @DRAFT@ version of the bot.
botLocaleImportSpecification_botVersion :: Lens.Lens' BotLocaleImportSpecification Prelude.Text
botLocaleImportSpecification_botVersion = Lens.lens (\BotLocaleImportSpecification' {botVersion} -> botVersion) (\s@BotLocaleImportSpecification' {} a -> s {botVersion = a} :: BotLocaleImportSpecification)

-- | The identifier of the language and locale that the bot will be used in.
-- The string must match one of the supported locales. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>.
botLocaleImportSpecification_localeId :: Lens.Lens' BotLocaleImportSpecification Prelude.Text
botLocaleImportSpecification_localeId = Lens.lens (\BotLocaleImportSpecification' {localeId} -> localeId) (\s@BotLocaleImportSpecification' {} a -> s {localeId = a} :: BotLocaleImportSpecification)

instance Data.FromJSON BotLocaleImportSpecification where
  parseJSON =
    Data.withObject
      "BotLocaleImportSpecification"
      ( \x ->
          BotLocaleImportSpecification'
            Prelude.<$> (x Data..:? "nluIntentConfidenceThreshold")
            Prelude.<*> (x Data..:? "voiceSettings")
            Prelude.<*> (x Data..: "botId")
            Prelude.<*> (x Data..: "botVersion")
            Prelude.<*> (x Data..: "localeId")
      )

instance
  Prelude.Hashable
    BotLocaleImportSpecification
  where
  hashWithSalt _salt BotLocaleImportSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` nluIntentConfidenceThreshold
      `Prelude.hashWithSalt` voiceSettings
      `Prelude.hashWithSalt` botId
      `Prelude.hashWithSalt` botVersion
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData BotLocaleImportSpecification where
  rnf BotLocaleImportSpecification' {..} =
    Prelude.rnf nluIntentConfidenceThreshold
      `Prelude.seq` Prelude.rnf voiceSettings
      `Prelude.seq` Prelude.rnf botId
      `Prelude.seq` Prelude.rnf botVersion
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToJSON BotLocaleImportSpecification where
  toJSON BotLocaleImportSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nluIntentConfidenceThreshold" Data..=)
              Prelude.<$> nluIntentConfidenceThreshold,
            ("voiceSettings" Data..=) Prelude.<$> voiceSettings,
            Prelude.Just ("botId" Data..= botId),
            Prelude.Just ("botVersion" Data..= botVersion),
            Prelude.Just ("localeId" Data..= localeId)
          ]
      )
