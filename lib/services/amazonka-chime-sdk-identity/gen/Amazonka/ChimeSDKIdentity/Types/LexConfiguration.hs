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
-- Module      : Amazonka.ChimeSDKIdentity.Types.LexConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.LexConfiguration where

import Amazonka.ChimeSDKIdentity.Types.InvokedBy
import Amazonka.ChimeSDKIdentity.Types.RespondsTo
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for an Amazon Lex V2 bot.
--
-- /See:/ 'newLexConfiguration' smart constructor.
data LexConfiguration = LexConfiguration'
  { -- | Specifies the type of message that triggers a bot.
    invokedBy :: Prelude.Maybe InvokedBy,
    -- | __Deprecated__. Use @InvokedBy@ instead.
    --
    -- Determines whether the Amazon Lex V2 bot responds to all standard
    -- messages. Control messages are not supported.
    respondsTo :: Prelude.Maybe RespondsTo,
    -- | The name of the welcome intent configured in the Amazon Lex V2 bot.
    welcomeIntent :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Amazon Lex V2 bot\'s alias. The ARN uses this format:
    -- @arn:aws:lex:REGION:ACCOUNT:bot-alias\/MYBOTID\/MYBOTALIAS@
    lexBotAliasArn :: Prelude.Text,
    -- | Identifies the Amazon Lex V2 bot\'s language and locale. The string must
    -- match one of the supported locales in Amazon Lex V2. All of the intents,
    -- slot types, and slots used in the bot must have the same locale. For
    -- more information, see
    -- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
    -- in the /Amazon Lex V2 Developer Guide/.
    localeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LexConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invokedBy', 'lexConfiguration_invokedBy' - Specifies the type of message that triggers a bot.
--
-- 'respondsTo', 'lexConfiguration_respondsTo' - __Deprecated__. Use @InvokedBy@ instead.
--
-- Determines whether the Amazon Lex V2 bot responds to all standard
-- messages. Control messages are not supported.
--
-- 'welcomeIntent', 'lexConfiguration_welcomeIntent' - The name of the welcome intent configured in the Amazon Lex V2 bot.
--
-- 'lexBotAliasArn', 'lexConfiguration_lexBotAliasArn' - The ARN of the Amazon Lex V2 bot\'s alias. The ARN uses this format:
-- @arn:aws:lex:REGION:ACCOUNT:bot-alias\/MYBOTID\/MYBOTALIAS@
--
-- 'localeId', 'lexConfiguration_localeId' - Identifies the Amazon Lex V2 bot\'s language and locale. The string must
-- match one of the supported locales in Amazon Lex V2. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
-- in the /Amazon Lex V2 Developer Guide/.
newLexConfiguration ::
  -- | 'lexBotAliasArn'
  Prelude.Text ->
  -- | 'localeId'
  Prelude.Text ->
  LexConfiguration
newLexConfiguration pLexBotAliasArn_ pLocaleId_ =
  LexConfiguration'
    { invokedBy = Prelude.Nothing,
      respondsTo = Prelude.Nothing,
      welcomeIntent = Prelude.Nothing,
      lexBotAliasArn = pLexBotAliasArn_,
      localeId = pLocaleId_
    }

-- | Specifies the type of message that triggers a bot.
lexConfiguration_invokedBy :: Lens.Lens' LexConfiguration (Prelude.Maybe InvokedBy)
lexConfiguration_invokedBy = Lens.lens (\LexConfiguration' {invokedBy} -> invokedBy) (\s@LexConfiguration' {} a -> s {invokedBy = a} :: LexConfiguration)

-- | __Deprecated__. Use @InvokedBy@ instead.
--
-- Determines whether the Amazon Lex V2 bot responds to all standard
-- messages. Control messages are not supported.
lexConfiguration_respondsTo :: Lens.Lens' LexConfiguration (Prelude.Maybe RespondsTo)
lexConfiguration_respondsTo = Lens.lens (\LexConfiguration' {respondsTo} -> respondsTo) (\s@LexConfiguration' {} a -> s {respondsTo = a} :: LexConfiguration)

-- | The name of the welcome intent configured in the Amazon Lex V2 bot.
lexConfiguration_welcomeIntent :: Lens.Lens' LexConfiguration (Prelude.Maybe Prelude.Text)
lexConfiguration_welcomeIntent = Lens.lens (\LexConfiguration' {welcomeIntent} -> welcomeIntent) (\s@LexConfiguration' {} a -> s {welcomeIntent = a} :: LexConfiguration)

-- | The ARN of the Amazon Lex V2 bot\'s alias. The ARN uses this format:
-- @arn:aws:lex:REGION:ACCOUNT:bot-alias\/MYBOTID\/MYBOTALIAS@
lexConfiguration_lexBotAliasArn :: Lens.Lens' LexConfiguration Prelude.Text
lexConfiguration_lexBotAliasArn = Lens.lens (\LexConfiguration' {lexBotAliasArn} -> lexBotAliasArn) (\s@LexConfiguration' {} a -> s {lexBotAliasArn = a} :: LexConfiguration)

-- | Identifies the Amazon Lex V2 bot\'s language and locale. The string must
-- match one of the supported locales in Amazon Lex V2. All of the intents,
-- slot types, and slots used in the bot must have the same locale. For
-- more information, see
-- <https://docs.aws.amazon.com/lexv2/latest/dg/how-languages.html Supported languages>
-- in the /Amazon Lex V2 Developer Guide/.
lexConfiguration_localeId :: Lens.Lens' LexConfiguration Prelude.Text
lexConfiguration_localeId = Lens.lens (\LexConfiguration' {localeId} -> localeId) (\s@LexConfiguration' {} a -> s {localeId = a} :: LexConfiguration)

instance Data.FromJSON LexConfiguration where
  parseJSON =
    Data.withObject
      "LexConfiguration"
      ( \x ->
          LexConfiguration'
            Prelude.<$> (x Data..:? "InvokedBy")
            Prelude.<*> (x Data..:? "RespondsTo")
            Prelude.<*> (x Data..:? "WelcomeIntent")
            Prelude.<*> (x Data..: "LexBotAliasArn")
            Prelude.<*> (x Data..: "LocaleId")
      )

instance Prelude.Hashable LexConfiguration where
  hashWithSalt _salt LexConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` invokedBy
      `Prelude.hashWithSalt` respondsTo
      `Prelude.hashWithSalt` welcomeIntent
      `Prelude.hashWithSalt` lexBotAliasArn
      `Prelude.hashWithSalt` localeId

instance Prelude.NFData LexConfiguration where
  rnf LexConfiguration' {..} =
    Prelude.rnf invokedBy
      `Prelude.seq` Prelude.rnf respondsTo
      `Prelude.seq` Prelude.rnf welcomeIntent
      `Prelude.seq` Prelude.rnf lexBotAliasArn
      `Prelude.seq` Prelude.rnf localeId

instance Data.ToJSON LexConfiguration where
  toJSON LexConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InvokedBy" Data..=) Prelude.<$> invokedBy,
            ("RespondsTo" Data..=) Prelude.<$> respondsTo,
            ("WelcomeIntent" Data..=) Prelude.<$> welcomeIntent,
            Prelude.Just
              ("LexBotAliasArn" Data..= lexBotAliasArn),
            Prelude.Just ("LocaleId" Data..= localeId)
          ]
      )
