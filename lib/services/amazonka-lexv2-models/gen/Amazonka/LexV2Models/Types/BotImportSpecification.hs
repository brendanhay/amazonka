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
-- Module      : Amazonka.LexV2Models.Types.BotImportSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BotImportSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.DataPrivacy
import qualified Amazonka.Prelude as Prelude

-- | Provides the bot parameters required for importing a bot.
--
-- /See:/ 'newBotImportSpecification' smart constructor.
data BotImportSpecification = BotImportSpecification'
  { -- | A list of tags to add to the bot. You can only add tags when you import
    -- a bot. You can\'t use the @UpdateBot@ operation to update tags. To
    -- update tags, use the @TagResource@ operation.
    botTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The time, in seconds, that Amazon Lex should keep information about a
    -- user\'s conversation with the bot.
    --
    -- A user interaction remains active for the amount of time specified. If
    -- no conversation occurs during this time, the session expires and Amazon
    -- Lex deletes any data provided before the timeout.
    --
    -- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
    idleSessionTTLInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | A list of tags to add to the test alias for a bot. You can only add tags
    -- when you import a bot. You can\'t use the @UpdateAlias@ operation to
    -- update tags. To update tags on the test alias, use the @TagResource@
    -- operation.
    testBotAliasTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name that Amazon Lex should use for the bot.
    botName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role used to build and run the
    -- bot.
    roleArn :: Prelude.Text,
    dataPrivacy :: DataPrivacy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotImportSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botTags', 'botImportSpecification_botTags' - A list of tags to add to the bot. You can only add tags when you import
-- a bot. You can\'t use the @UpdateBot@ operation to update tags. To
-- update tags, use the @TagResource@ operation.
--
-- 'idleSessionTTLInSeconds', 'botImportSpecification_idleSessionTTLInSeconds' - The time, in seconds, that Amazon Lex should keep information about a
-- user\'s conversation with the bot.
--
-- A user interaction remains active for the amount of time specified. If
-- no conversation occurs during this time, the session expires and Amazon
-- Lex deletes any data provided before the timeout.
--
-- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
--
-- 'testBotAliasTags', 'botImportSpecification_testBotAliasTags' - A list of tags to add to the test alias for a bot. You can only add tags
-- when you import a bot. You can\'t use the @UpdateAlias@ operation to
-- update tags. To update tags on the test alias, use the @TagResource@
-- operation.
--
-- 'botName', 'botImportSpecification_botName' - The name that Amazon Lex should use for the bot.
--
-- 'roleArn', 'botImportSpecification_roleArn' - The Amazon Resource Name (ARN) of the IAM role used to build and run the
-- bot.
--
-- 'dataPrivacy', 'botImportSpecification_dataPrivacy' - Undocumented member.
newBotImportSpecification ::
  -- | 'botName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'dataPrivacy'
  DataPrivacy ->
  BotImportSpecification
newBotImportSpecification
  pBotName_
  pRoleArn_
  pDataPrivacy_ =
    BotImportSpecification'
      { botTags = Prelude.Nothing,
        idleSessionTTLInSeconds = Prelude.Nothing,
        testBotAliasTags = Prelude.Nothing,
        botName = pBotName_,
        roleArn = pRoleArn_,
        dataPrivacy = pDataPrivacy_
      }

-- | A list of tags to add to the bot. You can only add tags when you import
-- a bot. You can\'t use the @UpdateBot@ operation to update tags. To
-- update tags, use the @TagResource@ operation.
botImportSpecification_botTags :: Lens.Lens' BotImportSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
botImportSpecification_botTags = Lens.lens (\BotImportSpecification' {botTags} -> botTags) (\s@BotImportSpecification' {} a -> s {botTags = a} :: BotImportSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The time, in seconds, that Amazon Lex should keep information about a
-- user\'s conversation with the bot.
--
-- A user interaction remains active for the amount of time specified. If
-- no conversation occurs during this time, the session expires and Amazon
-- Lex deletes any data provided before the timeout.
--
-- You can specify between 60 (1 minute) and 86,400 (24 hours) seconds.
botImportSpecification_idleSessionTTLInSeconds :: Lens.Lens' BotImportSpecification (Prelude.Maybe Prelude.Natural)
botImportSpecification_idleSessionTTLInSeconds = Lens.lens (\BotImportSpecification' {idleSessionTTLInSeconds} -> idleSessionTTLInSeconds) (\s@BotImportSpecification' {} a -> s {idleSessionTTLInSeconds = a} :: BotImportSpecification)

-- | A list of tags to add to the test alias for a bot. You can only add tags
-- when you import a bot. You can\'t use the @UpdateAlias@ operation to
-- update tags. To update tags on the test alias, use the @TagResource@
-- operation.
botImportSpecification_testBotAliasTags :: Lens.Lens' BotImportSpecification (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
botImportSpecification_testBotAliasTags = Lens.lens (\BotImportSpecification' {testBotAliasTags} -> testBotAliasTags) (\s@BotImportSpecification' {} a -> s {testBotAliasTags = a} :: BotImportSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The name that Amazon Lex should use for the bot.
botImportSpecification_botName :: Lens.Lens' BotImportSpecification Prelude.Text
botImportSpecification_botName = Lens.lens (\BotImportSpecification' {botName} -> botName) (\s@BotImportSpecification' {} a -> s {botName = a} :: BotImportSpecification)

-- | The Amazon Resource Name (ARN) of the IAM role used to build and run the
-- bot.
botImportSpecification_roleArn :: Lens.Lens' BotImportSpecification Prelude.Text
botImportSpecification_roleArn = Lens.lens (\BotImportSpecification' {roleArn} -> roleArn) (\s@BotImportSpecification' {} a -> s {roleArn = a} :: BotImportSpecification)

-- | Undocumented member.
botImportSpecification_dataPrivacy :: Lens.Lens' BotImportSpecification DataPrivacy
botImportSpecification_dataPrivacy = Lens.lens (\BotImportSpecification' {dataPrivacy} -> dataPrivacy) (\s@BotImportSpecification' {} a -> s {dataPrivacy = a} :: BotImportSpecification)

instance Data.FromJSON BotImportSpecification where
  parseJSON =
    Data.withObject
      "BotImportSpecification"
      ( \x ->
          BotImportSpecification'
            Prelude.<$> (x Data..:? "botTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "idleSessionTTLInSeconds")
            Prelude.<*> ( x
                            Data..:? "testBotAliasTags"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "botName")
            Prelude.<*> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "dataPrivacy")
      )

instance Prelude.Hashable BotImportSpecification where
  hashWithSalt _salt BotImportSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` botTags
      `Prelude.hashWithSalt` idleSessionTTLInSeconds
      `Prelude.hashWithSalt` testBotAliasTags
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` dataPrivacy

instance Prelude.NFData BotImportSpecification where
  rnf BotImportSpecification' {..} =
    Prelude.rnf botTags `Prelude.seq`
      Prelude.rnf idleSessionTTLInSeconds `Prelude.seq`
        Prelude.rnf testBotAliasTags `Prelude.seq`
          Prelude.rnf botName `Prelude.seq`
            Prelude.rnf roleArn `Prelude.seq`
              Prelude.rnf dataPrivacy

instance Data.ToJSON BotImportSpecification where
  toJSON BotImportSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("botTags" Data..=) Prelude.<$> botTags,
            ("idleSessionTTLInSeconds" Data..=)
              Prelude.<$> idleSessionTTLInSeconds,
            ("testBotAliasTags" Data..=)
              Prelude.<$> testBotAliasTags,
            Prelude.Just ("botName" Data..= botName),
            Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("dataPrivacy" Data..= dataPrivacy)
          ]
      )
