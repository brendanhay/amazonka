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
-- Module      : Amazonka.LexModels.Types.BotChannelAssociation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.BotChannelAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexModels.Types.ChannelStatus
import Amazonka.LexModels.Types.ChannelType
import qualified Amazonka.Prelude as Prelude

-- | Represents an association between an Amazon Lex bot and an external
-- messaging platform.
--
-- /See:/ 'newBotChannelAssociation' smart constructor.
data BotChannelAssociation = BotChannelAssociation'
  { -- | The name of the association between the bot and the channel.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of association by indicating the type of channel
    -- being established between the Amazon Lex bot and the external messaging
    -- platform.
    type' :: Prelude.Maybe ChannelType,
    -- | Provides information necessary to communicate with the messaging
    -- platform.
    botConfiguration :: Prelude.Maybe (Core.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The status of the bot channel.
    --
    -- -   @CREATED@ - The channel has been created and is ready for use.
    --
    -- -   @IN_PROGRESS@ - Channel creation is in progress.
    --
    -- -   @FAILED@ - There was an error creating the channel. For information
    --     about the reason for the failure, see the @failureReason@ field.
    status :: Prelude.Maybe ChannelStatus,
    -- | A text description of the association you are creating.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon Lex bot to which this association is being made.
    --
    -- Currently, Amazon Lex supports associations with Facebook and Slack, and
    -- Twilio.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The date that the association between the Amazon Lex bot and the channel
    -- was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | An alias pointing to the specific version of the Amazon Lex bot to which
    -- this association is being made.
    botAlias :: Prelude.Maybe Prelude.Text,
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to create the association.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotChannelAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'botChannelAssociation_name' - The name of the association between the bot and the channel.
--
-- 'type'', 'botChannelAssociation_type' - Specifies the type of association by indicating the type of channel
-- being established between the Amazon Lex bot and the external messaging
-- platform.
--
-- 'botConfiguration', 'botChannelAssociation_botConfiguration' - Provides information necessary to communicate with the messaging
-- platform.
--
-- 'status', 'botChannelAssociation_status' - The status of the bot channel.
--
-- -   @CREATED@ - The channel has been created and is ready for use.
--
-- -   @IN_PROGRESS@ - Channel creation is in progress.
--
-- -   @FAILED@ - There was an error creating the channel. For information
--     about the reason for the failure, see the @failureReason@ field.
--
-- 'description', 'botChannelAssociation_description' - A text description of the association you are creating.
--
-- 'botName', 'botChannelAssociation_botName' - The name of the Amazon Lex bot to which this association is being made.
--
-- Currently, Amazon Lex supports associations with Facebook and Slack, and
-- Twilio.
--
-- 'createdDate', 'botChannelAssociation_createdDate' - The date that the association between the Amazon Lex bot and the channel
-- was created.
--
-- 'botAlias', 'botChannelAssociation_botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
--
-- 'failureReason', 'botChannelAssociation_failureReason' - If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
newBotChannelAssociation ::
  BotChannelAssociation
newBotChannelAssociation =
  BotChannelAssociation'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      botConfiguration = Prelude.Nothing,
      status = Prelude.Nothing,
      description = Prelude.Nothing,
      botName = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      botAlias = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | The name of the association between the bot and the channel.
botChannelAssociation_name :: Lens.Lens' BotChannelAssociation (Prelude.Maybe Prelude.Text)
botChannelAssociation_name = Lens.lens (\BotChannelAssociation' {name} -> name) (\s@BotChannelAssociation' {} a -> s {name = a} :: BotChannelAssociation)

-- | Specifies the type of association by indicating the type of channel
-- being established between the Amazon Lex bot and the external messaging
-- platform.
botChannelAssociation_type :: Lens.Lens' BotChannelAssociation (Prelude.Maybe ChannelType)
botChannelAssociation_type = Lens.lens (\BotChannelAssociation' {type'} -> type') (\s@BotChannelAssociation' {} a -> s {type' = a} :: BotChannelAssociation)

-- | Provides information necessary to communicate with the messaging
-- platform.
botChannelAssociation_botConfiguration :: Lens.Lens' BotChannelAssociation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
botChannelAssociation_botConfiguration = Lens.lens (\BotChannelAssociation' {botConfiguration} -> botConfiguration) (\s@BotChannelAssociation' {} a -> s {botConfiguration = a} :: BotChannelAssociation) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens.coerced)

-- | The status of the bot channel.
--
-- -   @CREATED@ - The channel has been created and is ready for use.
--
-- -   @IN_PROGRESS@ - Channel creation is in progress.
--
-- -   @FAILED@ - There was an error creating the channel. For information
--     about the reason for the failure, see the @failureReason@ field.
botChannelAssociation_status :: Lens.Lens' BotChannelAssociation (Prelude.Maybe ChannelStatus)
botChannelAssociation_status = Lens.lens (\BotChannelAssociation' {status} -> status) (\s@BotChannelAssociation' {} a -> s {status = a} :: BotChannelAssociation)

-- | A text description of the association you are creating.
botChannelAssociation_description :: Lens.Lens' BotChannelAssociation (Prelude.Maybe Prelude.Text)
botChannelAssociation_description = Lens.lens (\BotChannelAssociation' {description} -> description) (\s@BotChannelAssociation' {} a -> s {description = a} :: BotChannelAssociation)

-- | The name of the Amazon Lex bot to which this association is being made.
--
-- Currently, Amazon Lex supports associations with Facebook and Slack, and
-- Twilio.
botChannelAssociation_botName :: Lens.Lens' BotChannelAssociation (Prelude.Maybe Prelude.Text)
botChannelAssociation_botName = Lens.lens (\BotChannelAssociation' {botName} -> botName) (\s@BotChannelAssociation' {} a -> s {botName = a} :: BotChannelAssociation)

-- | The date that the association between the Amazon Lex bot and the channel
-- was created.
botChannelAssociation_createdDate :: Lens.Lens' BotChannelAssociation (Prelude.Maybe Prelude.UTCTime)
botChannelAssociation_createdDate = Lens.lens (\BotChannelAssociation' {createdDate} -> createdDate) (\s@BotChannelAssociation' {} a -> s {createdDate = a} :: BotChannelAssociation) Prelude.. Lens.mapping Core._Time

-- | An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
botChannelAssociation_botAlias :: Lens.Lens' BotChannelAssociation (Prelude.Maybe Prelude.Text)
botChannelAssociation_botAlias = Lens.lens (\BotChannelAssociation' {botAlias} -> botAlias) (\s@BotChannelAssociation' {} a -> s {botAlias = a} :: BotChannelAssociation)

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
botChannelAssociation_failureReason :: Lens.Lens' BotChannelAssociation (Prelude.Maybe Prelude.Text)
botChannelAssociation_failureReason = Lens.lens (\BotChannelAssociation' {failureReason} -> failureReason) (\s@BotChannelAssociation' {} a -> s {failureReason = a} :: BotChannelAssociation)

instance Core.FromJSON BotChannelAssociation where
  parseJSON =
    Core.withObject
      "BotChannelAssociation"
      ( \x ->
          BotChannelAssociation'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> ( x Core..:? "botConfiguration"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "botName")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "botAlias")
            Prelude.<*> (x Core..:? "failureReason")
      )

instance Prelude.Hashable BotChannelAssociation where
  hashWithSalt _salt BotChannelAssociation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` botConfiguration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` botName
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` botAlias
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData BotChannelAssociation where
  rnf BotChannelAssociation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf botConfiguration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf botName
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf botAlias
      `Prelude.seq` Prelude.rnf failureReason
