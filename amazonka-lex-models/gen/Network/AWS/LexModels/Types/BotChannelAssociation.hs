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
-- Module      : Network.AWS.LexModels.Types.BotChannelAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotChannelAssociation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.ChannelStatus
import Network.AWS.LexModels.Types.ChannelType

-- | Represents an association between an Amazon Lex bot and an external
-- messaging platform.
--
-- /See:/ 'newBotChannelAssociation' smart constructor.
data BotChannelAssociation = BotChannelAssociation'
  { -- | An alias pointing to the specific version of the Amazon Lex bot to which
    -- this association is being made.
    botAlias :: Core.Maybe Core.Text,
    -- | The date that the association between the Amazon Lex bot and the channel
    -- was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The status of the bot channel.
    --
    -- -   @CREATED@ - The channel has been created and is ready for use.
    --
    -- -   @IN_PROGRESS@ - Channel creation is in progress.
    --
    -- -   @FAILED@ - There was an error creating the channel. For information
    --     about the reason for the failure, see the @failureReason@ field.
    status :: Core.Maybe ChannelStatus,
    -- | Provides information necessary to communicate with the messaging
    -- platform.
    botConfiguration :: Core.Maybe (Core.Sensitive (Core.HashMap Core.Text Core.Text)),
    -- | The name of the Amazon Lex bot to which this association is being made.
    --
    -- Currently, Amazon Lex supports associations with Facebook and Slack, and
    -- Twilio.
    botName :: Core.Maybe Core.Text,
    -- | The name of the association between the bot and the channel.
    name :: Core.Maybe Core.Text,
    -- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
    -- to create the association.
    failureReason :: Core.Maybe Core.Text,
    -- | A text description of the association you are creating.
    description :: Core.Maybe Core.Text,
    -- | Specifies the type of association by indicating the type of channel
    -- being established between the Amazon Lex bot and the external messaging
    -- platform.
    type' :: Core.Maybe ChannelType
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'BotChannelAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAlias', 'botChannelAssociation_botAlias' - An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
--
-- 'createdDate', 'botChannelAssociation_createdDate' - The date that the association between the Amazon Lex bot and the channel
-- was created.
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
-- 'botConfiguration', 'botChannelAssociation_botConfiguration' - Provides information necessary to communicate with the messaging
-- platform.
--
-- 'botName', 'botChannelAssociation_botName' - The name of the Amazon Lex bot to which this association is being made.
--
-- Currently, Amazon Lex supports associations with Facebook and Slack, and
-- Twilio.
--
-- 'name', 'botChannelAssociation_name' - The name of the association between the bot and the channel.
--
-- 'failureReason', 'botChannelAssociation_failureReason' - If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
--
-- 'description', 'botChannelAssociation_description' - A text description of the association you are creating.
--
-- 'type'', 'botChannelAssociation_type' - Specifies the type of association by indicating the type of channel
-- being established between the Amazon Lex bot and the external messaging
-- platform.
newBotChannelAssociation ::
  BotChannelAssociation
newBotChannelAssociation =
  BotChannelAssociation'
    { botAlias = Core.Nothing,
      createdDate = Core.Nothing,
      status = Core.Nothing,
      botConfiguration = Core.Nothing,
      botName = Core.Nothing,
      name = Core.Nothing,
      failureReason = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing
    }

-- | An alias pointing to the specific version of the Amazon Lex bot to which
-- this association is being made.
botChannelAssociation_botAlias :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.Text)
botChannelAssociation_botAlias = Lens.lens (\BotChannelAssociation' {botAlias} -> botAlias) (\s@BotChannelAssociation' {} a -> s {botAlias = a} :: BotChannelAssociation)

-- | The date that the association between the Amazon Lex bot and the channel
-- was created.
botChannelAssociation_createdDate :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.UTCTime)
botChannelAssociation_createdDate = Lens.lens (\BotChannelAssociation' {createdDate} -> createdDate) (\s@BotChannelAssociation' {} a -> s {createdDate = a} :: BotChannelAssociation) Core.. Lens.mapping Core._Time

-- | The status of the bot channel.
--
-- -   @CREATED@ - The channel has been created and is ready for use.
--
-- -   @IN_PROGRESS@ - Channel creation is in progress.
--
-- -   @FAILED@ - There was an error creating the channel. For information
--     about the reason for the failure, see the @failureReason@ field.
botChannelAssociation_status :: Lens.Lens' BotChannelAssociation (Core.Maybe ChannelStatus)
botChannelAssociation_status = Lens.lens (\BotChannelAssociation' {status} -> status) (\s@BotChannelAssociation' {} a -> s {status = a} :: BotChannelAssociation)

-- | Provides information necessary to communicate with the messaging
-- platform.
botChannelAssociation_botConfiguration :: Lens.Lens' BotChannelAssociation (Core.Maybe (Core.HashMap Core.Text Core.Text))
botChannelAssociation_botConfiguration = Lens.lens (\BotChannelAssociation' {botConfiguration} -> botConfiguration) (\s@BotChannelAssociation' {} a -> s {botConfiguration = a} :: BotChannelAssociation) Core.. Lens.mapping (Core._Sensitive Core.. Lens._Coerce)

-- | The name of the Amazon Lex bot to which this association is being made.
--
-- Currently, Amazon Lex supports associations with Facebook and Slack, and
-- Twilio.
botChannelAssociation_botName :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.Text)
botChannelAssociation_botName = Lens.lens (\BotChannelAssociation' {botName} -> botName) (\s@BotChannelAssociation' {} a -> s {botName = a} :: BotChannelAssociation)

-- | The name of the association between the bot and the channel.
botChannelAssociation_name :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.Text)
botChannelAssociation_name = Lens.lens (\BotChannelAssociation' {name} -> name) (\s@BotChannelAssociation' {} a -> s {name = a} :: BotChannelAssociation)

-- | If @status@ is @FAILED@, Amazon Lex provides the reason that it failed
-- to create the association.
botChannelAssociation_failureReason :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.Text)
botChannelAssociation_failureReason = Lens.lens (\BotChannelAssociation' {failureReason} -> failureReason) (\s@BotChannelAssociation' {} a -> s {failureReason = a} :: BotChannelAssociation)

-- | A text description of the association you are creating.
botChannelAssociation_description :: Lens.Lens' BotChannelAssociation (Core.Maybe Core.Text)
botChannelAssociation_description = Lens.lens (\BotChannelAssociation' {description} -> description) (\s@BotChannelAssociation' {} a -> s {description = a} :: BotChannelAssociation)

-- | Specifies the type of association by indicating the type of channel
-- being established between the Amazon Lex bot and the external messaging
-- platform.
botChannelAssociation_type :: Lens.Lens' BotChannelAssociation (Core.Maybe ChannelType)
botChannelAssociation_type = Lens.lens (\BotChannelAssociation' {type'} -> type') (\s@BotChannelAssociation' {} a -> s {type' = a} :: BotChannelAssociation)

instance Core.FromJSON BotChannelAssociation where
  parseJSON =
    Core.withObject
      "BotChannelAssociation"
      ( \x ->
          BotChannelAssociation'
            Core.<$> (x Core..:? "botAlias")
            Core.<*> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "botConfiguration" Core..!= Core.mempty)
            Core.<*> (x Core..:? "botName")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "failureReason")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable BotChannelAssociation

instance Core.NFData BotChannelAssociation
