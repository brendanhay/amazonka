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
-- Module      : Network.AWS.LexV2Models.Types.BotSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.BotSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.BotStatus
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about a bot returned by the ListBots operation.
--
-- /See:/ 'newBotSummary' smart constructor.
data BotSummary = BotSummary'
  { -- | The current status of the bot. When the status is @Available@ the bot is
    -- ready for use.
    botStatus :: Prelude.Maybe BotStatus,
    -- | The name of the bot.
    botName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the bot was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier assigned to the bot. Use this ID to get detailed
    -- information about the bot with the DescribeBot operation.
    botId :: Prelude.Maybe Prelude.Text,
    -- | The latest numerical version in use for the bot.
    latestBotVersion :: Prelude.Maybe Prelude.Text,
    -- | The description of the bot.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botStatus', 'botSummary_botStatus' - The current status of the bot. When the status is @Available@ the bot is
-- ready for use.
--
-- 'botName', 'botSummary_botName' - The name of the bot.
--
-- 'lastUpdatedDateTime', 'botSummary_lastUpdatedDateTime' - The date and time that the bot was last updated.
--
-- 'botId', 'botSummary_botId' - The unique identifier assigned to the bot. Use this ID to get detailed
-- information about the bot with the DescribeBot operation.
--
-- 'latestBotVersion', 'botSummary_latestBotVersion' - The latest numerical version in use for the bot.
--
-- 'description', 'botSummary_description' - The description of the bot.
newBotSummary ::
  BotSummary
newBotSummary =
  BotSummary'
    { botStatus = Prelude.Nothing,
      botName = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      botId = Prelude.Nothing,
      latestBotVersion = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The current status of the bot. When the status is @Available@ the bot is
-- ready for use.
botSummary_botStatus :: Lens.Lens' BotSummary (Prelude.Maybe BotStatus)
botSummary_botStatus = Lens.lens (\BotSummary' {botStatus} -> botStatus) (\s@BotSummary' {} a -> s {botStatus = a} :: BotSummary)

-- | The name of the bot.
botSummary_botName :: Lens.Lens' BotSummary (Prelude.Maybe Prelude.Text)
botSummary_botName = Lens.lens (\BotSummary' {botName} -> botName) (\s@BotSummary' {} a -> s {botName = a} :: BotSummary)

-- | The date and time that the bot was last updated.
botSummary_lastUpdatedDateTime :: Lens.Lens' BotSummary (Prelude.Maybe Prelude.UTCTime)
botSummary_lastUpdatedDateTime = Lens.lens (\BotSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BotSummary' {} a -> s {lastUpdatedDateTime = a} :: BotSummary) Prelude.. Lens.mapping Core._Time

-- | The unique identifier assigned to the bot. Use this ID to get detailed
-- information about the bot with the DescribeBot operation.
botSummary_botId :: Lens.Lens' BotSummary (Prelude.Maybe Prelude.Text)
botSummary_botId = Lens.lens (\BotSummary' {botId} -> botId) (\s@BotSummary' {} a -> s {botId = a} :: BotSummary)

-- | The latest numerical version in use for the bot.
botSummary_latestBotVersion :: Lens.Lens' BotSummary (Prelude.Maybe Prelude.Text)
botSummary_latestBotVersion = Lens.lens (\BotSummary' {latestBotVersion} -> latestBotVersion) (\s@BotSummary' {} a -> s {latestBotVersion = a} :: BotSummary)

-- | The description of the bot.
botSummary_description :: Lens.Lens' BotSummary (Prelude.Maybe Prelude.Text)
botSummary_description = Lens.lens (\BotSummary' {description} -> description) (\s@BotSummary' {} a -> s {description = a} :: BotSummary)

instance Core.FromJSON BotSummary where
  parseJSON =
    Core.withObject
      "BotSummary"
      ( \x ->
          BotSummary'
            Prelude.<$> (x Core..:? "botStatus")
            Prelude.<*> (x Core..:? "botName")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "botId")
            Prelude.<*> (x Core..:? "latestBotVersion")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable BotSummary

instance Prelude.NFData BotSummary
