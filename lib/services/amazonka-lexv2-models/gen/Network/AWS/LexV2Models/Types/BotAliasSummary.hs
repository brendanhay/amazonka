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
-- Module      : Network.AWS.LexV2Models.Types.BotAliasSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.BotAliasSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexV2Models.Types.BotAliasStatus
import qualified Network.AWS.Prelude as Prelude

-- | Summary information about bot aliases returned from the ListBotAliases
-- operation.
--
-- /See:/ 'newBotAliasSummary' smart constructor.
data BotAliasSummary = BotAliasSummary'
  { -- | The current state of the bot alias. If the status is @Available@, the
    -- alias is ready for use.
    botAliasStatus :: Prelude.Maybe BotAliasStatus,
    -- | The version of the bot that the bot alias references.
    botVersion :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot alias was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The unique identifier assigned to the bot alias. You can use this ID to
    -- get detailed information about the alias using the DescribeBotAlias
    -- operation.
    botAliasId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the bot alias was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the bot alias.
    botAliasName :: Prelude.Maybe Prelude.Text,
    -- | The description of the bot alias.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BotAliasSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'botAliasStatus', 'botAliasSummary_botAliasStatus' - The current state of the bot alias. If the status is @Available@, the
-- alias is ready for use.
--
-- 'botVersion', 'botAliasSummary_botVersion' - The version of the bot that the bot alias references.
--
-- 'lastUpdatedDateTime', 'botAliasSummary_lastUpdatedDateTime' - A timestamp of the date and time that the bot alias was last updated.
--
-- 'botAliasId', 'botAliasSummary_botAliasId' - The unique identifier assigned to the bot alias. You can use this ID to
-- get detailed information about the alias using the DescribeBotAlias
-- operation.
--
-- 'creationDateTime', 'botAliasSummary_creationDateTime' - A timestamp of the date and time that the bot alias was created.
--
-- 'botAliasName', 'botAliasSummary_botAliasName' - The name of the bot alias.
--
-- 'description', 'botAliasSummary_description' - The description of the bot alias.
newBotAliasSummary ::
  BotAliasSummary
newBotAliasSummary =
  BotAliasSummary'
    { botAliasStatus = Prelude.Nothing,
      botVersion = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      botAliasId = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      botAliasName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The current state of the bot alias. If the status is @Available@, the
-- alias is ready for use.
botAliasSummary_botAliasStatus :: Lens.Lens' BotAliasSummary (Prelude.Maybe BotAliasStatus)
botAliasSummary_botAliasStatus = Lens.lens (\BotAliasSummary' {botAliasStatus} -> botAliasStatus) (\s@BotAliasSummary' {} a -> s {botAliasStatus = a} :: BotAliasSummary)

-- | The version of the bot that the bot alias references.
botAliasSummary_botVersion :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_botVersion = Lens.lens (\BotAliasSummary' {botVersion} -> botVersion) (\s@BotAliasSummary' {} a -> s {botVersion = a} :: BotAliasSummary)

-- | A timestamp of the date and time that the bot alias was last updated.
botAliasSummary_lastUpdatedDateTime :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.UTCTime)
botAliasSummary_lastUpdatedDateTime = Lens.lens (\BotAliasSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@BotAliasSummary' {} a -> s {lastUpdatedDateTime = a} :: BotAliasSummary) Prelude.. Lens.mapping Core._Time

-- | The unique identifier assigned to the bot alias. You can use this ID to
-- get detailed information about the alias using the DescribeBotAlias
-- operation.
botAliasSummary_botAliasId :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_botAliasId = Lens.lens (\BotAliasSummary' {botAliasId} -> botAliasId) (\s@BotAliasSummary' {} a -> s {botAliasId = a} :: BotAliasSummary)

-- | A timestamp of the date and time that the bot alias was created.
botAliasSummary_creationDateTime :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.UTCTime)
botAliasSummary_creationDateTime = Lens.lens (\BotAliasSummary' {creationDateTime} -> creationDateTime) (\s@BotAliasSummary' {} a -> s {creationDateTime = a} :: BotAliasSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the bot alias.
botAliasSummary_botAliasName :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_botAliasName = Lens.lens (\BotAliasSummary' {botAliasName} -> botAliasName) (\s@BotAliasSummary' {} a -> s {botAliasName = a} :: BotAliasSummary)

-- | The description of the bot alias.
botAliasSummary_description :: Lens.Lens' BotAliasSummary (Prelude.Maybe Prelude.Text)
botAliasSummary_description = Lens.lens (\BotAliasSummary' {description} -> description) (\s@BotAliasSummary' {} a -> s {description = a} :: BotAliasSummary)

instance Core.FromJSON BotAliasSummary where
  parseJSON =
    Core.withObject
      "BotAliasSummary"
      ( \x ->
          BotAliasSummary'
            Prelude.<$> (x Core..:? "botAliasStatus")
            Prelude.<*> (x Core..:? "botVersion")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "botAliasId")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "botAliasName")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable BotAliasSummary

instance Prelude.NFData BotAliasSummary
