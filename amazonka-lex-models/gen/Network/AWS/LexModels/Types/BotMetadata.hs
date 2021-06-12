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
-- Module      : Network.AWS.LexModels.Types.BotMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.BotMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LexModels.Types.LexStatus

-- | Provides information about a bot. .
--
-- /See:/ 'newBotMetadata' smart constructor.
data BotMetadata = BotMetadata'
  { -- | The date that the bot was created.
    createdDate :: Core.Maybe Core.POSIX,
    -- | The status of the bot.
    status :: Core.Maybe LexStatus,
    -- | The date that the bot was updated. When you create a bot, the creation
    -- date and last updated date are the same.
    lastUpdatedDate :: Core.Maybe Core.POSIX,
    -- | The version of the bot. For a new bot, the version is always @$LATEST@.
    version :: Core.Maybe Core.Text,
    -- | The name of the bot.
    name :: Core.Maybe Core.Text,
    -- | A description of the bot.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BotMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'botMetadata_createdDate' - The date that the bot was created.
--
-- 'status', 'botMetadata_status' - The status of the bot.
--
-- 'lastUpdatedDate', 'botMetadata_lastUpdatedDate' - The date that the bot was updated. When you create a bot, the creation
-- date and last updated date are the same.
--
-- 'version', 'botMetadata_version' - The version of the bot. For a new bot, the version is always @$LATEST@.
--
-- 'name', 'botMetadata_name' - The name of the bot.
--
-- 'description', 'botMetadata_description' - A description of the bot.
newBotMetadata ::
  BotMetadata
newBotMetadata =
  BotMetadata'
    { createdDate = Core.Nothing,
      status = Core.Nothing,
      lastUpdatedDate = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing
    }

-- | The date that the bot was created.
botMetadata_createdDate :: Lens.Lens' BotMetadata (Core.Maybe Core.UTCTime)
botMetadata_createdDate = Lens.lens (\BotMetadata' {createdDate} -> createdDate) (\s@BotMetadata' {} a -> s {createdDate = a} :: BotMetadata) Core.. Lens.mapping Core._Time

-- | The status of the bot.
botMetadata_status :: Lens.Lens' BotMetadata (Core.Maybe LexStatus)
botMetadata_status = Lens.lens (\BotMetadata' {status} -> status) (\s@BotMetadata' {} a -> s {status = a} :: BotMetadata)

-- | The date that the bot was updated. When you create a bot, the creation
-- date and last updated date are the same.
botMetadata_lastUpdatedDate :: Lens.Lens' BotMetadata (Core.Maybe Core.UTCTime)
botMetadata_lastUpdatedDate = Lens.lens (\BotMetadata' {lastUpdatedDate} -> lastUpdatedDate) (\s@BotMetadata' {} a -> s {lastUpdatedDate = a} :: BotMetadata) Core.. Lens.mapping Core._Time

-- | The version of the bot. For a new bot, the version is always @$LATEST@.
botMetadata_version :: Lens.Lens' BotMetadata (Core.Maybe Core.Text)
botMetadata_version = Lens.lens (\BotMetadata' {version} -> version) (\s@BotMetadata' {} a -> s {version = a} :: BotMetadata)

-- | The name of the bot.
botMetadata_name :: Lens.Lens' BotMetadata (Core.Maybe Core.Text)
botMetadata_name = Lens.lens (\BotMetadata' {name} -> name) (\s@BotMetadata' {} a -> s {name = a} :: BotMetadata)

-- | A description of the bot.
botMetadata_description :: Lens.Lens' BotMetadata (Core.Maybe Core.Text)
botMetadata_description = Lens.lens (\BotMetadata' {description} -> description) (\s@BotMetadata' {} a -> s {description = a} :: BotMetadata)

instance Core.FromJSON BotMetadata where
  parseJSON =
    Core.withObject
      "BotMetadata"
      ( \x ->
          BotMetadata'
            Core.<$> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "lastUpdatedDate")
            Core.<*> (x Core..:? "version")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "description")
      )

instance Core.Hashable BotMetadata

instance Core.NFData BotMetadata
