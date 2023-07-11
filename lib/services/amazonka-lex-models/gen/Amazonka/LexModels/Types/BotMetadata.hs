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
-- Module      : Amazonka.LexModels.Types.BotMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.BotMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.LexStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a bot. .
--
-- /See:/ 'newBotMetadata' smart constructor.
data BotMetadata = BotMetadata'
  { -- | The date that the bot was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the bot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date that the bot was updated. When you create a bot, the creation
    -- date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the bot.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the bot.
    status :: Prelude.Maybe LexStatus,
    -- | The version of the bot. For a new bot, the version is always @$LATEST@.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'description', 'botMetadata_description' - A description of the bot.
--
-- 'lastUpdatedDate', 'botMetadata_lastUpdatedDate' - The date that the bot was updated. When you create a bot, the creation
-- date and last updated date are the same.
--
-- 'name', 'botMetadata_name' - The name of the bot.
--
-- 'status', 'botMetadata_status' - The status of the bot.
--
-- 'version', 'botMetadata_version' - The version of the bot. For a new bot, the version is always @$LATEST@.
newBotMetadata ::
  BotMetadata
newBotMetadata =
  BotMetadata'
    { createdDate = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The date that the bot was created.
botMetadata_createdDate :: Lens.Lens' BotMetadata (Prelude.Maybe Prelude.UTCTime)
botMetadata_createdDate = Lens.lens (\BotMetadata' {createdDate} -> createdDate) (\s@BotMetadata' {} a -> s {createdDate = a} :: BotMetadata) Prelude.. Lens.mapping Data._Time

-- | A description of the bot.
botMetadata_description :: Lens.Lens' BotMetadata (Prelude.Maybe Prelude.Text)
botMetadata_description = Lens.lens (\BotMetadata' {description} -> description) (\s@BotMetadata' {} a -> s {description = a} :: BotMetadata)

-- | The date that the bot was updated. When you create a bot, the creation
-- date and last updated date are the same.
botMetadata_lastUpdatedDate :: Lens.Lens' BotMetadata (Prelude.Maybe Prelude.UTCTime)
botMetadata_lastUpdatedDate = Lens.lens (\BotMetadata' {lastUpdatedDate} -> lastUpdatedDate) (\s@BotMetadata' {} a -> s {lastUpdatedDate = a} :: BotMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the bot.
botMetadata_name :: Lens.Lens' BotMetadata (Prelude.Maybe Prelude.Text)
botMetadata_name = Lens.lens (\BotMetadata' {name} -> name) (\s@BotMetadata' {} a -> s {name = a} :: BotMetadata)

-- | The status of the bot.
botMetadata_status :: Lens.Lens' BotMetadata (Prelude.Maybe LexStatus)
botMetadata_status = Lens.lens (\BotMetadata' {status} -> status) (\s@BotMetadata' {} a -> s {status = a} :: BotMetadata)

-- | The version of the bot. For a new bot, the version is always @$LATEST@.
botMetadata_version :: Lens.Lens' BotMetadata (Prelude.Maybe Prelude.Text)
botMetadata_version = Lens.lens (\BotMetadata' {version} -> version) (\s@BotMetadata' {} a -> s {version = a} :: BotMetadata)

instance Data.FromJSON BotMetadata where
  parseJSON =
    Data.withObject
      "BotMetadata"
      ( \x ->
          BotMetadata'
            Prelude.<$> (x Data..:? "createdDate")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedDate")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable BotMetadata where
  hashWithSalt _salt BotMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` version

instance Prelude.NFData BotMetadata where
  rnf BotMetadata' {..} =
    Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
