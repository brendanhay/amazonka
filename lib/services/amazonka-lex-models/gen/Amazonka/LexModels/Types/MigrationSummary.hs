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
-- Module      : Amazonka.LexModels.Types.MigrationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.MigrationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.Locale
import Amazonka.LexModels.Types.MigrationStatus
import Amazonka.LexModels.Types.MigrationStrategy
import qualified Amazonka.Prelude as Prelude

-- | Provides information about migrating a bot from Amazon Lex V1 to Amazon
-- Lex V2.
--
-- /See:/ 'newMigrationSummary' smart constructor.
data MigrationSummary = MigrationSummary'
  { -- | The unique identifier that Amazon Lex assigned to the migration.
    migrationId :: Prelude.Maybe Prelude.Text,
    -- | The status of the operation. When the status is @COMPLETE@ the bot is
    -- available in Amazon Lex V2. There may be alerts and warnings that need
    -- to be resolved to complete the migration.
    migrationStatus :: Prelude.Maybe MigrationStatus,
    -- | The strategy used to conduct the migration.
    migrationStrategy :: Prelude.Maybe MigrationStrategy,
    -- | The date and time that the migration started.
    migrationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The locale of the Amazon Lex V1 bot that is the source of the migration.
    v1BotLocale :: Prelude.Maybe Locale,
    -- | The name of the Amazon Lex V1 bot that is the source of the migration.
    v1BotName :: Prelude.Maybe Prelude.Text,
    -- | The version of the Amazon Lex V1 bot that is the source of the
    -- migration.
    v1BotVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the Amazon Lex V2 that is the destination of
    -- the migration.
    v2BotId :: Prelude.Maybe Prelude.Text,
    -- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
    v2BotRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MigrationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'migrationId', 'migrationSummary_migrationId' - The unique identifier that Amazon Lex assigned to the migration.
--
-- 'migrationStatus', 'migrationSummary_migrationStatus' - The status of the operation. When the status is @COMPLETE@ the bot is
-- available in Amazon Lex V2. There may be alerts and warnings that need
-- to be resolved to complete the migration.
--
-- 'migrationStrategy', 'migrationSummary_migrationStrategy' - The strategy used to conduct the migration.
--
-- 'migrationTimestamp', 'migrationSummary_migrationTimestamp' - The date and time that the migration started.
--
-- 'v1BotLocale', 'migrationSummary_v1BotLocale' - The locale of the Amazon Lex V1 bot that is the source of the migration.
--
-- 'v1BotName', 'migrationSummary_v1BotName' - The name of the Amazon Lex V1 bot that is the source of the migration.
--
-- 'v1BotVersion', 'migrationSummary_v1BotVersion' - The version of the Amazon Lex V1 bot that is the source of the
-- migration.
--
-- 'v2BotId', 'migrationSummary_v2BotId' - The unique identifier of the Amazon Lex V2 that is the destination of
-- the migration.
--
-- 'v2BotRole', 'migrationSummary_v2BotRole' - The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
newMigrationSummary ::
  MigrationSummary
newMigrationSummary =
  MigrationSummary'
    { migrationId = Prelude.Nothing,
      migrationStatus = Prelude.Nothing,
      migrationStrategy = Prelude.Nothing,
      migrationTimestamp = Prelude.Nothing,
      v1BotLocale = Prelude.Nothing,
      v1BotName = Prelude.Nothing,
      v1BotVersion = Prelude.Nothing,
      v2BotId = Prelude.Nothing,
      v2BotRole = Prelude.Nothing
    }

-- | The unique identifier that Amazon Lex assigned to the migration.
migrationSummary_migrationId :: Lens.Lens' MigrationSummary (Prelude.Maybe Prelude.Text)
migrationSummary_migrationId = Lens.lens (\MigrationSummary' {migrationId} -> migrationId) (\s@MigrationSummary' {} a -> s {migrationId = a} :: MigrationSummary)

-- | The status of the operation. When the status is @COMPLETE@ the bot is
-- available in Amazon Lex V2. There may be alerts and warnings that need
-- to be resolved to complete the migration.
migrationSummary_migrationStatus :: Lens.Lens' MigrationSummary (Prelude.Maybe MigrationStatus)
migrationSummary_migrationStatus = Lens.lens (\MigrationSummary' {migrationStatus} -> migrationStatus) (\s@MigrationSummary' {} a -> s {migrationStatus = a} :: MigrationSummary)

-- | The strategy used to conduct the migration.
migrationSummary_migrationStrategy :: Lens.Lens' MigrationSummary (Prelude.Maybe MigrationStrategy)
migrationSummary_migrationStrategy = Lens.lens (\MigrationSummary' {migrationStrategy} -> migrationStrategy) (\s@MigrationSummary' {} a -> s {migrationStrategy = a} :: MigrationSummary)

-- | The date and time that the migration started.
migrationSummary_migrationTimestamp :: Lens.Lens' MigrationSummary (Prelude.Maybe Prelude.UTCTime)
migrationSummary_migrationTimestamp = Lens.lens (\MigrationSummary' {migrationTimestamp} -> migrationTimestamp) (\s@MigrationSummary' {} a -> s {migrationTimestamp = a} :: MigrationSummary) Prelude.. Lens.mapping Data._Time

-- | The locale of the Amazon Lex V1 bot that is the source of the migration.
migrationSummary_v1BotLocale :: Lens.Lens' MigrationSummary (Prelude.Maybe Locale)
migrationSummary_v1BotLocale = Lens.lens (\MigrationSummary' {v1BotLocale} -> v1BotLocale) (\s@MigrationSummary' {} a -> s {v1BotLocale = a} :: MigrationSummary)

-- | The name of the Amazon Lex V1 bot that is the source of the migration.
migrationSummary_v1BotName :: Lens.Lens' MigrationSummary (Prelude.Maybe Prelude.Text)
migrationSummary_v1BotName = Lens.lens (\MigrationSummary' {v1BotName} -> v1BotName) (\s@MigrationSummary' {} a -> s {v1BotName = a} :: MigrationSummary)

-- | The version of the Amazon Lex V1 bot that is the source of the
-- migration.
migrationSummary_v1BotVersion :: Lens.Lens' MigrationSummary (Prelude.Maybe Prelude.Text)
migrationSummary_v1BotVersion = Lens.lens (\MigrationSummary' {v1BotVersion} -> v1BotVersion) (\s@MigrationSummary' {} a -> s {v1BotVersion = a} :: MigrationSummary)

-- | The unique identifier of the Amazon Lex V2 that is the destination of
-- the migration.
migrationSummary_v2BotId :: Lens.Lens' MigrationSummary (Prelude.Maybe Prelude.Text)
migrationSummary_v2BotId = Lens.lens (\MigrationSummary' {v2BotId} -> v2BotId) (\s@MigrationSummary' {} a -> s {v2BotId = a} :: MigrationSummary)

-- | The IAM role that Amazon Lex uses to run the Amazon Lex V2 bot.
migrationSummary_v2BotRole :: Lens.Lens' MigrationSummary (Prelude.Maybe Prelude.Text)
migrationSummary_v2BotRole = Lens.lens (\MigrationSummary' {v2BotRole} -> v2BotRole) (\s@MigrationSummary' {} a -> s {v2BotRole = a} :: MigrationSummary)

instance Data.FromJSON MigrationSummary where
  parseJSON =
    Data.withObject
      "MigrationSummary"
      ( \x ->
          MigrationSummary'
            Prelude.<$> (x Data..:? "migrationId")
            Prelude.<*> (x Data..:? "migrationStatus")
            Prelude.<*> (x Data..:? "migrationStrategy")
            Prelude.<*> (x Data..:? "migrationTimestamp")
            Prelude.<*> (x Data..:? "v1BotLocale")
            Prelude.<*> (x Data..:? "v1BotName")
            Prelude.<*> (x Data..:? "v1BotVersion")
            Prelude.<*> (x Data..:? "v2BotId")
            Prelude.<*> (x Data..:? "v2BotRole")
      )

instance Prelude.Hashable MigrationSummary where
  hashWithSalt _salt MigrationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` migrationId
      `Prelude.hashWithSalt` migrationStatus
      `Prelude.hashWithSalt` migrationStrategy
      `Prelude.hashWithSalt` migrationTimestamp
      `Prelude.hashWithSalt` v1BotLocale
      `Prelude.hashWithSalt` v1BotName
      `Prelude.hashWithSalt` v1BotVersion
      `Prelude.hashWithSalt` v2BotId
      `Prelude.hashWithSalt` v2BotRole

instance Prelude.NFData MigrationSummary where
  rnf MigrationSummary' {..} =
    Prelude.rnf migrationId
      `Prelude.seq` Prelude.rnf migrationStatus
      `Prelude.seq` Prelude.rnf migrationStrategy
      `Prelude.seq` Prelude.rnf migrationTimestamp
      `Prelude.seq` Prelude.rnf v1BotLocale
      `Prelude.seq` Prelude.rnf v1BotName
      `Prelude.seq` Prelude.rnf v1BotVersion
      `Prelude.seq` Prelude.rnf v2BotId
      `Prelude.seq` Prelude.rnf v2BotRole
