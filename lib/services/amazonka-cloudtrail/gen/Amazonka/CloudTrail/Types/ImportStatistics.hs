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
-- Module      : Amazonka.CloudTrail.Types.ImportStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.ImportStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides statistics for the specified @ImportID@. CloudTrail does not
-- update import statistics in real-time. Returned values for parameters
-- such as @EventsCompleted@ may be lower than the actual value, because
-- CloudTrail updates statistics incrementally over the course of the
-- import.
--
-- /See:/ 'newImportStatistics' smart constructor.
data ImportStatistics = ImportStatistics'
  { -- | The number of failed entries.
    failedEntries :: Prelude.Maybe Prelude.Integer,
    -- | The number of trail events imported into the event data store.
    eventsCompleted :: Prelude.Maybe Prelude.Integer,
    -- | The number of S3 prefixes found for the import.
    prefixesFound :: Prelude.Maybe Prelude.Integer,
    -- | The number of S3 prefixes that completed import.
    prefixesCompleted :: Prelude.Maybe Prelude.Integer,
    -- | The number of log files that completed import.
    filesCompleted :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntries', 'importStatistics_failedEntries' - The number of failed entries.
--
-- 'eventsCompleted', 'importStatistics_eventsCompleted' - The number of trail events imported into the event data store.
--
-- 'prefixesFound', 'importStatistics_prefixesFound' - The number of S3 prefixes found for the import.
--
-- 'prefixesCompleted', 'importStatistics_prefixesCompleted' - The number of S3 prefixes that completed import.
--
-- 'filesCompleted', 'importStatistics_filesCompleted' - The number of log files that completed import.
newImportStatistics ::
  ImportStatistics
newImportStatistics =
  ImportStatistics'
    { failedEntries = Prelude.Nothing,
      eventsCompleted = Prelude.Nothing,
      prefixesFound = Prelude.Nothing,
      prefixesCompleted = Prelude.Nothing,
      filesCompleted = Prelude.Nothing
    }

-- | The number of failed entries.
importStatistics_failedEntries :: Lens.Lens' ImportStatistics (Prelude.Maybe Prelude.Integer)
importStatistics_failedEntries = Lens.lens (\ImportStatistics' {failedEntries} -> failedEntries) (\s@ImportStatistics' {} a -> s {failedEntries = a} :: ImportStatistics)

-- | The number of trail events imported into the event data store.
importStatistics_eventsCompleted :: Lens.Lens' ImportStatistics (Prelude.Maybe Prelude.Integer)
importStatistics_eventsCompleted = Lens.lens (\ImportStatistics' {eventsCompleted} -> eventsCompleted) (\s@ImportStatistics' {} a -> s {eventsCompleted = a} :: ImportStatistics)

-- | The number of S3 prefixes found for the import.
importStatistics_prefixesFound :: Lens.Lens' ImportStatistics (Prelude.Maybe Prelude.Integer)
importStatistics_prefixesFound = Lens.lens (\ImportStatistics' {prefixesFound} -> prefixesFound) (\s@ImportStatistics' {} a -> s {prefixesFound = a} :: ImportStatistics)

-- | The number of S3 prefixes that completed import.
importStatistics_prefixesCompleted :: Lens.Lens' ImportStatistics (Prelude.Maybe Prelude.Integer)
importStatistics_prefixesCompleted = Lens.lens (\ImportStatistics' {prefixesCompleted} -> prefixesCompleted) (\s@ImportStatistics' {} a -> s {prefixesCompleted = a} :: ImportStatistics)

-- | The number of log files that completed import.
importStatistics_filesCompleted :: Lens.Lens' ImportStatistics (Prelude.Maybe Prelude.Integer)
importStatistics_filesCompleted = Lens.lens (\ImportStatistics' {filesCompleted} -> filesCompleted) (\s@ImportStatistics' {} a -> s {filesCompleted = a} :: ImportStatistics)

instance Data.FromJSON ImportStatistics where
  parseJSON =
    Data.withObject
      "ImportStatistics"
      ( \x ->
          ImportStatistics'
            Prelude.<$> (x Data..:? "FailedEntries")
            Prelude.<*> (x Data..:? "EventsCompleted")
            Prelude.<*> (x Data..:? "PrefixesFound")
            Prelude.<*> (x Data..:? "PrefixesCompleted")
            Prelude.<*> (x Data..:? "FilesCompleted")
      )

instance Prelude.Hashable ImportStatistics where
  hashWithSalt _salt ImportStatistics' {..} =
    _salt `Prelude.hashWithSalt` failedEntries
      `Prelude.hashWithSalt` eventsCompleted
      `Prelude.hashWithSalt` prefixesFound
      `Prelude.hashWithSalt` prefixesCompleted
      `Prelude.hashWithSalt` filesCompleted

instance Prelude.NFData ImportStatistics where
  rnf ImportStatistics' {..} =
    Prelude.rnf failedEntries
      `Prelude.seq` Prelude.rnf eventsCompleted
      `Prelude.seq` Prelude.rnf prefixesFound
      `Prelude.seq` Prelude.rnf prefixesCompleted
      `Prelude.seq` Prelude.rnf filesCompleted
