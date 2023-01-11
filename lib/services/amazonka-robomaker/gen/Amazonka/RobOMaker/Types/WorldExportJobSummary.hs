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
-- Module      : Amazonka.RobOMaker.Types.WorldExportJobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldExportJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.OutputLocation
import Amazonka.RobOMaker.Types.WorldExportJobStatus

-- | Information about a world export job.
--
-- /See:/ 'newWorldExportJobSummary' smart constructor.
data WorldExportJobSummary = WorldExportJobSummary'
  { -- | The Amazon Resource Name (ARN) of the world export job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world export job was
    -- created.
    createdAt :: Prelude.Maybe Data.POSIX,
    outputLocation :: Prelude.Maybe OutputLocation,
    -- | The status of the world export job.
    --
    -- [Pending]
    --     The world export job request is pending.
    --
    -- [Running]
    --     The world export job is running.
    --
    -- [Completed]
    --     The world export job completed.
    --
    -- [Failed]
    --     The world export job failed. See @failureCode@ for more information.
    --
    -- [Canceled]
    --     The world export job was cancelled.
    --
    -- [Canceling]
    --     The world export job is being cancelled.
    status :: Prelude.Maybe WorldExportJobStatus,
    -- | A list of worlds.
    worlds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorldExportJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'worldExportJobSummary_arn' - The Amazon Resource Name (ARN) of the world export job.
--
-- 'createdAt', 'worldExportJobSummary_createdAt' - The time, in milliseconds since the epoch, when the world export job was
-- created.
--
-- 'outputLocation', 'worldExportJobSummary_outputLocation' - Undocumented member.
--
-- 'status', 'worldExportJobSummary_status' - The status of the world export job.
--
-- [Pending]
--     The world export job request is pending.
--
-- [Running]
--     The world export job is running.
--
-- [Completed]
--     The world export job completed.
--
-- [Failed]
--     The world export job failed. See @failureCode@ for more information.
--
-- [Canceled]
--     The world export job was cancelled.
--
-- [Canceling]
--     The world export job is being cancelled.
--
-- 'worlds', 'worldExportJobSummary_worlds' - A list of worlds.
newWorldExportJobSummary ::
  WorldExportJobSummary
newWorldExportJobSummary =
  WorldExportJobSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      outputLocation = Prelude.Nothing,
      status = Prelude.Nothing,
      worlds = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the world export job.
worldExportJobSummary_arn :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe Prelude.Text)
worldExportJobSummary_arn = Lens.lens (\WorldExportJobSummary' {arn} -> arn) (\s@WorldExportJobSummary' {} a -> s {arn = a} :: WorldExportJobSummary)

-- | The time, in milliseconds since the epoch, when the world export job was
-- created.
worldExportJobSummary_createdAt :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe Prelude.UTCTime)
worldExportJobSummary_createdAt = Lens.lens (\WorldExportJobSummary' {createdAt} -> createdAt) (\s@WorldExportJobSummary' {} a -> s {createdAt = a} :: WorldExportJobSummary) Prelude.. Lens.mapping Data._Time

-- | Undocumented member.
worldExportJobSummary_outputLocation :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe OutputLocation)
worldExportJobSummary_outputLocation = Lens.lens (\WorldExportJobSummary' {outputLocation} -> outputLocation) (\s@WorldExportJobSummary' {} a -> s {outputLocation = a} :: WorldExportJobSummary)

-- | The status of the world export job.
--
-- [Pending]
--     The world export job request is pending.
--
-- [Running]
--     The world export job is running.
--
-- [Completed]
--     The world export job completed.
--
-- [Failed]
--     The world export job failed. See @failureCode@ for more information.
--
-- [Canceled]
--     The world export job was cancelled.
--
-- [Canceling]
--     The world export job is being cancelled.
worldExportJobSummary_status :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe WorldExportJobStatus)
worldExportJobSummary_status = Lens.lens (\WorldExportJobSummary' {status} -> status) (\s@WorldExportJobSummary' {} a -> s {status = a} :: WorldExportJobSummary)

-- | A list of worlds.
worldExportJobSummary_worlds :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
worldExportJobSummary_worlds = Lens.lens (\WorldExportJobSummary' {worlds} -> worlds) (\s@WorldExportJobSummary' {} a -> s {worlds = a} :: WorldExportJobSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON WorldExportJobSummary where
  parseJSON =
    Data.withObject
      "WorldExportJobSummary"
      ( \x ->
          WorldExportJobSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "outputLocation")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "worlds")
      )

instance Prelude.Hashable WorldExportJobSummary where
  hashWithSalt _salt WorldExportJobSummary' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` outputLocation
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` worlds

instance Prelude.NFData WorldExportJobSummary where
  rnf WorldExportJobSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf outputLocation
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf worlds
