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
-- Module      : Network.AWS.RobOMaker.Types.WorldExportJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RobOMaker.Types.WorldExportJobSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RobOMaker.Types.WorldExportJobStatus

-- | Information about a world export job.
--
-- /See:/ 'newWorldExportJobSummary' smart constructor.
data WorldExportJobSummary = WorldExportJobSummary'
  { -- | The status of the world export job.
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
    -- | The Amazon Resource Name (ARN) of the world export job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world export job was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
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
-- 'arn', 'worldExportJobSummary_arn' - The Amazon Resource Name (ARN) of the world export job.
--
-- 'createdAt', 'worldExportJobSummary_createdAt' - The time, in milliseconds since the epoch, when the world export job was
-- created.
--
-- 'worlds', 'worldExportJobSummary_worlds' - A list of worlds.
newWorldExportJobSummary ::
  WorldExportJobSummary
newWorldExportJobSummary =
  WorldExportJobSummary'
    { status = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      worlds = Prelude.Nothing
    }

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

-- | The Amazon Resource Name (ARN) of the world export job.
worldExportJobSummary_arn :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe Prelude.Text)
worldExportJobSummary_arn = Lens.lens (\WorldExportJobSummary' {arn} -> arn) (\s@WorldExportJobSummary' {} a -> s {arn = a} :: WorldExportJobSummary)

-- | The time, in milliseconds since the epoch, when the world export job was
-- created.
worldExportJobSummary_createdAt :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe Prelude.UTCTime)
worldExportJobSummary_createdAt = Lens.lens (\WorldExportJobSummary' {createdAt} -> createdAt) (\s@WorldExportJobSummary' {} a -> s {createdAt = a} :: WorldExportJobSummary) Prelude.. Lens.mapping Core._Time

-- | A list of worlds.
worldExportJobSummary_worlds :: Lens.Lens' WorldExportJobSummary (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
worldExportJobSummary_worlds = Lens.lens (\WorldExportJobSummary' {worlds} -> worlds) (\s@WorldExportJobSummary' {} a -> s {worlds = a} :: WorldExportJobSummary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON WorldExportJobSummary where
  parseJSON =
    Core.withObject
      "WorldExportJobSummary"
      ( \x ->
          WorldExportJobSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "worlds")
      )

instance Prelude.Hashable WorldExportJobSummary

instance Prelude.NFData WorldExportJobSummary
