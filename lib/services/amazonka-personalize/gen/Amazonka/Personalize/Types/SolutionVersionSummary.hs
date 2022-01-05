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
-- Module      : Amazonka.Personalize.Types.SolutionVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.SolutionVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a solution version. For a
-- complete listing, call the DescribeSolutionVersion API.
--
-- /See:/ 'newSolutionVersionSummary' smart constructor.
data SolutionVersionSummary = SolutionVersionSummary'
  { -- | If a solution version fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the solution version.
    --
    -- A solution version can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the solution version was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time (in Unix time) that this version of a solution was
    -- created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the solution version.
    solutionVersionArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SolutionVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'solutionVersionSummary_failureReason' - If a solution version fails, the reason behind the failure.
--
-- 'status', 'solutionVersionSummary_status' - The status of the solution version.
--
-- A solution version can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'lastUpdatedDateTime', 'solutionVersionSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the solution version was last
-- updated.
--
-- 'creationDateTime', 'solutionVersionSummary_creationDateTime' - The date and time (in Unix time) that this version of a solution was
-- created.
--
-- 'solutionVersionArn', 'solutionVersionSummary_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version.
newSolutionVersionSummary ::
  SolutionVersionSummary
newSolutionVersionSummary =
  SolutionVersionSummary'
    { failureReason =
        Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing
    }

-- | If a solution version fails, the reason behind the failure.
solutionVersionSummary_failureReason :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.Text)
solutionVersionSummary_failureReason = Lens.lens (\SolutionVersionSummary' {failureReason} -> failureReason) (\s@SolutionVersionSummary' {} a -> s {failureReason = a} :: SolutionVersionSummary)

-- | The status of the solution version.
--
-- A solution version can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
solutionVersionSummary_status :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.Text)
solutionVersionSummary_status = Lens.lens (\SolutionVersionSummary' {status} -> status) (\s@SolutionVersionSummary' {} a -> s {status = a} :: SolutionVersionSummary)

-- | The date and time (in Unix time) that the solution version was last
-- updated.
solutionVersionSummary_lastUpdatedDateTime :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.UTCTime)
solutionVersionSummary_lastUpdatedDateTime = Lens.lens (\SolutionVersionSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SolutionVersionSummary' {} a -> s {lastUpdatedDateTime = a} :: SolutionVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The date and time (in Unix time) that this version of a solution was
-- created.
solutionVersionSummary_creationDateTime :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.UTCTime)
solutionVersionSummary_creationDateTime = Lens.lens (\SolutionVersionSummary' {creationDateTime} -> creationDateTime) (\s@SolutionVersionSummary' {} a -> s {creationDateTime = a} :: SolutionVersionSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the solution version.
solutionVersionSummary_solutionVersionArn :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.Text)
solutionVersionSummary_solutionVersionArn = Lens.lens (\SolutionVersionSummary' {solutionVersionArn} -> solutionVersionArn) (\s@SolutionVersionSummary' {} a -> s {solutionVersionArn = a} :: SolutionVersionSummary)

instance Core.FromJSON SolutionVersionSummary where
  parseJSON =
    Core.withObject
      "SolutionVersionSummary"
      ( \x ->
          SolutionVersionSummary'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "solutionVersionArn")
      )

instance Prelude.Hashable SolutionVersionSummary where
  hashWithSalt _salt SolutionVersionSummary' {..} =
    _salt `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` solutionVersionArn

instance Prelude.NFData SolutionVersionSummary where
  rnf SolutionVersionSummary' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf solutionVersionArn
