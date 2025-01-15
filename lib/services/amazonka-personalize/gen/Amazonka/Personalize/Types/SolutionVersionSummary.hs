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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.SolutionVersionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a solution version. For a
-- complete listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeSolutionVersion.html DescribeSolutionVersion>
-- API.
--
-- /See:/ 'newSolutionVersionSummary' smart constructor.
data SolutionVersionSummary = SolutionVersionSummary'
  { -- | The date and time (in Unix time) that this version of a solution was
    -- created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | If a solution version fails, the reason behind the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the solution version was last
    -- updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the solution version.
    solutionVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the solution version.
    --
    -- A solution version can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text
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
-- 'creationDateTime', 'solutionVersionSummary_creationDateTime' - The date and time (in Unix time) that this version of a solution was
-- created.
--
-- 'failureReason', 'solutionVersionSummary_failureReason' - If a solution version fails, the reason behind the failure.
--
-- 'lastUpdatedDateTime', 'solutionVersionSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the solution version was last
-- updated.
--
-- 'solutionVersionArn', 'solutionVersionSummary_solutionVersionArn' - The Amazon Resource Name (ARN) of the solution version.
--
-- 'status', 'solutionVersionSummary_status' - The status of the solution version.
--
-- A solution version can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
newSolutionVersionSummary ::
  SolutionVersionSummary
newSolutionVersionSummary =
  SolutionVersionSummary'
    { creationDateTime =
        Prelude.Nothing,
      failureReason = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      solutionVersionArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix time) that this version of a solution was
-- created.
solutionVersionSummary_creationDateTime :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.UTCTime)
solutionVersionSummary_creationDateTime = Lens.lens (\SolutionVersionSummary' {creationDateTime} -> creationDateTime) (\s@SolutionVersionSummary' {} a -> s {creationDateTime = a} :: SolutionVersionSummary) Prelude.. Lens.mapping Data._Time

-- | If a solution version fails, the reason behind the failure.
solutionVersionSummary_failureReason :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.Text)
solutionVersionSummary_failureReason = Lens.lens (\SolutionVersionSummary' {failureReason} -> failureReason) (\s@SolutionVersionSummary' {} a -> s {failureReason = a} :: SolutionVersionSummary)

-- | The date and time (in Unix time) that the solution version was last
-- updated.
solutionVersionSummary_lastUpdatedDateTime :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.UTCTime)
solutionVersionSummary_lastUpdatedDateTime = Lens.lens (\SolutionVersionSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SolutionVersionSummary' {} a -> s {lastUpdatedDateTime = a} :: SolutionVersionSummary) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the solution version.
solutionVersionSummary_solutionVersionArn :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.Text)
solutionVersionSummary_solutionVersionArn = Lens.lens (\SolutionVersionSummary' {solutionVersionArn} -> solutionVersionArn) (\s@SolutionVersionSummary' {} a -> s {solutionVersionArn = a} :: SolutionVersionSummary)

-- | The status of the solution version.
--
-- A solution version can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
solutionVersionSummary_status :: Lens.Lens' SolutionVersionSummary (Prelude.Maybe Prelude.Text)
solutionVersionSummary_status = Lens.lens (\SolutionVersionSummary' {status} -> status) (\s@SolutionVersionSummary' {} a -> s {status = a} :: SolutionVersionSummary)

instance Data.FromJSON SolutionVersionSummary where
  parseJSON =
    Data.withObject
      "SolutionVersionSummary"
      ( \x ->
          SolutionVersionSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "failureReason")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "solutionVersionArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable SolutionVersionSummary where
  hashWithSalt _salt SolutionVersionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` solutionVersionArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData SolutionVersionSummary where
  rnf SolutionVersionSummary' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf failureReason `Prelude.seq`
        Prelude.rnf lastUpdatedDateTime `Prelude.seq`
          Prelude.rnf solutionVersionArn `Prelude.seq`
            Prelude.rnf status
