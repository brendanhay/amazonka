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
-- Module      : Amazonka.Personalize.Types.SolutionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.SolutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides a summary of the properties of a solution. For a complete
-- listing, call the
-- <https://docs.aws.amazon.com/personalize/latest/dg/API_DescribeSolution.html DescribeSolution>
-- API.
--
-- /See:/ 'newSolutionSummary' smart constructor.
data SolutionSummary = SolutionSummary'
  { -- | The date and time (in Unix time) that the solution was created.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time (in Unix time) that the solution was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the solution.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the recipe used by the solution.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the solution.
    solutionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the solution.
    --
    -- A solution can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SolutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'solutionSummary_creationDateTime' - The date and time (in Unix time) that the solution was created.
--
-- 'lastUpdatedDateTime', 'solutionSummary_lastUpdatedDateTime' - The date and time (in Unix time) that the solution was last updated.
--
-- 'name', 'solutionSummary_name' - The name of the solution.
--
-- 'recipeArn', 'solutionSummary_recipeArn' - The Amazon Resource Name (ARN) of the recipe used by the solution.
--
-- 'solutionArn', 'solutionSummary_solutionArn' - The Amazon Resource Name (ARN) of the solution.
--
-- 'status', 'solutionSummary_status' - The status of the solution.
--
-- A solution can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
newSolutionSummary ::
  SolutionSummary
newSolutionSummary =
  SolutionSummary'
    { creationDateTime =
        Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      solutionArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date and time (in Unix time) that the solution was created.
solutionSummary_creationDateTime :: Lens.Lens' SolutionSummary (Prelude.Maybe Prelude.UTCTime)
solutionSummary_creationDateTime = Lens.lens (\SolutionSummary' {creationDateTime} -> creationDateTime) (\s@SolutionSummary' {} a -> s {creationDateTime = a} :: SolutionSummary) Prelude.. Lens.mapping Data._Time

-- | The date and time (in Unix time) that the solution was last updated.
solutionSummary_lastUpdatedDateTime :: Lens.Lens' SolutionSummary (Prelude.Maybe Prelude.UTCTime)
solutionSummary_lastUpdatedDateTime = Lens.lens (\SolutionSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SolutionSummary' {} a -> s {lastUpdatedDateTime = a} :: SolutionSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the solution.
solutionSummary_name :: Lens.Lens' SolutionSummary (Prelude.Maybe Prelude.Text)
solutionSummary_name = Lens.lens (\SolutionSummary' {name} -> name) (\s@SolutionSummary' {} a -> s {name = a} :: SolutionSummary)

-- | The Amazon Resource Name (ARN) of the recipe used by the solution.
solutionSummary_recipeArn :: Lens.Lens' SolutionSummary (Prelude.Maybe Prelude.Text)
solutionSummary_recipeArn = Lens.lens (\SolutionSummary' {recipeArn} -> recipeArn) (\s@SolutionSummary' {} a -> s {recipeArn = a} :: SolutionSummary)

-- | The Amazon Resource Name (ARN) of the solution.
solutionSummary_solutionArn :: Lens.Lens' SolutionSummary (Prelude.Maybe Prelude.Text)
solutionSummary_solutionArn = Lens.lens (\SolutionSummary' {solutionArn} -> solutionArn) (\s@SolutionSummary' {} a -> s {solutionArn = a} :: SolutionSummary)

-- | The status of the solution.
--
-- A solution can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
solutionSummary_status :: Lens.Lens' SolutionSummary (Prelude.Maybe Prelude.Text)
solutionSummary_status = Lens.lens (\SolutionSummary' {status} -> status) (\s@SolutionSummary' {} a -> s {status = a} :: SolutionSummary)

instance Data.FromJSON SolutionSummary where
  parseJSON =
    Data.withObject
      "SolutionSummary"
      ( \x ->
          SolutionSummary'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "recipeArn")
            Prelude.<*> (x Data..:? "solutionArn")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable SolutionSummary where
  hashWithSalt _salt SolutionSummary' {..} =
    _salt `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recipeArn
      `Prelude.hashWithSalt` solutionArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData SolutionSummary where
  rnf SolutionSummary' {..} =
    Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recipeArn
      `Prelude.seq` Prelude.rnf solutionArn
      `Prelude.seq` Prelude.rnf status
