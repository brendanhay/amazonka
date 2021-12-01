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
-- Module      : Amazonka.Personalize.Types.Solution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.Solution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Personalize.Types.AutoMLResult
import Amazonka.Personalize.Types.SolutionConfig
import Amazonka.Personalize.Types.SolutionVersionSummary
import qualified Amazonka.Prelude as Prelude

-- | An object that provides information about a solution. A solution is a
-- trained model that can be deployed as a campaign.
--
-- /See:/ 'newSolution' smart constructor.
data Solution = Solution'
  { -- | The ARN of the solution.
    solutionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the solution.
    --
    -- A solution can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    --
    -- -   DELETE PENDING > DELETE IN_PROGRESS
    status :: Prelude.Maybe Prelude.Text,
    -- | When true, Amazon Personalize performs a search for the best
    -- USER_PERSONALIZATION recipe from the list specified in the solution
    -- configuration (@recipeArn@ must not be specified). When false (the
    -- default), Amazon Personalize uses @recipeArn@ for training.
    performAutoML :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the recipe used to create the solution.
    recipeArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time (in Unix time) that the solution was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The event type (for example, \'click\' or \'like\') that is used for
    -- training the model. If no @eventType@ is provided, Amazon Personalize
    -- uses all interactions for training with equal weight regardless of type.
    eventType :: Prelude.Maybe Prelude.Text,
    -- | The name of the solution.
    name :: Prelude.Maybe Prelude.Text,
    -- | When @performAutoML@ is true, specifies the best recipe found.
    autoMLResult :: Prelude.Maybe AutoMLResult,
    -- | The creation date and time (in Unix time) of the solution.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset group that provides the
    -- training data.
    datasetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Describes the latest version of the solution, including the status and
    -- the ARN.
    latestSolutionVersion :: Prelude.Maybe SolutionVersionSummary,
    -- | Describes the configuration properties for the solution.
    solutionConfig :: Prelude.Maybe SolutionConfig,
    -- | Whether to perform hyperparameter optimization (HPO) on the chosen
    -- recipe. The default is @false@.
    performHPO :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Solution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'solutionArn', 'solution_solutionArn' - The ARN of the solution.
--
-- 'status', 'solution_status' - The status of the solution.
--
-- A solution can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
--
-- 'performAutoML', 'solution_performAutoML' - When true, Amazon Personalize performs a search for the best
-- USER_PERSONALIZATION recipe from the list specified in the solution
-- configuration (@recipeArn@ must not be specified). When false (the
-- default), Amazon Personalize uses @recipeArn@ for training.
--
-- 'recipeArn', 'solution_recipeArn' - The ARN of the recipe used to create the solution.
--
-- 'lastUpdatedDateTime', 'solution_lastUpdatedDateTime' - The date and time (in Unix time) that the solution was last updated.
--
-- 'eventType', 'solution_eventType' - The event type (for example, \'click\' or \'like\') that is used for
-- training the model. If no @eventType@ is provided, Amazon Personalize
-- uses all interactions for training with equal weight regardless of type.
--
-- 'name', 'solution_name' - The name of the solution.
--
-- 'autoMLResult', 'solution_autoMLResult' - When @performAutoML@ is true, specifies the best recipe found.
--
-- 'creationDateTime', 'solution_creationDateTime' - The creation date and time (in Unix time) of the solution.
--
-- 'datasetGroupArn', 'solution_datasetGroupArn' - The Amazon Resource Name (ARN) of the dataset group that provides the
-- training data.
--
-- 'latestSolutionVersion', 'solution_latestSolutionVersion' - Describes the latest version of the solution, including the status and
-- the ARN.
--
-- 'solutionConfig', 'solution_solutionConfig' - Describes the configuration properties for the solution.
--
-- 'performHPO', 'solution_performHPO' - Whether to perform hyperparameter optimization (HPO) on the chosen
-- recipe. The default is @false@.
newSolution ::
  Solution
newSolution =
  Solution'
    { solutionArn = Prelude.Nothing,
      status = Prelude.Nothing,
      performAutoML = Prelude.Nothing,
      recipeArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      eventType = Prelude.Nothing,
      name = Prelude.Nothing,
      autoMLResult = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing,
      latestSolutionVersion = Prelude.Nothing,
      solutionConfig = Prelude.Nothing,
      performHPO = Prelude.Nothing
    }

-- | The ARN of the solution.
solution_solutionArn :: Lens.Lens' Solution (Prelude.Maybe Prelude.Text)
solution_solutionArn = Lens.lens (\Solution' {solutionArn} -> solutionArn) (\s@Solution' {} a -> s {solutionArn = a} :: Solution)

-- | The status of the solution.
--
-- A solution can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- -   DELETE PENDING > DELETE IN_PROGRESS
solution_status :: Lens.Lens' Solution (Prelude.Maybe Prelude.Text)
solution_status = Lens.lens (\Solution' {status} -> status) (\s@Solution' {} a -> s {status = a} :: Solution)

-- | When true, Amazon Personalize performs a search for the best
-- USER_PERSONALIZATION recipe from the list specified in the solution
-- configuration (@recipeArn@ must not be specified). When false (the
-- default), Amazon Personalize uses @recipeArn@ for training.
solution_performAutoML :: Lens.Lens' Solution (Prelude.Maybe Prelude.Bool)
solution_performAutoML = Lens.lens (\Solution' {performAutoML} -> performAutoML) (\s@Solution' {} a -> s {performAutoML = a} :: Solution)

-- | The ARN of the recipe used to create the solution.
solution_recipeArn :: Lens.Lens' Solution (Prelude.Maybe Prelude.Text)
solution_recipeArn = Lens.lens (\Solution' {recipeArn} -> recipeArn) (\s@Solution' {} a -> s {recipeArn = a} :: Solution)

-- | The date and time (in Unix time) that the solution was last updated.
solution_lastUpdatedDateTime :: Lens.Lens' Solution (Prelude.Maybe Prelude.UTCTime)
solution_lastUpdatedDateTime = Lens.lens (\Solution' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@Solution' {} a -> s {lastUpdatedDateTime = a} :: Solution) Prelude.. Lens.mapping Core._Time

-- | The event type (for example, \'click\' or \'like\') that is used for
-- training the model. If no @eventType@ is provided, Amazon Personalize
-- uses all interactions for training with equal weight regardless of type.
solution_eventType :: Lens.Lens' Solution (Prelude.Maybe Prelude.Text)
solution_eventType = Lens.lens (\Solution' {eventType} -> eventType) (\s@Solution' {} a -> s {eventType = a} :: Solution)

-- | The name of the solution.
solution_name :: Lens.Lens' Solution (Prelude.Maybe Prelude.Text)
solution_name = Lens.lens (\Solution' {name} -> name) (\s@Solution' {} a -> s {name = a} :: Solution)

-- | When @performAutoML@ is true, specifies the best recipe found.
solution_autoMLResult :: Lens.Lens' Solution (Prelude.Maybe AutoMLResult)
solution_autoMLResult = Lens.lens (\Solution' {autoMLResult} -> autoMLResult) (\s@Solution' {} a -> s {autoMLResult = a} :: Solution)

-- | The creation date and time (in Unix time) of the solution.
solution_creationDateTime :: Lens.Lens' Solution (Prelude.Maybe Prelude.UTCTime)
solution_creationDateTime = Lens.lens (\Solution' {creationDateTime} -> creationDateTime) (\s@Solution' {} a -> s {creationDateTime = a} :: Solution) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset group that provides the
-- training data.
solution_datasetGroupArn :: Lens.Lens' Solution (Prelude.Maybe Prelude.Text)
solution_datasetGroupArn = Lens.lens (\Solution' {datasetGroupArn} -> datasetGroupArn) (\s@Solution' {} a -> s {datasetGroupArn = a} :: Solution)

-- | Describes the latest version of the solution, including the status and
-- the ARN.
solution_latestSolutionVersion :: Lens.Lens' Solution (Prelude.Maybe SolutionVersionSummary)
solution_latestSolutionVersion = Lens.lens (\Solution' {latestSolutionVersion} -> latestSolutionVersion) (\s@Solution' {} a -> s {latestSolutionVersion = a} :: Solution)

-- | Describes the configuration properties for the solution.
solution_solutionConfig :: Lens.Lens' Solution (Prelude.Maybe SolutionConfig)
solution_solutionConfig = Lens.lens (\Solution' {solutionConfig} -> solutionConfig) (\s@Solution' {} a -> s {solutionConfig = a} :: Solution)

-- | Whether to perform hyperparameter optimization (HPO) on the chosen
-- recipe. The default is @false@.
solution_performHPO :: Lens.Lens' Solution (Prelude.Maybe Prelude.Bool)
solution_performHPO = Lens.lens (\Solution' {performHPO} -> performHPO) (\s@Solution' {} a -> s {performHPO = a} :: Solution)

instance Core.FromJSON Solution where
  parseJSON =
    Core.withObject
      "Solution"
      ( \x ->
          Solution'
            Prelude.<$> (x Core..:? "solutionArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "performAutoML")
            Prelude.<*> (x Core..:? "recipeArn")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "eventType")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "autoMLResult")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "datasetGroupArn")
            Prelude.<*> (x Core..:? "latestSolutionVersion")
            Prelude.<*> (x Core..:? "solutionConfig")
            Prelude.<*> (x Core..:? "performHPO")
      )

instance Prelude.Hashable Solution where
  hashWithSalt salt' Solution' {..} =
    salt' `Prelude.hashWithSalt` performHPO
      `Prelude.hashWithSalt` solutionConfig
      `Prelude.hashWithSalt` latestSolutionVersion
      `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` autoMLResult
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` recipeArn
      `Prelude.hashWithSalt` performAutoML
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` solutionArn

instance Prelude.NFData Solution where
  rnf Solution' {..} =
    Prelude.rnf solutionArn
      `Prelude.seq` Prelude.rnf performHPO
      `Prelude.seq` Prelude.rnf solutionConfig
      `Prelude.seq` Prelude.rnf latestSolutionVersion
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf autoMLResult
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf recipeArn
      `Prelude.seq` Prelude.rnf performAutoML
      `Prelude.seq` Prelude.rnf status
