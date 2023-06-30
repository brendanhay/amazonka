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
-- Module      : Amazonka.Glue.Types.DataQualityResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataQualityRuleResult
import Amazonka.Glue.Types.DataSource
import qualified Amazonka.Prelude as Prelude

-- | Describes a data quality result.
--
-- /See:/ 'newDataQualityResult' smart constructor.
data DataQualityResult = DataQualityResult'
  { -- | The date and time when this data quality run completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | The table associated with the data quality result, if any.
    dataSource :: Prelude.Maybe DataSource,
    -- | In the context of a job in Glue Studio, each node in the canvas is
    -- typically assigned some sort of name and data quality nodes will have
    -- names. In the case of multiple nodes, the @evaluationContext@ can
    -- differentiate the nodes.
    evaluationContext :: Prelude.Maybe Prelude.Text,
    -- | The job name associated with the data quality result, if any.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The job run ID associated with the data quality result, if any.
    jobRunId :: Prelude.Maybe Prelude.Text,
    -- | A unique result ID for the data quality result.
    resultId :: Prelude.Maybe Prelude.Text,
    -- | A list of @DataQualityRuleResult@ objects representing the results for
    -- each rule.
    ruleResults :: Prelude.Maybe (Prelude.NonEmpty DataQualityRuleResult),
    -- | The unique run ID for the ruleset evaluation for this data quality
    -- result.
    rulesetEvaluationRunId :: Prelude.Maybe Prelude.Text,
    -- | The name of the ruleset associated with the data quality result.
    rulesetName :: Prelude.Maybe Prelude.Text,
    -- | An aggregate data quality score. Represents the ratio of rules that
    -- passed to the total number of rules.
    score :: Prelude.Maybe Prelude.Double,
    -- | The date and time when this data quality run started.
    startedOn :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'dataQualityResult_completedOn' - The date and time when this data quality run completed.
--
-- 'dataSource', 'dataQualityResult_dataSource' - The table associated with the data quality result, if any.
--
-- 'evaluationContext', 'dataQualityResult_evaluationContext' - In the context of a job in Glue Studio, each node in the canvas is
-- typically assigned some sort of name and data quality nodes will have
-- names. In the case of multiple nodes, the @evaluationContext@ can
-- differentiate the nodes.
--
-- 'jobName', 'dataQualityResult_jobName' - The job name associated with the data quality result, if any.
--
-- 'jobRunId', 'dataQualityResult_jobRunId' - The job run ID associated with the data quality result, if any.
--
-- 'resultId', 'dataQualityResult_resultId' - A unique result ID for the data quality result.
--
-- 'ruleResults', 'dataQualityResult_ruleResults' - A list of @DataQualityRuleResult@ objects representing the results for
-- each rule.
--
-- 'rulesetEvaluationRunId', 'dataQualityResult_rulesetEvaluationRunId' - The unique run ID for the ruleset evaluation for this data quality
-- result.
--
-- 'rulesetName', 'dataQualityResult_rulesetName' - The name of the ruleset associated with the data quality result.
--
-- 'score', 'dataQualityResult_score' - An aggregate data quality score. Represents the ratio of rules that
-- passed to the total number of rules.
--
-- 'startedOn', 'dataQualityResult_startedOn' - The date and time when this data quality run started.
newDataQualityResult ::
  DataQualityResult
newDataQualityResult =
  DataQualityResult'
    { completedOn = Prelude.Nothing,
      dataSource = Prelude.Nothing,
      evaluationContext = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      resultId = Prelude.Nothing,
      ruleResults = Prelude.Nothing,
      rulesetEvaluationRunId = Prelude.Nothing,
      rulesetName = Prelude.Nothing,
      score = Prelude.Nothing,
      startedOn = Prelude.Nothing
    }

-- | The date and time when this data quality run completed.
dataQualityResult_completedOn :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.UTCTime)
dataQualityResult_completedOn = Lens.lens (\DataQualityResult' {completedOn} -> completedOn) (\s@DataQualityResult' {} a -> s {completedOn = a} :: DataQualityResult) Prelude.. Lens.mapping Data._Time

-- | The table associated with the data quality result, if any.
dataQualityResult_dataSource :: Lens.Lens' DataQualityResult (Prelude.Maybe DataSource)
dataQualityResult_dataSource = Lens.lens (\DataQualityResult' {dataSource} -> dataSource) (\s@DataQualityResult' {} a -> s {dataSource = a} :: DataQualityResult)

-- | In the context of a job in Glue Studio, each node in the canvas is
-- typically assigned some sort of name and data quality nodes will have
-- names. In the case of multiple nodes, the @evaluationContext@ can
-- differentiate the nodes.
dataQualityResult_evaluationContext :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.Text)
dataQualityResult_evaluationContext = Lens.lens (\DataQualityResult' {evaluationContext} -> evaluationContext) (\s@DataQualityResult' {} a -> s {evaluationContext = a} :: DataQualityResult)

-- | The job name associated with the data quality result, if any.
dataQualityResult_jobName :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.Text)
dataQualityResult_jobName = Lens.lens (\DataQualityResult' {jobName} -> jobName) (\s@DataQualityResult' {} a -> s {jobName = a} :: DataQualityResult)

-- | The job run ID associated with the data quality result, if any.
dataQualityResult_jobRunId :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.Text)
dataQualityResult_jobRunId = Lens.lens (\DataQualityResult' {jobRunId} -> jobRunId) (\s@DataQualityResult' {} a -> s {jobRunId = a} :: DataQualityResult)

-- | A unique result ID for the data quality result.
dataQualityResult_resultId :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.Text)
dataQualityResult_resultId = Lens.lens (\DataQualityResult' {resultId} -> resultId) (\s@DataQualityResult' {} a -> s {resultId = a} :: DataQualityResult)

-- | A list of @DataQualityRuleResult@ objects representing the results for
-- each rule.
dataQualityResult_ruleResults :: Lens.Lens' DataQualityResult (Prelude.Maybe (Prelude.NonEmpty DataQualityRuleResult))
dataQualityResult_ruleResults = Lens.lens (\DataQualityResult' {ruleResults} -> ruleResults) (\s@DataQualityResult' {} a -> s {ruleResults = a} :: DataQualityResult) Prelude.. Lens.mapping Lens.coerced

-- | The unique run ID for the ruleset evaluation for this data quality
-- result.
dataQualityResult_rulesetEvaluationRunId :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.Text)
dataQualityResult_rulesetEvaluationRunId = Lens.lens (\DataQualityResult' {rulesetEvaluationRunId} -> rulesetEvaluationRunId) (\s@DataQualityResult' {} a -> s {rulesetEvaluationRunId = a} :: DataQualityResult)

-- | The name of the ruleset associated with the data quality result.
dataQualityResult_rulesetName :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.Text)
dataQualityResult_rulesetName = Lens.lens (\DataQualityResult' {rulesetName} -> rulesetName) (\s@DataQualityResult' {} a -> s {rulesetName = a} :: DataQualityResult)

-- | An aggregate data quality score. Represents the ratio of rules that
-- passed to the total number of rules.
dataQualityResult_score :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.Double)
dataQualityResult_score = Lens.lens (\DataQualityResult' {score} -> score) (\s@DataQualityResult' {} a -> s {score = a} :: DataQualityResult)

-- | The date and time when this data quality run started.
dataQualityResult_startedOn :: Lens.Lens' DataQualityResult (Prelude.Maybe Prelude.UTCTime)
dataQualityResult_startedOn = Lens.lens (\DataQualityResult' {startedOn} -> startedOn) (\s@DataQualityResult' {} a -> s {startedOn = a} :: DataQualityResult) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DataQualityResult where
  parseJSON =
    Data.withObject
      "DataQualityResult"
      ( \x ->
          DataQualityResult'
            Prelude.<$> (x Data..:? "CompletedOn")
            Prelude.<*> (x Data..:? "DataSource")
            Prelude.<*> (x Data..:? "EvaluationContext")
            Prelude.<*> (x Data..:? "JobName")
            Prelude.<*> (x Data..:? "JobRunId")
            Prelude.<*> (x Data..:? "ResultId")
            Prelude.<*> (x Data..:? "RuleResults")
            Prelude.<*> (x Data..:? "RulesetEvaluationRunId")
            Prelude.<*> (x Data..:? "RulesetName")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..:? "StartedOn")
      )

instance Prelude.Hashable DataQualityResult where
  hashWithSalt _salt DataQualityResult' {..} =
    _salt
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` evaluationContext
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobRunId
      `Prelude.hashWithSalt` resultId
      `Prelude.hashWithSalt` ruleResults
      `Prelude.hashWithSalt` rulesetEvaluationRunId
      `Prelude.hashWithSalt` rulesetName
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` startedOn

instance Prelude.NFData DataQualityResult where
  rnf DataQualityResult' {..} =
    Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf dataSource
      `Prelude.seq` Prelude.rnf evaluationContext
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobRunId
      `Prelude.seq` Prelude.rnf resultId
      `Prelude.seq` Prelude.rnf ruleResults
      `Prelude.seq` Prelude.rnf rulesetEvaluationRunId
      `Prelude.seq` Prelude.rnf rulesetName
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf startedOn
