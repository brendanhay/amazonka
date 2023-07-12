{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.GetDataQualityResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the result of a data quality rule evaluation.
module Amazonka.Glue.GetDataQualityResult
  ( -- * Creating a Request
    GetDataQualityResult (..),
    newGetDataQualityResult,

    -- * Request Lenses
    getDataQualityResult_resultId,

    -- * Destructuring the Response
    GetDataQualityResultResponse (..),
    newGetDataQualityResultResponse,

    -- * Response Lenses
    getDataQualityResultResponse_completedOn,
    getDataQualityResultResponse_dataSource,
    getDataQualityResultResponse_evaluationContext,
    getDataQualityResultResponse_jobName,
    getDataQualityResultResponse_jobRunId,
    getDataQualityResultResponse_resultId,
    getDataQualityResultResponse_ruleResults,
    getDataQualityResultResponse_rulesetEvaluationRunId,
    getDataQualityResultResponse_rulesetName,
    getDataQualityResultResponse_score,
    getDataQualityResultResponse_startedOn,
    getDataQualityResultResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataQualityResult' smart constructor.
data GetDataQualityResult = GetDataQualityResult'
  { -- | A unique result ID for the data quality result.
    resultId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resultId', 'getDataQualityResult_resultId' - A unique result ID for the data quality result.
newGetDataQualityResult ::
  -- | 'resultId'
  Prelude.Text ->
  GetDataQualityResult
newGetDataQualityResult pResultId_ =
  GetDataQualityResult' {resultId = pResultId_}

-- | A unique result ID for the data quality result.
getDataQualityResult_resultId :: Lens.Lens' GetDataQualityResult Prelude.Text
getDataQualityResult_resultId = Lens.lens (\GetDataQualityResult' {resultId} -> resultId) (\s@GetDataQualityResult' {} a -> s {resultId = a} :: GetDataQualityResult)

instance Core.AWSRequest GetDataQualityResult where
  type
    AWSResponse GetDataQualityResult =
      GetDataQualityResultResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataQualityResultResponse'
            Prelude.<$> (x Data..?> "CompletedOn")
            Prelude.<*> (x Data..?> "DataSource")
            Prelude.<*> (x Data..?> "EvaluationContext")
            Prelude.<*> (x Data..?> "JobName")
            Prelude.<*> (x Data..?> "JobRunId")
            Prelude.<*> (x Data..?> "ResultId")
            Prelude.<*> (x Data..?> "RuleResults")
            Prelude.<*> (x Data..?> "RulesetEvaluationRunId")
            Prelude.<*> (x Data..?> "RulesetName")
            Prelude.<*> (x Data..?> "Score")
            Prelude.<*> (x Data..?> "StartedOn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataQualityResult where
  hashWithSalt _salt GetDataQualityResult' {..} =
    _salt `Prelude.hashWithSalt` resultId

instance Prelude.NFData GetDataQualityResult where
  rnf GetDataQualityResult' {..} = Prelude.rnf resultId

instance Data.ToHeaders GetDataQualityResult where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetDataQualityResult" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataQualityResult where
  toJSON GetDataQualityResult' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResultId" Data..= resultId)]
      )

instance Data.ToPath GetDataQualityResult where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDataQualityResult where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataQualityResultResponse' smart constructor.
data GetDataQualityResultResponse = GetDataQualityResultResponse'
  { -- | The date and time when the run for this data quality result was
    -- completed.
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
    -- | The unique run ID associated with the ruleset evaluation.
    rulesetEvaluationRunId :: Prelude.Maybe Prelude.Text,
    -- | The name of the ruleset associated with the data quality result.
    rulesetName :: Prelude.Maybe Prelude.Text,
    -- | An aggregate data quality score. Represents the ratio of rules that
    -- passed to the total number of rules.
    score :: Prelude.Maybe Prelude.Double,
    -- | The date and time when the run for this data quality result started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityResultResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'getDataQualityResultResponse_completedOn' - The date and time when the run for this data quality result was
-- completed.
--
-- 'dataSource', 'getDataQualityResultResponse_dataSource' - The table associated with the data quality result, if any.
--
-- 'evaluationContext', 'getDataQualityResultResponse_evaluationContext' - In the context of a job in Glue Studio, each node in the canvas is
-- typically assigned some sort of name and data quality nodes will have
-- names. In the case of multiple nodes, the @evaluationContext@ can
-- differentiate the nodes.
--
-- 'jobName', 'getDataQualityResultResponse_jobName' - The job name associated with the data quality result, if any.
--
-- 'jobRunId', 'getDataQualityResultResponse_jobRunId' - The job run ID associated with the data quality result, if any.
--
-- 'resultId', 'getDataQualityResultResponse_resultId' - A unique result ID for the data quality result.
--
-- 'ruleResults', 'getDataQualityResultResponse_ruleResults' - A list of @DataQualityRuleResult@ objects representing the results for
-- each rule.
--
-- 'rulesetEvaluationRunId', 'getDataQualityResultResponse_rulesetEvaluationRunId' - The unique run ID associated with the ruleset evaluation.
--
-- 'rulesetName', 'getDataQualityResultResponse_rulesetName' - The name of the ruleset associated with the data quality result.
--
-- 'score', 'getDataQualityResultResponse_score' - An aggregate data quality score. Represents the ratio of rules that
-- passed to the total number of rules.
--
-- 'startedOn', 'getDataQualityResultResponse_startedOn' - The date and time when the run for this data quality result started.
--
-- 'httpStatus', 'getDataQualityResultResponse_httpStatus' - The response's http status code.
newGetDataQualityResultResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataQualityResultResponse
newGetDataQualityResultResponse pHttpStatus_ =
  GetDataQualityResultResponse'
    { completedOn =
        Prelude.Nothing,
      dataSource = Prelude.Nothing,
      evaluationContext = Prelude.Nothing,
      jobName = Prelude.Nothing,
      jobRunId = Prelude.Nothing,
      resultId = Prelude.Nothing,
      ruleResults = Prelude.Nothing,
      rulesetEvaluationRunId = Prelude.Nothing,
      rulesetName = Prelude.Nothing,
      score = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the run for this data quality result was
-- completed.
getDataQualityResultResponse_completedOn :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityResultResponse_completedOn = Lens.lens (\GetDataQualityResultResponse' {completedOn} -> completedOn) (\s@GetDataQualityResultResponse' {} a -> s {completedOn = a} :: GetDataQualityResultResponse) Prelude.. Lens.mapping Data._Time

-- | The table associated with the data quality result, if any.
getDataQualityResultResponse_dataSource :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe DataSource)
getDataQualityResultResponse_dataSource = Lens.lens (\GetDataQualityResultResponse' {dataSource} -> dataSource) (\s@GetDataQualityResultResponse' {} a -> s {dataSource = a} :: GetDataQualityResultResponse)

-- | In the context of a job in Glue Studio, each node in the canvas is
-- typically assigned some sort of name and data quality nodes will have
-- names. In the case of multiple nodes, the @evaluationContext@ can
-- differentiate the nodes.
getDataQualityResultResponse_evaluationContext :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.Text)
getDataQualityResultResponse_evaluationContext = Lens.lens (\GetDataQualityResultResponse' {evaluationContext} -> evaluationContext) (\s@GetDataQualityResultResponse' {} a -> s {evaluationContext = a} :: GetDataQualityResultResponse)

-- | The job name associated with the data quality result, if any.
getDataQualityResultResponse_jobName :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.Text)
getDataQualityResultResponse_jobName = Lens.lens (\GetDataQualityResultResponse' {jobName} -> jobName) (\s@GetDataQualityResultResponse' {} a -> s {jobName = a} :: GetDataQualityResultResponse)

-- | The job run ID associated with the data quality result, if any.
getDataQualityResultResponse_jobRunId :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.Text)
getDataQualityResultResponse_jobRunId = Lens.lens (\GetDataQualityResultResponse' {jobRunId} -> jobRunId) (\s@GetDataQualityResultResponse' {} a -> s {jobRunId = a} :: GetDataQualityResultResponse)

-- | A unique result ID for the data quality result.
getDataQualityResultResponse_resultId :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.Text)
getDataQualityResultResponse_resultId = Lens.lens (\GetDataQualityResultResponse' {resultId} -> resultId) (\s@GetDataQualityResultResponse' {} a -> s {resultId = a} :: GetDataQualityResultResponse)

-- | A list of @DataQualityRuleResult@ objects representing the results for
-- each rule.
getDataQualityResultResponse_ruleResults :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe (Prelude.NonEmpty DataQualityRuleResult))
getDataQualityResultResponse_ruleResults = Lens.lens (\GetDataQualityResultResponse' {ruleResults} -> ruleResults) (\s@GetDataQualityResultResponse' {} a -> s {ruleResults = a} :: GetDataQualityResultResponse) Prelude.. Lens.mapping Lens.coerced

-- | The unique run ID associated with the ruleset evaluation.
getDataQualityResultResponse_rulesetEvaluationRunId :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.Text)
getDataQualityResultResponse_rulesetEvaluationRunId = Lens.lens (\GetDataQualityResultResponse' {rulesetEvaluationRunId} -> rulesetEvaluationRunId) (\s@GetDataQualityResultResponse' {} a -> s {rulesetEvaluationRunId = a} :: GetDataQualityResultResponse)

-- | The name of the ruleset associated with the data quality result.
getDataQualityResultResponse_rulesetName :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.Text)
getDataQualityResultResponse_rulesetName = Lens.lens (\GetDataQualityResultResponse' {rulesetName} -> rulesetName) (\s@GetDataQualityResultResponse' {} a -> s {rulesetName = a} :: GetDataQualityResultResponse)

-- | An aggregate data quality score. Represents the ratio of rules that
-- passed to the total number of rules.
getDataQualityResultResponse_score :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.Double)
getDataQualityResultResponse_score = Lens.lens (\GetDataQualityResultResponse' {score} -> score) (\s@GetDataQualityResultResponse' {} a -> s {score = a} :: GetDataQualityResultResponse)

-- | The date and time when the run for this data quality result started.
getDataQualityResultResponse_startedOn :: Lens.Lens' GetDataQualityResultResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityResultResponse_startedOn = Lens.lens (\GetDataQualityResultResponse' {startedOn} -> startedOn) (\s@GetDataQualityResultResponse' {} a -> s {startedOn = a} :: GetDataQualityResultResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getDataQualityResultResponse_httpStatus :: Lens.Lens' GetDataQualityResultResponse Prelude.Int
getDataQualityResultResponse_httpStatus = Lens.lens (\GetDataQualityResultResponse' {httpStatus} -> httpStatus) (\s@GetDataQualityResultResponse' {} a -> s {httpStatus = a} :: GetDataQualityResultResponse)

instance Prelude.NFData GetDataQualityResultResponse where
  rnf GetDataQualityResultResponse' {..} =
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
      `Prelude.seq` Prelude.rnf httpStatus
