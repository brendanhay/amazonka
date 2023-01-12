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
-- Module      : Amazonka.ApplicationInsights.Types.Problem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApplicationInsights.Types.Problem where

import Amazonka.ApplicationInsights.Types.FeedbackKey
import Amazonka.ApplicationInsights.Types.FeedbackValue
import Amazonka.ApplicationInsights.Types.SeverityLevel
import Amazonka.ApplicationInsights.Types.Status
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a problem that is detected by correlating observations.
--
-- /See:/ 'newProblem' smart constructor.
data Problem = Problem'
  { -- | The resource affected by the problem.
    affectedResource :: Prelude.Maybe Prelude.Text,
    -- | The time when the problem ended, in epoch seconds.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Feedback provided by the user about the problem.
    feedback :: Prelude.Maybe (Prelude.HashMap FeedbackKey FeedbackValue),
    -- | The ID of the problem.
    id :: Prelude.Maybe Prelude.Text,
    -- | A detailed analysis of the problem using machine learning.
    insights :: Prelude.Maybe Prelude.Text,
    -- | The last time that the problem reoccurred after its last resolution.
    lastRecurrenceTime :: Prelude.Maybe Data.POSIX,
    -- | The number of times that the same problem reoccurred after the first
    -- time it was resolved.
    recurringCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the resource group affected by the problem.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | A measure of the level of impact of the problem.
    severityLevel :: Prelude.Maybe SeverityLevel,
    -- | The time when the problem started, in epoch seconds.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the problem.
    status :: Prelude.Maybe Status,
    -- | The name of the problem.
    title :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Problem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affectedResource', 'problem_affectedResource' - The resource affected by the problem.
--
-- 'endTime', 'problem_endTime' - The time when the problem ended, in epoch seconds.
--
-- 'feedback', 'problem_feedback' - Feedback provided by the user about the problem.
--
-- 'id', 'problem_id' - The ID of the problem.
--
-- 'insights', 'problem_insights' - A detailed analysis of the problem using machine learning.
--
-- 'lastRecurrenceTime', 'problem_lastRecurrenceTime' - The last time that the problem reoccurred after its last resolution.
--
-- 'recurringCount', 'problem_recurringCount' - The number of times that the same problem reoccurred after the first
-- time it was resolved.
--
-- 'resourceGroupName', 'problem_resourceGroupName' - The name of the resource group affected by the problem.
--
-- 'severityLevel', 'problem_severityLevel' - A measure of the level of impact of the problem.
--
-- 'startTime', 'problem_startTime' - The time when the problem started, in epoch seconds.
--
-- 'status', 'problem_status' - The status of the problem.
--
-- 'title', 'problem_title' - The name of the problem.
newProblem ::
  Problem
newProblem =
  Problem'
    { affectedResource = Prelude.Nothing,
      endTime = Prelude.Nothing,
      feedback = Prelude.Nothing,
      id = Prelude.Nothing,
      insights = Prelude.Nothing,
      lastRecurrenceTime = Prelude.Nothing,
      recurringCount = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      severityLevel = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      title = Prelude.Nothing
    }

-- | The resource affected by the problem.
problem_affectedResource :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_affectedResource = Lens.lens (\Problem' {affectedResource} -> affectedResource) (\s@Problem' {} a -> s {affectedResource = a} :: Problem)

-- | The time when the problem ended, in epoch seconds.
problem_endTime :: Lens.Lens' Problem (Prelude.Maybe Prelude.UTCTime)
problem_endTime = Lens.lens (\Problem' {endTime} -> endTime) (\s@Problem' {} a -> s {endTime = a} :: Problem) Prelude.. Lens.mapping Data._Time

-- | Feedback provided by the user about the problem.
problem_feedback :: Lens.Lens' Problem (Prelude.Maybe (Prelude.HashMap FeedbackKey FeedbackValue))
problem_feedback = Lens.lens (\Problem' {feedback} -> feedback) (\s@Problem' {} a -> s {feedback = a} :: Problem) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the problem.
problem_id :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_id = Lens.lens (\Problem' {id} -> id) (\s@Problem' {} a -> s {id = a} :: Problem)

-- | A detailed analysis of the problem using machine learning.
problem_insights :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_insights = Lens.lens (\Problem' {insights} -> insights) (\s@Problem' {} a -> s {insights = a} :: Problem)

-- | The last time that the problem reoccurred after its last resolution.
problem_lastRecurrenceTime :: Lens.Lens' Problem (Prelude.Maybe Prelude.UTCTime)
problem_lastRecurrenceTime = Lens.lens (\Problem' {lastRecurrenceTime} -> lastRecurrenceTime) (\s@Problem' {} a -> s {lastRecurrenceTime = a} :: Problem) Prelude.. Lens.mapping Data._Time

-- | The number of times that the same problem reoccurred after the first
-- time it was resolved.
problem_recurringCount :: Lens.Lens' Problem (Prelude.Maybe Prelude.Integer)
problem_recurringCount = Lens.lens (\Problem' {recurringCount} -> recurringCount) (\s@Problem' {} a -> s {recurringCount = a} :: Problem)

-- | The name of the resource group affected by the problem.
problem_resourceGroupName :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_resourceGroupName = Lens.lens (\Problem' {resourceGroupName} -> resourceGroupName) (\s@Problem' {} a -> s {resourceGroupName = a} :: Problem)

-- | A measure of the level of impact of the problem.
problem_severityLevel :: Lens.Lens' Problem (Prelude.Maybe SeverityLevel)
problem_severityLevel = Lens.lens (\Problem' {severityLevel} -> severityLevel) (\s@Problem' {} a -> s {severityLevel = a} :: Problem)

-- | The time when the problem started, in epoch seconds.
problem_startTime :: Lens.Lens' Problem (Prelude.Maybe Prelude.UTCTime)
problem_startTime = Lens.lens (\Problem' {startTime} -> startTime) (\s@Problem' {} a -> s {startTime = a} :: Problem) Prelude.. Lens.mapping Data._Time

-- | The status of the problem.
problem_status :: Lens.Lens' Problem (Prelude.Maybe Status)
problem_status = Lens.lens (\Problem' {status} -> status) (\s@Problem' {} a -> s {status = a} :: Problem)

-- | The name of the problem.
problem_title :: Lens.Lens' Problem (Prelude.Maybe Prelude.Text)
problem_title = Lens.lens (\Problem' {title} -> title) (\s@Problem' {} a -> s {title = a} :: Problem)

instance Data.FromJSON Problem where
  parseJSON =
    Data.withObject
      "Problem"
      ( \x ->
          Problem'
            Prelude.<$> (x Data..:? "AffectedResource")
            Prelude.<*> (x Data..:? "EndTime")
            Prelude.<*> (x Data..:? "Feedback" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Insights")
            Prelude.<*> (x Data..:? "LastRecurrenceTime")
            Prelude.<*> (x Data..:? "RecurringCount")
            Prelude.<*> (x Data..:? "ResourceGroupName")
            Prelude.<*> (x Data..:? "SeverityLevel")
            Prelude.<*> (x Data..:? "StartTime")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Title")
      )

instance Prelude.Hashable Problem where
  hashWithSalt _salt Problem' {..} =
    _salt `Prelude.hashWithSalt` affectedResource
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` feedback
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` insights
      `Prelude.hashWithSalt` lastRecurrenceTime
      `Prelude.hashWithSalt` recurringCount
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` severityLevel
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` title

instance Prelude.NFData Problem where
  rnf Problem' {..} =
    Prelude.rnf affectedResource
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf feedback
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf insights
      `Prelude.seq` Prelude.rnf lastRecurrenceTime
      `Prelude.seq` Prelude.rnf recurringCount
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf severityLevel
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf title
