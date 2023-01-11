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
-- Module      : Amazonka.CodeGuruProfiler.Types.Recommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruProfiler.Types.Recommendation where

import Amazonka.CodeGuruProfiler.Types.Match
import Amazonka.CodeGuruProfiler.Types.Pattern
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A potential improvement that was found from analyzing the profiling
-- data.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | How many different places in the profile graph triggered a match.
    allMatchesCount :: Prelude.Int,
    -- | How much of the total sample count is potentially affected.
    allMatchesSum :: Prelude.Double,
    -- | End time of the profile that was used by this analysis. This is
    -- specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    endTime :: Data.ISO8601,
    -- | The pattern that analysis recognized in the profile to make this
    -- recommendation.
    pattern' :: Pattern,
    -- | The start time of the profile that was used by this analysis. This is
    -- specified using the ISO 8601 format. For example,
    -- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
    -- 1:15:02 PM UTC.
    startTime :: Data.ISO8601,
    -- | List of the matches with most impact.
    topMatches :: [Match]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Recommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allMatchesCount', 'recommendation_allMatchesCount' - How many different places in the profile graph triggered a match.
--
-- 'allMatchesSum', 'recommendation_allMatchesSum' - How much of the total sample count is potentially affected.
--
-- 'endTime', 'recommendation_endTime' - End time of the profile that was used by this analysis. This is
-- specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
--
-- 'pattern'', 'recommendation_pattern' - The pattern that analysis recognized in the profile to make this
-- recommendation.
--
-- 'startTime', 'recommendation_startTime' - The start time of the profile that was used by this analysis. This is
-- specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
--
-- 'topMatches', 'recommendation_topMatches' - List of the matches with most impact.
newRecommendation ::
  -- | 'allMatchesCount'
  Prelude.Int ->
  -- | 'allMatchesSum'
  Prelude.Double ->
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'pattern''
  Pattern ->
  -- | 'startTime'
  Prelude.UTCTime ->
  Recommendation
newRecommendation
  pAllMatchesCount_
  pAllMatchesSum_
  pEndTime_
  pPattern_
  pStartTime_ =
    Recommendation'
      { allMatchesCount =
          pAllMatchesCount_,
        allMatchesSum = pAllMatchesSum_,
        endTime = Data._Time Lens.# pEndTime_,
        pattern' = pPattern_,
        startTime = Data._Time Lens.# pStartTime_,
        topMatches = Prelude.mempty
      }

-- | How many different places in the profile graph triggered a match.
recommendation_allMatchesCount :: Lens.Lens' Recommendation Prelude.Int
recommendation_allMatchesCount = Lens.lens (\Recommendation' {allMatchesCount} -> allMatchesCount) (\s@Recommendation' {} a -> s {allMatchesCount = a} :: Recommendation)

-- | How much of the total sample count is potentially affected.
recommendation_allMatchesSum :: Lens.Lens' Recommendation Prelude.Double
recommendation_allMatchesSum = Lens.lens (\Recommendation' {allMatchesSum} -> allMatchesSum) (\s@Recommendation' {} a -> s {allMatchesSum = a} :: Recommendation)

-- | End time of the profile that was used by this analysis. This is
-- specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
recommendation_endTime :: Lens.Lens' Recommendation Prelude.UTCTime
recommendation_endTime = Lens.lens (\Recommendation' {endTime} -> endTime) (\s@Recommendation' {} a -> s {endTime = a} :: Recommendation) Prelude.. Data._Time

-- | The pattern that analysis recognized in the profile to make this
-- recommendation.
recommendation_pattern :: Lens.Lens' Recommendation Pattern
recommendation_pattern = Lens.lens (\Recommendation' {pattern'} -> pattern') (\s@Recommendation' {} a -> s {pattern' = a} :: Recommendation)

-- | The start time of the profile that was used by this analysis. This is
-- specified using the ISO 8601 format. For example,
-- 2020-06-01T13:15:02.001Z represents 1 millisecond past June 1, 2020
-- 1:15:02 PM UTC.
recommendation_startTime :: Lens.Lens' Recommendation Prelude.UTCTime
recommendation_startTime = Lens.lens (\Recommendation' {startTime} -> startTime) (\s@Recommendation' {} a -> s {startTime = a} :: Recommendation) Prelude.. Data._Time

-- | List of the matches with most impact.
recommendation_topMatches :: Lens.Lens' Recommendation [Match]
recommendation_topMatches = Lens.lens (\Recommendation' {topMatches} -> topMatches) (\s@Recommendation' {} a -> s {topMatches = a} :: Recommendation) Prelude.. Lens.coerced

instance Data.FromJSON Recommendation where
  parseJSON =
    Data.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Data..: "allMatchesCount")
            Prelude.<*> (x Data..: "allMatchesSum")
            Prelude.<*> (x Data..: "endTime")
            Prelude.<*> (x Data..: "pattern")
            Prelude.<*> (x Data..: "startTime")
            Prelude.<*> (x Data..:? "topMatches" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt `Prelude.hashWithSalt` allMatchesCount
      `Prelude.hashWithSalt` allMatchesSum
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` pattern'
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` topMatches

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf allMatchesCount
      `Prelude.seq` Prelude.rnf allMatchesSum
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf pattern'
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf topMatches
