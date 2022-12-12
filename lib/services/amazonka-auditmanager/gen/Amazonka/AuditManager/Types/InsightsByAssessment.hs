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
-- Module      : Amazonka.AuditManager.Types.InsightsByAssessment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.InsightsByAssessment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of the latest analytics data for a specific active assessment.
--
-- This summary is a snapshot of the data that was collected on the
-- @lastUpdated@ date. It’s important to understand that the totals in
-- @InsightsByAssessment@ are daily counts based on this date — they aren’t
-- a total sum to date.
--
-- The @InsightsByAssessment@ data is eventually consistent. This means
-- that when you read data from @InsightsByAssessment@, the response might
-- not instantly reflect the results of a recently completed write or
-- update operation. If you repeat your read request after a few hours, the
-- response returns the latest data.
--
-- If you delete an assessment or change its status to inactive,
-- @InsightsByAssessment@ includes data for that assessment as follows.
--
-- -   __Inactive assessments__ - If Audit Manager collected evidence for
--     your assessment before you changed it inactive, that evidence is
--     included in the @InsightsByAssessment@ counts for that day.
--
-- -   __Deleted assessments__ - If Audit Manager collected evidence for
--     your assessment before you deleted it, that evidence isn\'t included
--     in the @InsightsByAssessment@ counts for that day.
--
-- /See:/ 'newInsightsByAssessment' smart constructor.
data InsightsByAssessment = InsightsByAssessment'
  { -- | The number of assessment controls that collected non-compliant evidence
    -- on the @lastUpdated@ date.
    assessmentControlsCountByNoncompliantEvidence :: Prelude.Maybe Prelude.Int,
    -- | The number of compliance check evidence that Audit Manager classified as
    -- compliant. This includes evidence that was collected from Security Hub
    -- with a /Pass/ ruling, or collected from Config with a /Compliant/
    -- ruling.
    compliantEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The amount of evidence without a compliance check ruling. Evidence is
    -- inconclusive if the associated control uses Security Hub or Config as a
    -- data source and you didn\'t enable those services. This is also the case
    -- if a control uses a data source that doesn’t support compliance checks
    -- (for example, manual evidence, API calls, or CloudTrail).
    --
    -- If evidence has a compliance check status of /not applicable/, it\'s
    -- classified as /inconclusive/ in @InsightsByAssessment@ data.
    inconclusiveEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The time when the assessment insights were last updated.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The number of compliance check evidence that Audit Manager classified as
    -- non-compliant. This includes evidence that was collected from Security
    -- Hub with a /Fail/ ruling, or collected from Config with a
    -- /Non-compliant/ ruling.
    noncompliantEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The total number of controls in the assessment.
    totalAssessmentControlsCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightsByAssessment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentControlsCountByNoncompliantEvidence', 'insightsByAssessment_assessmentControlsCountByNoncompliantEvidence' - The number of assessment controls that collected non-compliant evidence
-- on the @lastUpdated@ date.
--
-- 'compliantEvidenceCount', 'insightsByAssessment_compliantEvidenceCount' - The number of compliance check evidence that Audit Manager classified as
-- compliant. This includes evidence that was collected from Security Hub
-- with a /Pass/ ruling, or collected from Config with a /Compliant/
-- ruling.
--
-- 'inconclusiveEvidenceCount', 'insightsByAssessment_inconclusiveEvidenceCount' - The amount of evidence without a compliance check ruling. Evidence is
-- inconclusive if the associated control uses Security Hub or Config as a
-- data source and you didn\'t enable those services. This is also the case
-- if a control uses a data source that doesn’t support compliance checks
-- (for example, manual evidence, API calls, or CloudTrail).
--
-- If evidence has a compliance check status of /not applicable/, it\'s
-- classified as /inconclusive/ in @InsightsByAssessment@ data.
--
-- 'lastUpdated', 'insightsByAssessment_lastUpdated' - The time when the assessment insights were last updated.
--
-- 'noncompliantEvidenceCount', 'insightsByAssessment_noncompliantEvidenceCount' - The number of compliance check evidence that Audit Manager classified as
-- non-compliant. This includes evidence that was collected from Security
-- Hub with a /Fail/ ruling, or collected from Config with a
-- /Non-compliant/ ruling.
--
-- 'totalAssessmentControlsCount', 'insightsByAssessment_totalAssessmentControlsCount' - The total number of controls in the assessment.
newInsightsByAssessment ::
  InsightsByAssessment
newInsightsByAssessment =
  InsightsByAssessment'
    { assessmentControlsCountByNoncompliantEvidence =
        Prelude.Nothing,
      compliantEvidenceCount = Prelude.Nothing,
      inconclusiveEvidenceCount = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      noncompliantEvidenceCount = Prelude.Nothing,
      totalAssessmentControlsCount = Prelude.Nothing
    }

-- | The number of assessment controls that collected non-compliant evidence
-- on the @lastUpdated@ date.
insightsByAssessment_assessmentControlsCountByNoncompliantEvidence :: Lens.Lens' InsightsByAssessment (Prelude.Maybe Prelude.Int)
insightsByAssessment_assessmentControlsCountByNoncompliantEvidence = Lens.lens (\InsightsByAssessment' {assessmentControlsCountByNoncompliantEvidence} -> assessmentControlsCountByNoncompliantEvidence) (\s@InsightsByAssessment' {} a -> s {assessmentControlsCountByNoncompliantEvidence = a} :: InsightsByAssessment)

-- | The number of compliance check evidence that Audit Manager classified as
-- compliant. This includes evidence that was collected from Security Hub
-- with a /Pass/ ruling, or collected from Config with a /Compliant/
-- ruling.
insightsByAssessment_compliantEvidenceCount :: Lens.Lens' InsightsByAssessment (Prelude.Maybe Prelude.Int)
insightsByAssessment_compliantEvidenceCount = Lens.lens (\InsightsByAssessment' {compliantEvidenceCount} -> compliantEvidenceCount) (\s@InsightsByAssessment' {} a -> s {compliantEvidenceCount = a} :: InsightsByAssessment)

-- | The amount of evidence without a compliance check ruling. Evidence is
-- inconclusive if the associated control uses Security Hub or Config as a
-- data source and you didn\'t enable those services. This is also the case
-- if a control uses a data source that doesn’t support compliance checks
-- (for example, manual evidence, API calls, or CloudTrail).
--
-- If evidence has a compliance check status of /not applicable/, it\'s
-- classified as /inconclusive/ in @InsightsByAssessment@ data.
insightsByAssessment_inconclusiveEvidenceCount :: Lens.Lens' InsightsByAssessment (Prelude.Maybe Prelude.Int)
insightsByAssessment_inconclusiveEvidenceCount = Lens.lens (\InsightsByAssessment' {inconclusiveEvidenceCount} -> inconclusiveEvidenceCount) (\s@InsightsByAssessment' {} a -> s {inconclusiveEvidenceCount = a} :: InsightsByAssessment)

-- | The time when the assessment insights were last updated.
insightsByAssessment_lastUpdated :: Lens.Lens' InsightsByAssessment (Prelude.Maybe Prelude.UTCTime)
insightsByAssessment_lastUpdated = Lens.lens (\InsightsByAssessment' {lastUpdated} -> lastUpdated) (\s@InsightsByAssessment' {} a -> s {lastUpdated = a} :: InsightsByAssessment) Prelude.. Lens.mapping Data._Time

-- | The number of compliance check evidence that Audit Manager classified as
-- non-compliant. This includes evidence that was collected from Security
-- Hub with a /Fail/ ruling, or collected from Config with a
-- /Non-compliant/ ruling.
insightsByAssessment_noncompliantEvidenceCount :: Lens.Lens' InsightsByAssessment (Prelude.Maybe Prelude.Int)
insightsByAssessment_noncompliantEvidenceCount = Lens.lens (\InsightsByAssessment' {noncompliantEvidenceCount} -> noncompliantEvidenceCount) (\s@InsightsByAssessment' {} a -> s {noncompliantEvidenceCount = a} :: InsightsByAssessment)

-- | The total number of controls in the assessment.
insightsByAssessment_totalAssessmentControlsCount :: Lens.Lens' InsightsByAssessment (Prelude.Maybe Prelude.Int)
insightsByAssessment_totalAssessmentControlsCount = Lens.lens (\InsightsByAssessment' {totalAssessmentControlsCount} -> totalAssessmentControlsCount) (\s@InsightsByAssessment' {} a -> s {totalAssessmentControlsCount = a} :: InsightsByAssessment)

instance Data.FromJSON InsightsByAssessment where
  parseJSON =
    Data.withObject
      "InsightsByAssessment"
      ( \x ->
          InsightsByAssessment'
            Prelude.<$> ( x
                            Data..:? "assessmentControlsCountByNoncompliantEvidence"
                        )
            Prelude.<*> (x Data..:? "compliantEvidenceCount")
            Prelude.<*> (x Data..:? "inconclusiveEvidenceCount")
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "noncompliantEvidenceCount")
            Prelude.<*> (x Data..:? "totalAssessmentControlsCount")
      )

instance Prelude.Hashable InsightsByAssessment where
  hashWithSalt _salt InsightsByAssessment' {..} =
    _salt
      `Prelude.hashWithSalt` assessmentControlsCountByNoncompliantEvidence
      `Prelude.hashWithSalt` compliantEvidenceCount
      `Prelude.hashWithSalt` inconclusiveEvidenceCount
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` noncompliantEvidenceCount
      `Prelude.hashWithSalt` totalAssessmentControlsCount

instance Prelude.NFData InsightsByAssessment where
  rnf InsightsByAssessment' {..} =
    Prelude.rnf
      assessmentControlsCountByNoncompliantEvidence
      `Prelude.seq` Prelude.rnf compliantEvidenceCount
      `Prelude.seq` Prelude.rnf inconclusiveEvidenceCount
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf noncompliantEvidenceCount
      `Prelude.seq` Prelude.rnf totalAssessmentControlsCount
