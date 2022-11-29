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
-- Module      : Amazonka.AuditManager.Types.Insights
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.Insights where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of the latest analytics data for all your active assessments.
--
-- This summary is a snapshot of the data that your active assessments
-- collected on the @lastUpdated@ date. It’s important to understand that
-- the following totals are daily counts based on this date — they aren’t a
-- total sum to date.
--
-- The @Insights@ data is eventually consistent. This means that, when you
-- read data from @Insights@, the response might not instantly reflect the
-- results of a recently completed write or update operation. If you repeat
-- your read request after a few hours, the response should return the
-- latest data.
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
-- /See:/ 'newInsights' smart constructor.
data Insights = Insights'
  { -- | The total number of controls across all active assessments.
    totalAssessmentControlsCount :: Prelude.Maybe Prelude.Int,
    -- | The number of compliance check evidence that Audit Manager classified as
    -- compliant on the @lastUpdated@ date. This includes evidence that was
    -- collected from Security Hub with a /Pass/ ruling, or collected from
    -- Config with a /Compliant/ ruling.
    compliantEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The time when the cross-assessment insights were last updated.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | The number of active assessments in Audit Manager.
    activeAssessmentsCount :: Prelude.Maybe Prelude.Int,
    -- | The number of evidence without a compliance check ruling. Evidence is
    -- inconclusive when the associated control uses Security Hub or Config as
    -- a data source but you didn\'t enable those services. This is also the
    -- case when a control uses a data source that doesn’t support compliance
    -- checks (for example: manual evidence, API calls, or CloudTrail).
    --
    -- If evidence has a compliance check status of /not applicable/, it\'s
    -- classed as /inconclusive/ in @Insights@ data.
    inconclusiveEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The number of compliance check evidence that Audit Manager classified as
    -- non-compliant on the @lastUpdated@ date. This includes evidence that was
    -- collected from Security Hub with a /Fail/ ruling, or collected from
    -- Config with a /Non-compliant/ ruling.
    noncompliantEvidenceCount :: Prelude.Maybe Prelude.Int,
    -- | The number of assessment controls that collected non-compliant evidence
    -- on the @lastUpdated@ date.
    assessmentControlsCountByNoncompliantEvidence :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Insights' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalAssessmentControlsCount', 'insights_totalAssessmentControlsCount' - The total number of controls across all active assessments.
--
-- 'compliantEvidenceCount', 'insights_compliantEvidenceCount' - The number of compliance check evidence that Audit Manager classified as
-- compliant on the @lastUpdated@ date. This includes evidence that was
-- collected from Security Hub with a /Pass/ ruling, or collected from
-- Config with a /Compliant/ ruling.
--
-- 'lastUpdated', 'insights_lastUpdated' - The time when the cross-assessment insights were last updated.
--
-- 'activeAssessmentsCount', 'insights_activeAssessmentsCount' - The number of active assessments in Audit Manager.
--
-- 'inconclusiveEvidenceCount', 'insights_inconclusiveEvidenceCount' - The number of evidence without a compliance check ruling. Evidence is
-- inconclusive when the associated control uses Security Hub or Config as
-- a data source but you didn\'t enable those services. This is also the
-- case when a control uses a data source that doesn’t support compliance
-- checks (for example: manual evidence, API calls, or CloudTrail).
--
-- If evidence has a compliance check status of /not applicable/, it\'s
-- classed as /inconclusive/ in @Insights@ data.
--
-- 'noncompliantEvidenceCount', 'insights_noncompliantEvidenceCount' - The number of compliance check evidence that Audit Manager classified as
-- non-compliant on the @lastUpdated@ date. This includes evidence that was
-- collected from Security Hub with a /Fail/ ruling, or collected from
-- Config with a /Non-compliant/ ruling.
--
-- 'assessmentControlsCountByNoncompliantEvidence', 'insights_assessmentControlsCountByNoncompliantEvidence' - The number of assessment controls that collected non-compliant evidence
-- on the @lastUpdated@ date.
newInsights ::
  Insights
newInsights =
  Insights'
    { totalAssessmentControlsCount =
        Prelude.Nothing,
      compliantEvidenceCount = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      activeAssessmentsCount = Prelude.Nothing,
      inconclusiveEvidenceCount = Prelude.Nothing,
      noncompliantEvidenceCount = Prelude.Nothing,
      assessmentControlsCountByNoncompliantEvidence =
        Prelude.Nothing
    }

-- | The total number of controls across all active assessments.
insights_totalAssessmentControlsCount :: Lens.Lens' Insights (Prelude.Maybe Prelude.Int)
insights_totalAssessmentControlsCount = Lens.lens (\Insights' {totalAssessmentControlsCount} -> totalAssessmentControlsCount) (\s@Insights' {} a -> s {totalAssessmentControlsCount = a} :: Insights)

-- | The number of compliance check evidence that Audit Manager classified as
-- compliant on the @lastUpdated@ date. This includes evidence that was
-- collected from Security Hub with a /Pass/ ruling, or collected from
-- Config with a /Compliant/ ruling.
insights_compliantEvidenceCount :: Lens.Lens' Insights (Prelude.Maybe Prelude.Int)
insights_compliantEvidenceCount = Lens.lens (\Insights' {compliantEvidenceCount} -> compliantEvidenceCount) (\s@Insights' {} a -> s {compliantEvidenceCount = a} :: Insights)

-- | The time when the cross-assessment insights were last updated.
insights_lastUpdated :: Lens.Lens' Insights (Prelude.Maybe Prelude.UTCTime)
insights_lastUpdated = Lens.lens (\Insights' {lastUpdated} -> lastUpdated) (\s@Insights' {} a -> s {lastUpdated = a} :: Insights) Prelude.. Lens.mapping Core._Time

-- | The number of active assessments in Audit Manager.
insights_activeAssessmentsCount :: Lens.Lens' Insights (Prelude.Maybe Prelude.Int)
insights_activeAssessmentsCount = Lens.lens (\Insights' {activeAssessmentsCount} -> activeAssessmentsCount) (\s@Insights' {} a -> s {activeAssessmentsCount = a} :: Insights)

-- | The number of evidence without a compliance check ruling. Evidence is
-- inconclusive when the associated control uses Security Hub or Config as
-- a data source but you didn\'t enable those services. This is also the
-- case when a control uses a data source that doesn’t support compliance
-- checks (for example: manual evidence, API calls, or CloudTrail).
--
-- If evidence has a compliance check status of /not applicable/, it\'s
-- classed as /inconclusive/ in @Insights@ data.
insights_inconclusiveEvidenceCount :: Lens.Lens' Insights (Prelude.Maybe Prelude.Int)
insights_inconclusiveEvidenceCount = Lens.lens (\Insights' {inconclusiveEvidenceCount} -> inconclusiveEvidenceCount) (\s@Insights' {} a -> s {inconclusiveEvidenceCount = a} :: Insights)

-- | The number of compliance check evidence that Audit Manager classified as
-- non-compliant on the @lastUpdated@ date. This includes evidence that was
-- collected from Security Hub with a /Fail/ ruling, or collected from
-- Config with a /Non-compliant/ ruling.
insights_noncompliantEvidenceCount :: Lens.Lens' Insights (Prelude.Maybe Prelude.Int)
insights_noncompliantEvidenceCount = Lens.lens (\Insights' {noncompliantEvidenceCount} -> noncompliantEvidenceCount) (\s@Insights' {} a -> s {noncompliantEvidenceCount = a} :: Insights)

-- | The number of assessment controls that collected non-compliant evidence
-- on the @lastUpdated@ date.
insights_assessmentControlsCountByNoncompliantEvidence :: Lens.Lens' Insights (Prelude.Maybe Prelude.Int)
insights_assessmentControlsCountByNoncompliantEvidence = Lens.lens (\Insights' {assessmentControlsCountByNoncompliantEvidence} -> assessmentControlsCountByNoncompliantEvidence) (\s@Insights' {} a -> s {assessmentControlsCountByNoncompliantEvidence = a} :: Insights)

instance Core.FromJSON Insights where
  parseJSON =
    Core.withObject
      "Insights"
      ( \x ->
          Insights'
            Prelude.<$> (x Core..:? "totalAssessmentControlsCount")
            Prelude.<*> (x Core..:? "compliantEvidenceCount")
            Prelude.<*> (x Core..:? "lastUpdated")
            Prelude.<*> (x Core..:? "activeAssessmentsCount")
            Prelude.<*> (x Core..:? "inconclusiveEvidenceCount")
            Prelude.<*> (x Core..:? "noncompliantEvidenceCount")
            Prelude.<*> ( x
                            Core..:? "assessmentControlsCountByNoncompliantEvidence"
                        )
      )

instance Prelude.Hashable Insights where
  hashWithSalt _salt Insights' {..} =
    _salt
      `Prelude.hashWithSalt` totalAssessmentControlsCount
      `Prelude.hashWithSalt` compliantEvidenceCount
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` activeAssessmentsCount
      `Prelude.hashWithSalt` inconclusiveEvidenceCount
      `Prelude.hashWithSalt` noncompliantEvidenceCount
      `Prelude.hashWithSalt` assessmentControlsCountByNoncompliantEvidence

instance Prelude.NFData Insights where
  rnf Insights' {..} =
    Prelude.rnf totalAssessmentControlsCount
      `Prelude.seq` Prelude.rnf compliantEvidenceCount
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf activeAssessmentsCount
      `Prelude.seq` Prelude.rnf inconclusiveEvidenceCount
      `Prelude.seq` Prelude.rnf noncompliantEvidenceCount
      `Prelude.seq` Prelude.rnf
        assessmentControlsCountByNoncompliantEvidence
