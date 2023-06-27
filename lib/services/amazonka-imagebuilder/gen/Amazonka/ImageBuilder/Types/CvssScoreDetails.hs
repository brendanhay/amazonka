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
-- Module      : Amazonka.ImageBuilder.Types.CvssScoreDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.CvssScoreDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.CvssScoreAdjustment
import qualified Amazonka.Prelude as Prelude

-- | Details about the source of the score, and the factors that determined
-- the adjustments to create the final score.
--
-- /See:/ 'newCvssScoreDetails' smart constructor.
data CvssScoreDetails = CvssScoreDetails'
  { -- | An object that contains details about an adjustment that Amazon
    -- Inspector made to the CVSS score for the finding.
    adjustments :: Prelude.Maybe [CvssScoreAdjustment],
    -- | The source of the finding.
    cvssSource :: Prelude.Maybe Prelude.Text,
    -- | The CVSS score.
    score :: Prelude.Maybe Prelude.Double,
    -- | The source for the CVSS score.
    scoreSource :: Prelude.Maybe Prelude.Text,
    -- | A vector that measures the severity of the vulnerability.
    scoringVector :: Prelude.Maybe Prelude.Text,
    -- | The CVSS version that generated the score.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CvssScoreDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adjustments', 'cvssScoreDetails_adjustments' - An object that contains details about an adjustment that Amazon
-- Inspector made to the CVSS score for the finding.
--
-- 'cvssSource', 'cvssScoreDetails_cvssSource' - The source of the finding.
--
-- 'score', 'cvssScoreDetails_score' - The CVSS score.
--
-- 'scoreSource', 'cvssScoreDetails_scoreSource' - The source for the CVSS score.
--
-- 'scoringVector', 'cvssScoreDetails_scoringVector' - A vector that measures the severity of the vulnerability.
--
-- 'version', 'cvssScoreDetails_version' - The CVSS version that generated the score.
newCvssScoreDetails ::
  CvssScoreDetails
newCvssScoreDetails =
  CvssScoreDetails'
    { adjustments = Prelude.Nothing,
      cvssSource = Prelude.Nothing,
      score = Prelude.Nothing,
      scoreSource = Prelude.Nothing,
      scoringVector = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | An object that contains details about an adjustment that Amazon
-- Inspector made to the CVSS score for the finding.
cvssScoreDetails_adjustments :: Lens.Lens' CvssScoreDetails (Prelude.Maybe [CvssScoreAdjustment])
cvssScoreDetails_adjustments = Lens.lens (\CvssScoreDetails' {adjustments} -> adjustments) (\s@CvssScoreDetails' {} a -> s {adjustments = a} :: CvssScoreDetails) Prelude.. Lens.mapping Lens.coerced

-- | The source of the finding.
cvssScoreDetails_cvssSource :: Lens.Lens' CvssScoreDetails (Prelude.Maybe Prelude.Text)
cvssScoreDetails_cvssSource = Lens.lens (\CvssScoreDetails' {cvssSource} -> cvssSource) (\s@CvssScoreDetails' {} a -> s {cvssSource = a} :: CvssScoreDetails)

-- | The CVSS score.
cvssScoreDetails_score :: Lens.Lens' CvssScoreDetails (Prelude.Maybe Prelude.Double)
cvssScoreDetails_score = Lens.lens (\CvssScoreDetails' {score} -> score) (\s@CvssScoreDetails' {} a -> s {score = a} :: CvssScoreDetails)

-- | The source for the CVSS score.
cvssScoreDetails_scoreSource :: Lens.Lens' CvssScoreDetails (Prelude.Maybe Prelude.Text)
cvssScoreDetails_scoreSource = Lens.lens (\CvssScoreDetails' {scoreSource} -> scoreSource) (\s@CvssScoreDetails' {} a -> s {scoreSource = a} :: CvssScoreDetails)

-- | A vector that measures the severity of the vulnerability.
cvssScoreDetails_scoringVector :: Lens.Lens' CvssScoreDetails (Prelude.Maybe Prelude.Text)
cvssScoreDetails_scoringVector = Lens.lens (\CvssScoreDetails' {scoringVector} -> scoringVector) (\s@CvssScoreDetails' {} a -> s {scoringVector = a} :: CvssScoreDetails)

-- | The CVSS version that generated the score.
cvssScoreDetails_version :: Lens.Lens' CvssScoreDetails (Prelude.Maybe Prelude.Text)
cvssScoreDetails_version = Lens.lens (\CvssScoreDetails' {version} -> version) (\s@CvssScoreDetails' {} a -> s {version = a} :: CvssScoreDetails)

instance Data.FromJSON CvssScoreDetails where
  parseJSON =
    Data.withObject
      "CvssScoreDetails"
      ( \x ->
          CvssScoreDetails'
            Prelude.<$> (x Data..:? "adjustments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "cvssSource")
            Prelude.<*> (x Data..:? "score")
            Prelude.<*> (x Data..:? "scoreSource")
            Prelude.<*> (x Data..:? "scoringVector")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable CvssScoreDetails where
  hashWithSalt _salt CvssScoreDetails' {..} =
    _salt
      `Prelude.hashWithSalt` adjustments
      `Prelude.hashWithSalt` cvssSource
      `Prelude.hashWithSalt` score
      `Prelude.hashWithSalt` scoreSource
      `Prelude.hashWithSalt` scoringVector
      `Prelude.hashWithSalt` version

instance Prelude.NFData CvssScoreDetails where
  rnf CvssScoreDetails' {..} =
    Prelude.rnf adjustments
      `Prelude.seq` Prelude.rnf cvssSource
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf scoreSource
      `Prelude.seq` Prelude.rnf scoringVector
      `Prelude.seq` Prelude.rnf version
