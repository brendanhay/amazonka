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
-- Module      : Amazonka.Inspector2.Types.CvssScoreDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.CvssScoreDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.CvssScoreAdjustment
import qualified Amazonka.Prelude as Prelude

-- | Information about the CVSS score.
--
-- /See:/ 'newCvssScoreDetails' smart constructor.
data CvssScoreDetails = CvssScoreDetails'
  { -- | An object that contains details about adjustment Amazon Inspector made
    -- to the CVSS score.
    adjustments :: Prelude.Maybe [CvssScoreAdjustment],
    -- | The source of the CVSS data.
    cvssSource :: Prelude.Maybe Prelude.Text,
    -- | The CVSS score.
    score :: Prelude.Double,
    -- | The source for the CVSS score.
    scoreSource :: Prelude.Text,
    -- | The vector for the CVSS score.
    scoringVector :: Prelude.Text,
    -- | The CVSS version used in scoring.
    version :: Prelude.Text
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
-- 'adjustments', 'cvssScoreDetails_adjustments' - An object that contains details about adjustment Amazon Inspector made
-- to the CVSS score.
--
-- 'cvssSource', 'cvssScoreDetails_cvssSource' - The source of the CVSS data.
--
-- 'score', 'cvssScoreDetails_score' - The CVSS score.
--
-- 'scoreSource', 'cvssScoreDetails_scoreSource' - The source for the CVSS score.
--
-- 'scoringVector', 'cvssScoreDetails_scoringVector' - The vector for the CVSS score.
--
-- 'version', 'cvssScoreDetails_version' - The CVSS version used in scoring.
newCvssScoreDetails ::
  -- | 'score'
  Prelude.Double ->
  -- | 'scoreSource'
  Prelude.Text ->
  -- | 'scoringVector'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  CvssScoreDetails
newCvssScoreDetails
  pScore_
  pScoreSource_
  pScoringVector_
  pVersion_ =
    CvssScoreDetails'
      { adjustments = Prelude.Nothing,
        cvssSource = Prelude.Nothing,
        score = pScore_,
        scoreSource = pScoreSource_,
        scoringVector = pScoringVector_,
        version = pVersion_
      }

-- | An object that contains details about adjustment Amazon Inspector made
-- to the CVSS score.
cvssScoreDetails_adjustments :: Lens.Lens' CvssScoreDetails (Prelude.Maybe [CvssScoreAdjustment])
cvssScoreDetails_adjustments = Lens.lens (\CvssScoreDetails' {adjustments} -> adjustments) (\s@CvssScoreDetails' {} a -> s {adjustments = a} :: CvssScoreDetails) Prelude.. Lens.mapping Lens.coerced

-- | The source of the CVSS data.
cvssScoreDetails_cvssSource :: Lens.Lens' CvssScoreDetails (Prelude.Maybe Prelude.Text)
cvssScoreDetails_cvssSource = Lens.lens (\CvssScoreDetails' {cvssSource} -> cvssSource) (\s@CvssScoreDetails' {} a -> s {cvssSource = a} :: CvssScoreDetails)

-- | The CVSS score.
cvssScoreDetails_score :: Lens.Lens' CvssScoreDetails Prelude.Double
cvssScoreDetails_score = Lens.lens (\CvssScoreDetails' {score} -> score) (\s@CvssScoreDetails' {} a -> s {score = a} :: CvssScoreDetails)

-- | The source for the CVSS score.
cvssScoreDetails_scoreSource :: Lens.Lens' CvssScoreDetails Prelude.Text
cvssScoreDetails_scoreSource = Lens.lens (\CvssScoreDetails' {scoreSource} -> scoreSource) (\s@CvssScoreDetails' {} a -> s {scoreSource = a} :: CvssScoreDetails)

-- | The vector for the CVSS score.
cvssScoreDetails_scoringVector :: Lens.Lens' CvssScoreDetails Prelude.Text
cvssScoreDetails_scoringVector = Lens.lens (\CvssScoreDetails' {scoringVector} -> scoringVector) (\s@CvssScoreDetails' {} a -> s {scoringVector = a} :: CvssScoreDetails)

-- | The CVSS version used in scoring.
cvssScoreDetails_version :: Lens.Lens' CvssScoreDetails Prelude.Text
cvssScoreDetails_version = Lens.lens (\CvssScoreDetails' {version} -> version) (\s@CvssScoreDetails' {} a -> s {version = a} :: CvssScoreDetails)

instance Data.FromJSON CvssScoreDetails where
  parseJSON =
    Data.withObject
      "CvssScoreDetails"
      ( \x ->
          CvssScoreDetails'
            Prelude.<$> (x Data..:? "adjustments" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "cvssSource")
            Prelude.<*> (x Data..: "score")
            Prelude.<*> (x Data..: "scoreSource")
            Prelude.<*> (x Data..: "scoringVector")
            Prelude.<*> (x Data..: "version")
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
    Prelude.rnf adjustments `Prelude.seq`
      Prelude.rnf cvssSource `Prelude.seq`
        Prelude.rnf score `Prelude.seq`
          Prelude.rnf scoreSource `Prelude.seq`
            Prelude.rnf scoringVector `Prelude.seq`
              Prelude.rnf version
