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
-- Module      : Amazonka.ECR.Types.CvssScore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.CvssScore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The CVSS score for a finding.
--
-- /See:/ 'newCvssScore' smart constructor.
data CvssScore = CvssScore'
  { -- | The base CVSS score used for the finding.
    baseScore :: Prelude.Maybe Prelude.Double,
    -- | The vector string of the CVSS score.
    scoringVector :: Prelude.Maybe Prelude.Text,
    -- | The source of the CVSS score.
    source :: Prelude.Maybe Prelude.Text,
    -- | The version of CVSS used for the score.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CvssScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseScore', 'cvssScore_baseScore' - The base CVSS score used for the finding.
--
-- 'scoringVector', 'cvssScore_scoringVector' - The vector string of the CVSS score.
--
-- 'source', 'cvssScore_source' - The source of the CVSS score.
--
-- 'version', 'cvssScore_version' - The version of CVSS used for the score.
newCvssScore ::
  CvssScore
newCvssScore =
  CvssScore'
    { baseScore = Prelude.Nothing,
      scoringVector = Prelude.Nothing,
      source = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The base CVSS score used for the finding.
cvssScore_baseScore :: Lens.Lens' CvssScore (Prelude.Maybe Prelude.Double)
cvssScore_baseScore = Lens.lens (\CvssScore' {baseScore} -> baseScore) (\s@CvssScore' {} a -> s {baseScore = a} :: CvssScore)

-- | The vector string of the CVSS score.
cvssScore_scoringVector :: Lens.Lens' CvssScore (Prelude.Maybe Prelude.Text)
cvssScore_scoringVector = Lens.lens (\CvssScore' {scoringVector} -> scoringVector) (\s@CvssScore' {} a -> s {scoringVector = a} :: CvssScore)

-- | The source of the CVSS score.
cvssScore_source :: Lens.Lens' CvssScore (Prelude.Maybe Prelude.Text)
cvssScore_source = Lens.lens (\CvssScore' {source} -> source) (\s@CvssScore' {} a -> s {source = a} :: CvssScore)

-- | The version of CVSS used for the score.
cvssScore_version :: Lens.Lens' CvssScore (Prelude.Maybe Prelude.Text)
cvssScore_version = Lens.lens (\CvssScore' {version} -> version) (\s@CvssScore' {} a -> s {version = a} :: CvssScore)

instance Data.FromJSON CvssScore where
  parseJSON =
    Data.withObject
      "CvssScore"
      ( \x ->
          CvssScore'
            Prelude.<$> (x Data..:? "baseScore")
            Prelude.<*> (x Data..:? "scoringVector")
            Prelude.<*> (x Data..:? "source")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable CvssScore where
  hashWithSalt _salt CvssScore' {..} =
    _salt
      `Prelude.hashWithSalt` baseScore
      `Prelude.hashWithSalt` scoringVector
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` version

instance Prelude.NFData CvssScore where
  rnf CvssScore' {..} =
    Prelude.rnf baseScore
      `Prelude.seq` Prelude.rnf scoringVector
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf version
