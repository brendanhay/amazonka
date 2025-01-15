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
-- Module      : Amazonka.Config.Types.ConformancePackComplianceScore
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConformancePackComplianceScore where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A compliance score is the percentage of the number of compliant
-- rule-resource combinations in a conformance pack compared to the number
-- of total possible rule-resource combinations in the conformance pack.
-- This metric provides you with a high-level view of the compliance state
-- of your conformance packs. You can use it to identify, investigate, and
-- understand the level of compliance in your conformance packs.
--
-- /See:/ 'newConformancePackComplianceScore' smart constructor.
data ConformancePackComplianceScore = ConformancePackComplianceScore'
  { -- | The name of the conformance pack.
    conformancePackName :: Prelude.Maybe Prelude.Text,
    -- | The time that the conformance pack compliance score was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Compliance score for the conformance pack. Conformance packs with no
    -- evaluation results will have a compliance score of @INSUFFICIENT_DATA@.
    score :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConformancePackComplianceScore' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conformancePackName', 'conformancePackComplianceScore_conformancePackName' - The name of the conformance pack.
--
-- 'lastUpdatedTime', 'conformancePackComplianceScore_lastUpdatedTime' - The time that the conformance pack compliance score was last updated.
--
-- 'score', 'conformancePackComplianceScore_score' - Compliance score for the conformance pack. Conformance packs with no
-- evaluation results will have a compliance score of @INSUFFICIENT_DATA@.
newConformancePackComplianceScore ::
  ConformancePackComplianceScore
newConformancePackComplianceScore =
  ConformancePackComplianceScore'
    { conformancePackName =
        Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      score = Prelude.Nothing
    }

-- | The name of the conformance pack.
conformancePackComplianceScore_conformancePackName :: Lens.Lens' ConformancePackComplianceScore (Prelude.Maybe Prelude.Text)
conformancePackComplianceScore_conformancePackName = Lens.lens (\ConformancePackComplianceScore' {conformancePackName} -> conformancePackName) (\s@ConformancePackComplianceScore' {} a -> s {conformancePackName = a} :: ConformancePackComplianceScore)

-- | The time that the conformance pack compliance score was last updated.
conformancePackComplianceScore_lastUpdatedTime :: Lens.Lens' ConformancePackComplianceScore (Prelude.Maybe Prelude.UTCTime)
conformancePackComplianceScore_lastUpdatedTime = Lens.lens (\ConformancePackComplianceScore' {lastUpdatedTime} -> lastUpdatedTime) (\s@ConformancePackComplianceScore' {} a -> s {lastUpdatedTime = a} :: ConformancePackComplianceScore) Prelude.. Lens.mapping Data._Time

-- | Compliance score for the conformance pack. Conformance packs with no
-- evaluation results will have a compliance score of @INSUFFICIENT_DATA@.
conformancePackComplianceScore_score :: Lens.Lens' ConformancePackComplianceScore (Prelude.Maybe Prelude.Text)
conformancePackComplianceScore_score = Lens.lens (\ConformancePackComplianceScore' {score} -> score) (\s@ConformancePackComplianceScore' {} a -> s {score = a} :: ConformancePackComplianceScore)

instance Data.FromJSON ConformancePackComplianceScore where
  parseJSON =
    Data.withObject
      "ConformancePackComplianceScore"
      ( \x ->
          ConformancePackComplianceScore'
            Prelude.<$> (x Data..:? "ConformancePackName")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Score")
      )

instance
  Prelude.Hashable
    ConformancePackComplianceScore
  where
  hashWithSalt
    _salt
    ConformancePackComplianceScore' {..} =
      _salt
        `Prelude.hashWithSalt` conformancePackName
        `Prelude.hashWithSalt` lastUpdatedTime
        `Prelude.hashWithSalt` score

instance
  Prelude.NFData
    ConformancePackComplianceScore
  where
  rnf ConformancePackComplianceScore' {..} =
    Prelude.rnf conformancePackName `Prelude.seq`
      Prelude.rnf lastUpdatedTime `Prelude.seq`
        Prelude.rnf score
