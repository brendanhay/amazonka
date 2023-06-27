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
-- Module      : Amazonka.WellArchitected.Types.PillarMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.PillarMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.QuestionMetric
import Amazonka.WellArchitected.Types.Risk

-- | A metric for a particular pillar in a lens.
--
-- /See:/ 'newPillarMetric' smart constructor.
data PillarMetric = PillarMetric'
  { pillarId :: Prelude.Maybe Prelude.Text,
    -- | The questions that have been identified as risks in the pillar.
    questions :: Prelude.Maybe [QuestionMetric],
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PillarMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pillarId', 'pillarMetric_pillarId' - Undocumented member.
--
-- 'questions', 'pillarMetric_questions' - The questions that have been identified as risks in the pillar.
--
-- 'riskCounts', 'pillarMetric_riskCounts' - Undocumented member.
newPillarMetric ::
  PillarMetric
newPillarMetric =
  PillarMetric'
    { pillarId = Prelude.Nothing,
      questions = Prelude.Nothing,
      riskCounts = Prelude.Nothing
    }

-- | Undocumented member.
pillarMetric_pillarId :: Lens.Lens' PillarMetric (Prelude.Maybe Prelude.Text)
pillarMetric_pillarId = Lens.lens (\PillarMetric' {pillarId} -> pillarId) (\s@PillarMetric' {} a -> s {pillarId = a} :: PillarMetric)

-- | The questions that have been identified as risks in the pillar.
pillarMetric_questions :: Lens.Lens' PillarMetric (Prelude.Maybe [QuestionMetric])
pillarMetric_questions = Lens.lens (\PillarMetric' {questions} -> questions) (\s@PillarMetric' {} a -> s {questions = a} :: PillarMetric) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
pillarMetric_riskCounts :: Lens.Lens' PillarMetric (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
pillarMetric_riskCounts = Lens.lens (\PillarMetric' {riskCounts} -> riskCounts) (\s@PillarMetric' {} a -> s {riskCounts = a} :: PillarMetric) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PillarMetric where
  parseJSON =
    Data.withObject
      "PillarMetric"
      ( \x ->
          PillarMetric'
            Prelude.<$> (x Data..:? "PillarId")
            Prelude.<*> (x Data..:? "Questions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "RiskCounts" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PillarMetric where
  hashWithSalt _salt PillarMetric' {..} =
    _salt
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` questions
      `Prelude.hashWithSalt` riskCounts

instance Prelude.NFData PillarMetric where
  rnf PillarMetric' {..} =
    Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf questions
      `Prelude.seq` Prelude.rnf riskCounts
