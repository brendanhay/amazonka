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
-- Module      : Amazonka.WellArchitected.Types.QuestionMetric
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.QuestionMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.BestPractice
import Amazonka.WellArchitected.Types.Risk

-- | A metric for a particular question in the pillar.
--
-- /See:/ 'newQuestionMetric' smart constructor.
data QuestionMetric = QuestionMetric'
  { -- | The best practices, or choices, that have been identified as
    -- contributing to risk in a question.
    bestPractices :: Prelude.Maybe [BestPractice],
    questionId :: Prelude.Maybe Prelude.Text,
    risk :: Prelude.Maybe Risk
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuestionMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bestPractices', 'questionMetric_bestPractices' - The best practices, or choices, that have been identified as
-- contributing to risk in a question.
--
-- 'questionId', 'questionMetric_questionId' - Undocumented member.
--
-- 'risk', 'questionMetric_risk' - Undocumented member.
newQuestionMetric ::
  QuestionMetric
newQuestionMetric =
  QuestionMetric'
    { bestPractices = Prelude.Nothing,
      questionId = Prelude.Nothing,
      risk = Prelude.Nothing
    }

-- | The best practices, or choices, that have been identified as
-- contributing to risk in a question.
questionMetric_bestPractices :: Lens.Lens' QuestionMetric (Prelude.Maybe [BestPractice])
questionMetric_bestPractices = Lens.lens (\QuestionMetric' {bestPractices} -> bestPractices) (\s@QuestionMetric' {} a -> s {bestPractices = a} :: QuestionMetric) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
questionMetric_questionId :: Lens.Lens' QuestionMetric (Prelude.Maybe Prelude.Text)
questionMetric_questionId = Lens.lens (\QuestionMetric' {questionId} -> questionId) (\s@QuestionMetric' {} a -> s {questionId = a} :: QuestionMetric)

-- | Undocumented member.
questionMetric_risk :: Lens.Lens' QuestionMetric (Prelude.Maybe Risk)
questionMetric_risk = Lens.lens (\QuestionMetric' {risk} -> risk) (\s@QuestionMetric' {} a -> s {risk = a} :: QuestionMetric)

instance Data.FromJSON QuestionMetric where
  parseJSON =
    Data.withObject
      "QuestionMetric"
      ( \x ->
          QuestionMetric'
            Prelude.<$> (x Data..:? "BestPractices" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "Risk")
      )

instance Prelude.Hashable QuestionMetric where
  hashWithSalt _salt QuestionMetric' {..} =
    _salt
      `Prelude.hashWithSalt` bestPractices
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` risk

instance Prelude.NFData QuestionMetric where
  rnf QuestionMetric' {..} =
    Prelude.rnf bestPractices
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf risk
