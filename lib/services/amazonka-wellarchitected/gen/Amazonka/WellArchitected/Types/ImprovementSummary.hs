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
-- Module      : Amazonka.WellArchitected.Types.ImprovementSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ImprovementSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ChoiceImprovementPlan
import Amazonka.WellArchitected.Types.Risk

-- | An improvement summary of a lens review in a workload.
--
-- /See:/ 'newImprovementSummary' smart constructor.
data ImprovementSummary = ImprovementSummary'
  { improvementPlanUrl :: Prelude.Maybe Prelude.Text,
    -- | The improvement plan details.
    improvementPlans :: Prelude.Maybe [ChoiceImprovementPlan],
    pillarId :: Prelude.Maybe Prelude.Text,
    questionId :: Prelude.Maybe Prelude.Text,
    questionTitle :: Prelude.Maybe Prelude.Text,
    risk :: Prelude.Maybe Risk
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImprovementSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'improvementPlanUrl', 'improvementSummary_improvementPlanUrl' - Undocumented member.
--
-- 'improvementPlans', 'improvementSummary_improvementPlans' - The improvement plan details.
--
-- 'pillarId', 'improvementSummary_pillarId' - Undocumented member.
--
-- 'questionId', 'improvementSummary_questionId' - Undocumented member.
--
-- 'questionTitle', 'improvementSummary_questionTitle' - Undocumented member.
--
-- 'risk', 'improvementSummary_risk' - Undocumented member.
newImprovementSummary ::
  ImprovementSummary
newImprovementSummary =
  ImprovementSummary'
    { improvementPlanUrl =
        Prelude.Nothing,
      improvementPlans = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      questionId = Prelude.Nothing,
      questionTitle = Prelude.Nothing,
      risk = Prelude.Nothing
    }

-- | Undocumented member.
improvementSummary_improvementPlanUrl :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_improvementPlanUrl = Lens.lens (\ImprovementSummary' {improvementPlanUrl} -> improvementPlanUrl) (\s@ImprovementSummary' {} a -> s {improvementPlanUrl = a} :: ImprovementSummary)

-- | The improvement plan details.
improvementSummary_improvementPlans :: Lens.Lens' ImprovementSummary (Prelude.Maybe [ChoiceImprovementPlan])
improvementSummary_improvementPlans = Lens.lens (\ImprovementSummary' {improvementPlans} -> improvementPlans) (\s@ImprovementSummary' {} a -> s {improvementPlans = a} :: ImprovementSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
improvementSummary_pillarId :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_pillarId = Lens.lens (\ImprovementSummary' {pillarId} -> pillarId) (\s@ImprovementSummary' {} a -> s {pillarId = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_questionId :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_questionId = Lens.lens (\ImprovementSummary' {questionId} -> questionId) (\s@ImprovementSummary' {} a -> s {questionId = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_questionTitle :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_questionTitle = Lens.lens (\ImprovementSummary' {questionTitle} -> questionTitle) (\s@ImprovementSummary' {} a -> s {questionTitle = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_risk :: Lens.Lens' ImprovementSummary (Prelude.Maybe Risk)
improvementSummary_risk = Lens.lens (\ImprovementSummary' {risk} -> risk) (\s@ImprovementSummary' {} a -> s {risk = a} :: ImprovementSummary)

instance Data.FromJSON ImprovementSummary where
  parseJSON =
    Data.withObject
      "ImprovementSummary"
      ( \x ->
          ImprovementSummary'
            Prelude.<$> (x Data..:? "ImprovementPlanUrl")
            Prelude.<*> ( x
                            Data..:? "ImprovementPlans"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "PillarId")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "QuestionTitle")
            Prelude.<*> (x Data..:? "Risk")
      )

instance Prelude.Hashable ImprovementSummary where
  hashWithSalt _salt ImprovementSummary' {..} =
    _salt
      `Prelude.hashWithSalt` improvementPlanUrl
      `Prelude.hashWithSalt` improvementPlans
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` questionTitle
      `Prelude.hashWithSalt` risk

instance Prelude.NFData ImprovementSummary where
  rnf ImprovementSummary' {..} =
    Prelude.rnf improvementPlanUrl
      `Prelude.seq` Prelude.rnf improvementPlans
      `Prelude.seq` Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf questionTitle
      `Prelude.seq` Prelude.rnf risk
