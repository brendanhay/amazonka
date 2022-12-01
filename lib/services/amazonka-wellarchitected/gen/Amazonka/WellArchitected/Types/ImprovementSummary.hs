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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ImprovementSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ChoiceImprovementPlan
import Amazonka.WellArchitected.Types.Risk

-- | An improvement summary of a lens review in a workload.
--
-- /See:/ 'newImprovementSummary' smart constructor.
data ImprovementSummary = ImprovementSummary'
  { risk :: Prelude.Maybe Risk,
    questionId :: Prelude.Maybe Prelude.Text,
    improvementPlanUrl :: Prelude.Maybe Prelude.Text,
    -- | The improvement plan details.
    improvementPlans :: Prelude.Maybe [ChoiceImprovementPlan],
    questionTitle :: Prelude.Maybe Prelude.Text,
    pillarId :: Prelude.Maybe Prelude.Text
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
-- 'risk', 'improvementSummary_risk' - Undocumented member.
--
-- 'questionId', 'improvementSummary_questionId' - Undocumented member.
--
-- 'improvementPlanUrl', 'improvementSummary_improvementPlanUrl' - Undocumented member.
--
-- 'improvementPlans', 'improvementSummary_improvementPlans' - The improvement plan details.
--
-- 'questionTitle', 'improvementSummary_questionTitle' - Undocumented member.
--
-- 'pillarId', 'improvementSummary_pillarId' - Undocumented member.
newImprovementSummary ::
  ImprovementSummary
newImprovementSummary =
  ImprovementSummary'
    { risk = Prelude.Nothing,
      questionId = Prelude.Nothing,
      improvementPlanUrl = Prelude.Nothing,
      improvementPlans = Prelude.Nothing,
      questionTitle = Prelude.Nothing,
      pillarId = Prelude.Nothing
    }

-- | Undocumented member.
improvementSummary_risk :: Lens.Lens' ImprovementSummary (Prelude.Maybe Risk)
improvementSummary_risk = Lens.lens (\ImprovementSummary' {risk} -> risk) (\s@ImprovementSummary' {} a -> s {risk = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_questionId :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_questionId = Lens.lens (\ImprovementSummary' {questionId} -> questionId) (\s@ImprovementSummary' {} a -> s {questionId = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_improvementPlanUrl :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_improvementPlanUrl = Lens.lens (\ImprovementSummary' {improvementPlanUrl} -> improvementPlanUrl) (\s@ImprovementSummary' {} a -> s {improvementPlanUrl = a} :: ImprovementSummary)

-- | The improvement plan details.
improvementSummary_improvementPlans :: Lens.Lens' ImprovementSummary (Prelude.Maybe [ChoiceImprovementPlan])
improvementSummary_improvementPlans = Lens.lens (\ImprovementSummary' {improvementPlans} -> improvementPlans) (\s@ImprovementSummary' {} a -> s {improvementPlans = a} :: ImprovementSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
improvementSummary_questionTitle :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_questionTitle = Lens.lens (\ImprovementSummary' {questionTitle} -> questionTitle) (\s@ImprovementSummary' {} a -> s {questionTitle = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_pillarId :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_pillarId = Lens.lens (\ImprovementSummary' {pillarId} -> pillarId) (\s@ImprovementSummary' {} a -> s {pillarId = a} :: ImprovementSummary)

instance Core.FromJSON ImprovementSummary where
  parseJSON =
    Core.withObject
      "ImprovementSummary"
      ( \x ->
          ImprovementSummary'
            Prelude.<$> (x Core..:? "Risk")
            Prelude.<*> (x Core..:? "QuestionId")
            Prelude.<*> (x Core..:? "ImprovementPlanUrl")
            Prelude.<*> ( x Core..:? "ImprovementPlans"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "QuestionTitle")
            Prelude.<*> (x Core..:? "PillarId")
      )

instance Prelude.Hashable ImprovementSummary where
  hashWithSalt _salt ImprovementSummary' {..} =
    _salt `Prelude.hashWithSalt` risk
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` improvementPlanUrl
      `Prelude.hashWithSalt` improvementPlans
      `Prelude.hashWithSalt` questionTitle
      `Prelude.hashWithSalt` pillarId

instance Prelude.NFData ImprovementSummary where
  rnf ImprovementSummary' {..} =
    Prelude.rnf risk
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf improvementPlanUrl
      `Prelude.seq` Prelude.rnf improvementPlans
      `Prelude.seq` Prelude.rnf questionTitle
      `Prelude.seq` Prelude.rnf pillarId
