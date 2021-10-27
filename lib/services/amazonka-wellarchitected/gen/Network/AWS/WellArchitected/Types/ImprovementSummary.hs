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
-- Module      : Network.AWS.WellArchitected.Types.ImprovementSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.ImprovementSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WellArchitected.Types.Risk

-- | An improvement summary of a lens review in a workload.
--
-- /See:/ 'newImprovementSummary' smart constructor.
data ImprovementSummary = ImprovementSummary'
  { pillarId :: Prelude.Maybe Prelude.Text,
    improvementPlanUrl :: Prelude.Maybe Prelude.Text,
    risk :: Prelude.Maybe Risk,
    questionTitle :: Prelude.Maybe Prelude.Text,
    questionId :: Prelude.Maybe Prelude.Text
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
-- 'pillarId', 'improvementSummary_pillarId' - Undocumented member.
--
-- 'improvementPlanUrl', 'improvementSummary_improvementPlanUrl' - Undocumented member.
--
-- 'risk', 'improvementSummary_risk' - Undocumented member.
--
-- 'questionTitle', 'improvementSummary_questionTitle' - Undocumented member.
--
-- 'questionId', 'improvementSummary_questionId' - Undocumented member.
newImprovementSummary ::
  ImprovementSummary
newImprovementSummary =
  ImprovementSummary'
    { pillarId = Prelude.Nothing,
      improvementPlanUrl = Prelude.Nothing,
      risk = Prelude.Nothing,
      questionTitle = Prelude.Nothing,
      questionId = Prelude.Nothing
    }

-- | Undocumented member.
improvementSummary_pillarId :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_pillarId = Lens.lens (\ImprovementSummary' {pillarId} -> pillarId) (\s@ImprovementSummary' {} a -> s {pillarId = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_improvementPlanUrl :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_improvementPlanUrl = Lens.lens (\ImprovementSummary' {improvementPlanUrl} -> improvementPlanUrl) (\s@ImprovementSummary' {} a -> s {improvementPlanUrl = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_risk :: Lens.Lens' ImprovementSummary (Prelude.Maybe Risk)
improvementSummary_risk = Lens.lens (\ImprovementSummary' {risk} -> risk) (\s@ImprovementSummary' {} a -> s {risk = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_questionTitle :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_questionTitle = Lens.lens (\ImprovementSummary' {questionTitle} -> questionTitle) (\s@ImprovementSummary' {} a -> s {questionTitle = a} :: ImprovementSummary)

-- | Undocumented member.
improvementSummary_questionId :: Lens.Lens' ImprovementSummary (Prelude.Maybe Prelude.Text)
improvementSummary_questionId = Lens.lens (\ImprovementSummary' {questionId} -> questionId) (\s@ImprovementSummary' {} a -> s {questionId = a} :: ImprovementSummary)

instance Core.FromJSON ImprovementSummary where
  parseJSON =
    Core.withObject
      "ImprovementSummary"
      ( \x ->
          ImprovementSummary'
            Prelude.<$> (x Core..:? "PillarId")
            Prelude.<*> (x Core..:? "ImprovementPlanUrl")
            Prelude.<*> (x Core..:? "Risk")
            Prelude.<*> (x Core..:? "QuestionTitle")
            Prelude.<*> (x Core..:? "QuestionId")
      )

instance Prelude.Hashable ImprovementSummary

instance Prelude.NFData ImprovementSummary
