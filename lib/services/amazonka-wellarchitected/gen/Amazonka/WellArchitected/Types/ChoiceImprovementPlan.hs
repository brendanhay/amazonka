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
-- Module      : Amazonka.WellArchitected.Types.ChoiceImprovementPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ChoiceImprovementPlan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The choice level improvement plan.
--
-- /See:/ 'newChoiceImprovementPlan' smart constructor.
data ChoiceImprovementPlan = ChoiceImprovementPlan'
  { choiceId :: Prelude.Maybe Prelude.Text,
    -- | The display text for the improvement plan.
    displayText :: Prelude.Maybe Prelude.Text,
    improvementPlanUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChoiceImprovementPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'choiceId', 'choiceImprovementPlan_choiceId' - Undocumented member.
--
-- 'displayText', 'choiceImprovementPlan_displayText' - The display text for the improvement plan.
--
-- 'improvementPlanUrl', 'choiceImprovementPlan_improvementPlanUrl' - Undocumented member.
newChoiceImprovementPlan ::
  ChoiceImprovementPlan
newChoiceImprovementPlan =
  ChoiceImprovementPlan'
    { choiceId = Prelude.Nothing,
      displayText = Prelude.Nothing,
      improvementPlanUrl = Prelude.Nothing
    }

-- | Undocumented member.
choiceImprovementPlan_choiceId :: Lens.Lens' ChoiceImprovementPlan (Prelude.Maybe Prelude.Text)
choiceImprovementPlan_choiceId = Lens.lens (\ChoiceImprovementPlan' {choiceId} -> choiceId) (\s@ChoiceImprovementPlan' {} a -> s {choiceId = a} :: ChoiceImprovementPlan)

-- | The display text for the improvement plan.
choiceImprovementPlan_displayText :: Lens.Lens' ChoiceImprovementPlan (Prelude.Maybe Prelude.Text)
choiceImprovementPlan_displayText = Lens.lens (\ChoiceImprovementPlan' {displayText} -> displayText) (\s@ChoiceImprovementPlan' {} a -> s {displayText = a} :: ChoiceImprovementPlan)

-- | Undocumented member.
choiceImprovementPlan_improvementPlanUrl :: Lens.Lens' ChoiceImprovementPlan (Prelude.Maybe Prelude.Text)
choiceImprovementPlan_improvementPlanUrl = Lens.lens (\ChoiceImprovementPlan' {improvementPlanUrl} -> improvementPlanUrl) (\s@ChoiceImprovementPlan' {} a -> s {improvementPlanUrl = a} :: ChoiceImprovementPlan)

instance Data.FromJSON ChoiceImprovementPlan where
  parseJSON =
    Data.withObject
      "ChoiceImprovementPlan"
      ( \x ->
          ChoiceImprovementPlan'
            Prelude.<$> (x Data..:? "ChoiceId")
            Prelude.<*> (x Data..:? "DisplayText")
            Prelude.<*> (x Data..:? "ImprovementPlanUrl")
      )

instance Prelude.Hashable ChoiceImprovementPlan where
  hashWithSalt _salt ChoiceImprovementPlan' {..} =
    _salt `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` displayText
      `Prelude.hashWithSalt` improvementPlanUrl

instance Prelude.NFData ChoiceImprovementPlan where
  rnf ChoiceImprovementPlan' {..} =
    Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf displayText
      `Prelude.seq` Prelude.rnf improvementPlanUrl
