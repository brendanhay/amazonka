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
-- Module      : Amazonka.WellArchitected.Types.AnswerSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.AnswerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.AnswerReason
import Amazonka.WellArchitected.Types.Choice
import Amazonka.WellArchitected.Types.ChoiceAnswerSummary
import Amazonka.WellArchitected.Types.Risk

-- | An answer summary of a lens review in a workload.
--
-- /See:/ 'newAnswerSummary' smart constructor.
data AnswerSummary = AnswerSummary'
  { choices :: Prelude.Maybe [Choice],
    selectedChoices :: Prelude.Maybe [Prelude.Text],
    risk :: Prelude.Maybe Risk,
    -- | A list of selected choices to a question in your workload.
    choiceAnswerSummaries :: Prelude.Maybe [ChoiceAnswerSummary],
    questionId :: Prelude.Maybe Prelude.Text,
    isApplicable :: Prelude.Maybe Prelude.Bool,
    -- | The reason why a choice is non-applicable to a question in your
    -- workload.
    reason :: Prelude.Maybe AnswerReason,
    questionTitle :: Prelude.Maybe Prelude.Text,
    pillarId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnswerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'choices', 'answerSummary_choices' - Undocumented member.
--
-- 'selectedChoices', 'answerSummary_selectedChoices' - Undocumented member.
--
-- 'risk', 'answerSummary_risk' - Undocumented member.
--
-- 'choiceAnswerSummaries', 'answerSummary_choiceAnswerSummaries' - A list of selected choices to a question in your workload.
--
-- 'questionId', 'answerSummary_questionId' - Undocumented member.
--
-- 'isApplicable', 'answerSummary_isApplicable' - Undocumented member.
--
-- 'reason', 'answerSummary_reason' - The reason why a choice is non-applicable to a question in your
-- workload.
--
-- 'questionTitle', 'answerSummary_questionTitle' - Undocumented member.
--
-- 'pillarId', 'answerSummary_pillarId' - Undocumented member.
newAnswerSummary ::
  AnswerSummary
newAnswerSummary =
  AnswerSummary'
    { choices = Prelude.Nothing,
      selectedChoices = Prelude.Nothing,
      risk = Prelude.Nothing,
      choiceAnswerSummaries = Prelude.Nothing,
      questionId = Prelude.Nothing,
      isApplicable = Prelude.Nothing,
      reason = Prelude.Nothing,
      questionTitle = Prelude.Nothing,
      pillarId = Prelude.Nothing
    }

-- | Undocumented member.
answerSummary_choices :: Lens.Lens' AnswerSummary (Prelude.Maybe [Choice])
answerSummary_choices = Lens.lens (\AnswerSummary' {choices} -> choices) (\s@AnswerSummary' {} a -> s {choices = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
answerSummary_selectedChoices :: Lens.Lens' AnswerSummary (Prelude.Maybe [Prelude.Text])
answerSummary_selectedChoices = Lens.lens (\AnswerSummary' {selectedChoices} -> selectedChoices) (\s@AnswerSummary' {} a -> s {selectedChoices = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
answerSummary_risk :: Lens.Lens' AnswerSummary (Prelude.Maybe Risk)
answerSummary_risk = Lens.lens (\AnswerSummary' {risk} -> risk) (\s@AnswerSummary' {} a -> s {risk = a} :: AnswerSummary)

-- | A list of selected choices to a question in your workload.
answerSummary_choiceAnswerSummaries :: Lens.Lens' AnswerSummary (Prelude.Maybe [ChoiceAnswerSummary])
answerSummary_choiceAnswerSummaries = Lens.lens (\AnswerSummary' {choiceAnswerSummaries} -> choiceAnswerSummaries) (\s@AnswerSummary' {} a -> s {choiceAnswerSummaries = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
answerSummary_questionId :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_questionId = Lens.lens (\AnswerSummary' {questionId} -> questionId) (\s@AnswerSummary' {} a -> s {questionId = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_isApplicable :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Bool)
answerSummary_isApplicable = Lens.lens (\AnswerSummary' {isApplicable} -> isApplicable) (\s@AnswerSummary' {} a -> s {isApplicable = a} :: AnswerSummary)

-- | The reason why a choice is non-applicable to a question in your
-- workload.
answerSummary_reason :: Lens.Lens' AnswerSummary (Prelude.Maybe AnswerReason)
answerSummary_reason = Lens.lens (\AnswerSummary' {reason} -> reason) (\s@AnswerSummary' {} a -> s {reason = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_questionTitle :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_questionTitle = Lens.lens (\AnswerSummary' {questionTitle} -> questionTitle) (\s@AnswerSummary' {} a -> s {questionTitle = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_pillarId :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_pillarId = Lens.lens (\AnswerSummary' {pillarId} -> pillarId) (\s@AnswerSummary' {} a -> s {pillarId = a} :: AnswerSummary)

instance Core.FromJSON AnswerSummary where
  parseJSON =
    Core.withObject
      "AnswerSummary"
      ( \x ->
          AnswerSummary'
            Prelude.<$> (x Core..:? "Choices" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "SelectedChoices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Risk")
            Prelude.<*> ( x Core..:? "ChoiceAnswerSummaries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "QuestionId")
            Prelude.<*> (x Core..:? "IsApplicable")
            Prelude.<*> (x Core..:? "Reason")
            Prelude.<*> (x Core..:? "QuestionTitle")
            Prelude.<*> (x Core..:? "PillarId")
      )

instance Prelude.Hashable AnswerSummary where
  hashWithSalt _salt AnswerSummary' {..} =
    _salt `Prelude.hashWithSalt` choices
      `Prelude.hashWithSalt` selectedChoices
      `Prelude.hashWithSalt` risk
      `Prelude.hashWithSalt` choiceAnswerSummaries
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` isApplicable
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` questionTitle
      `Prelude.hashWithSalt` pillarId

instance Prelude.NFData AnswerSummary where
  rnf AnswerSummary' {..} =
    Prelude.rnf choices
      `Prelude.seq` Prelude.rnf selectedChoices
      `Prelude.seq` Prelude.rnf risk
      `Prelude.seq` Prelude.rnf choiceAnswerSummaries
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf isApplicable
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf questionTitle
      `Prelude.seq` Prelude.rnf pillarId
