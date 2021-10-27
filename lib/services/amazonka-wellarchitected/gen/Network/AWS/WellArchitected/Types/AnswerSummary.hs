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
-- Module      : Network.AWS.WellArchitected.Types.AnswerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WellArchitected.Types.AnswerSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WellArchitected.Types.AnswerReason
import Network.AWS.WellArchitected.Types.Choice
import Network.AWS.WellArchitected.Types.ChoiceAnswerSummary
import Network.AWS.WellArchitected.Types.Risk

-- | An answer summary of a lens review in a workload.
--
-- /See:/ 'newAnswerSummary' smart constructor.
data AnswerSummary = AnswerSummary'
  { isApplicable :: Prelude.Maybe Prelude.Bool,
    pillarId :: Prelude.Maybe Prelude.Text,
    -- | A list of selected choices to a question in your workload.
    choiceAnswerSummaries :: Prelude.Maybe [ChoiceAnswerSummary],
    risk :: Prelude.Maybe Risk,
    questionTitle :: Prelude.Maybe Prelude.Text,
    selectedChoices :: Prelude.Maybe [Prelude.Text],
    -- | The reason why a choice is non-applicable to a question in your
    -- workload.
    reason :: Prelude.Maybe AnswerReason,
    choices :: Prelude.Maybe [Choice],
    questionId :: Prelude.Maybe Prelude.Text
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
-- 'isApplicable', 'answerSummary_isApplicable' - Undocumented member.
--
-- 'pillarId', 'answerSummary_pillarId' - Undocumented member.
--
-- 'choiceAnswerSummaries', 'answerSummary_choiceAnswerSummaries' - A list of selected choices to a question in your workload.
--
-- 'risk', 'answerSummary_risk' - Undocumented member.
--
-- 'questionTitle', 'answerSummary_questionTitle' - Undocumented member.
--
-- 'selectedChoices', 'answerSummary_selectedChoices' - Undocumented member.
--
-- 'reason', 'answerSummary_reason' - The reason why a choice is non-applicable to a question in your
-- workload.
--
-- 'choices', 'answerSummary_choices' - Undocumented member.
--
-- 'questionId', 'answerSummary_questionId' - Undocumented member.
newAnswerSummary ::
  AnswerSummary
newAnswerSummary =
  AnswerSummary'
    { isApplicable = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      choiceAnswerSummaries = Prelude.Nothing,
      risk = Prelude.Nothing,
      questionTitle = Prelude.Nothing,
      selectedChoices = Prelude.Nothing,
      reason = Prelude.Nothing,
      choices = Prelude.Nothing,
      questionId = Prelude.Nothing
    }

-- | Undocumented member.
answerSummary_isApplicable :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Bool)
answerSummary_isApplicable = Lens.lens (\AnswerSummary' {isApplicable} -> isApplicable) (\s@AnswerSummary' {} a -> s {isApplicable = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_pillarId :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_pillarId = Lens.lens (\AnswerSummary' {pillarId} -> pillarId) (\s@AnswerSummary' {} a -> s {pillarId = a} :: AnswerSummary)

-- | A list of selected choices to a question in your workload.
answerSummary_choiceAnswerSummaries :: Lens.Lens' AnswerSummary (Prelude.Maybe [ChoiceAnswerSummary])
answerSummary_choiceAnswerSummaries = Lens.lens (\AnswerSummary' {choiceAnswerSummaries} -> choiceAnswerSummaries) (\s@AnswerSummary' {} a -> s {choiceAnswerSummaries = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
answerSummary_risk :: Lens.Lens' AnswerSummary (Prelude.Maybe Risk)
answerSummary_risk = Lens.lens (\AnswerSummary' {risk} -> risk) (\s@AnswerSummary' {} a -> s {risk = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_questionTitle :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_questionTitle = Lens.lens (\AnswerSummary' {questionTitle} -> questionTitle) (\s@AnswerSummary' {} a -> s {questionTitle = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_selectedChoices :: Lens.Lens' AnswerSummary (Prelude.Maybe [Prelude.Text])
answerSummary_selectedChoices = Lens.lens (\AnswerSummary' {selectedChoices} -> selectedChoices) (\s@AnswerSummary' {} a -> s {selectedChoices = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | The reason why a choice is non-applicable to a question in your
-- workload.
answerSummary_reason :: Lens.Lens' AnswerSummary (Prelude.Maybe AnswerReason)
answerSummary_reason = Lens.lens (\AnswerSummary' {reason} -> reason) (\s@AnswerSummary' {} a -> s {reason = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_choices :: Lens.Lens' AnswerSummary (Prelude.Maybe [Choice])
answerSummary_choices = Lens.lens (\AnswerSummary' {choices} -> choices) (\s@AnswerSummary' {} a -> s {choices = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
answerSummary_questionId :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_questionId = Lens.lens (\AnswerSummary' {questionId} -> questionId) (\s@AnswerSummary' {} a -> s {questionId = a} :: AnswerSummary)

instance Core.FromJSON AnswerSummary where
  parseJSON =
    Core.withObject
      "AnswerSummary"
      ( \x ->
          AnswerSummary'
            Prelude.<$> (x Core..:? "IsApplicable")
            Prelude.<*> (x Core..:? "PillarId")
            Prelude.<*> ( x Core..:? "ChoiceAnswerSummaries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Risk")
            Prelude.<*> (x Core..:? "QuestionTitle")
            Prelude.<*> ( x Core..:? "SelectedChoices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Reason")
            Prelude.<*> (x Core..:? "Choices" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "QuestionId")
      )

instance Prelude.Hashable AnswerSummary

instance Prelude.NFData AnswerSummary
