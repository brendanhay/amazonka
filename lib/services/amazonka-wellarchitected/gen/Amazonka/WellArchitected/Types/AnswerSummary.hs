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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.AnswerReason
import Amazonka.WellArchitected.Types.Choice
import Amazonka.WellArchitected.Types.ChoiceAnswerSummary
import Amazonka.WellArchitected.Types.Risk

-- | An answer summary of a lens review in a workload.
--
-- /See:/ 'newAnswerSummary' smart constructor.
data AnswerSummary = AnswerSummary'
  { -- | A list of selected choices to a question in your workload.
    choiceAnswerSummaries :: Prelude.Maybe [ChoiceAnswerSummary],
    choices :: Prelude.Maybe [Choice],
    isApplicable :: Prelude.Maybe Prelude.Bool,
    pillarId :: Prelude.Maybe Prelude.Text,
    questionId :: Prelude.Maybe Prelude.Text,
    questionTitle :: Prelude.Maybe Prelude.Text,
    -- | The reason why a choice is non-applicable to a question in your
    -- workload.
    reason :: Prelude.Maybe AnswerReason,
    risk :: Prelude.Maybe Risk,
    selectedChoices :: Prelude.Maybe [Prelude.Text]
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
-- 'choiceAnswerSummaries', 'answerSummary_choiceAnswerSummaries' - A list of selected choices to a question in your workload.
--
-- 'choices', 'answerSummary_choices' - Undocumented member.
--
-- 'isApplicable', 'answerSummary_isApplicable' - Undocumented member.
--
-- 'pillarId', 'answerSummary_pillarId' - Undocumented member.
--
-- 'questionId', 'answerSummary_questionId' - Undocumented member.
--
-- 'questionTitle', 'answerSummary_questionTitle' - Undocumented member.
--
-- 'reason', 'answerSummary_reason' - The reason why a choice is non-applicable to a question in your
-- workload.
--
-- 'risk', 'answerSummary_risk' - Undocumented member.
--
-- 'selectedChoices', 'answerSummary_selectedChoices' - Undocumented member.
newAnswerSummary ::
  AnswerSummary
newAnswerSummary =
  AnswerSummary'
    { choiceAnswerSummaries =
        Prelude.Nothing,
      choices = Prelude.Nothing,
      isApplicable = Prelude.Nothing,
      pillarId = Prelude.Nothing,
      questionId = Prelude.Nothing,
      questionTitle = Prelude.Nothing,
      reason = Prelude.Nothing,
      risk = Prelude.Nothing,
      selectedChoices = Prelude.Nothing
    }

-- | A list of selected choices to a question in your workload.
answerSummary_choiceAnswerSummaries :: Lens.Lens' AnswerSummary (Prelude.Maybe [ChoiceAnswerSummary])
answerSummary_choiceAnswerSummaries = Lens.lens (\AnswerSummary' {choiceAnswerSummaries} -> choiceAnswerSummaries) (\s@AnswerSummary' {} a -> s {choiceAnswerSummaries = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
answerSummary_choices :: Lens.Lens' AnswerSummary (Prelude.Maybe [Choice])
answerSummary_choices = Lens.lens (\AnswerSummary' {choices} -> choices) (\s@AnswerSummary' {} a -> s {choices = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
answerSummary_isApplicable :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Bool)
answerSummary_isApplicable = Lens.lens (\AnswerSummary' {isApplicable} -> isApplicable) (\s@AnswerSummary' {} a -> s {isApplicable = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_pillarId :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_pillarId = Lens.lens (\AnswerSummary' {pillarId} -> pillarId) (\s@AnswerSummary' {} a -> s {pillarId = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_questionId :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_questionId = Lens.lens (\AnswerSummary' {questionId} -> questionId) (\s@AnswerSummary' {} a -> s {questionId = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_questionTitle :: Lens.Lens' AnswerSummary (Prelude.Maybe Prelude.Text)
answerSummary_questionTitle = Lens.lens (\AnswerSummary' {questionTitle} -> questionTitle) (\s@AnswerSummary' {} a -> s {questionTitle = a} :: AnswerSummary)

-- | The reason why a choice is non-applicable to a question in your
-- workload.
answerSummary_reason :: Lens.Lens' AnswerSummary (Prelude.Maybe AnswerReason)
answerSummary_reason = Lens.lens (\AnswerSummary' {reason} -> reason) (\s@AnswerSummary' {} a -> s {reason = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_risk :: Lens.Lens' AnswerSummary (Prelude.Maybe Risk)
answerSummary_risk = Lens.lens (\AnswerSummary' {risk} -> risk) (\s@AnswerSummary' {} a -> s {risk = a} :: AnswerSummary)

-- | Undocumented member.
answerSummary_selectedChoices :: Lens.Lens' AnswerSummary (Prelude.Maybe [Prelude.Text])
answerSummary_selectedChoices = Lens.lens (\AnswerSummary' {selectedChoices} -> selectedChoices) (\s@AnswerSummary' {} a -> s {selectedChoices = a} :: AnswerSummary) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AnswerSummary where
  parseJSON =
    Data.withObject
      "AnswerSummary"
      ( \x ->
          AnswerSummary'
            Prelude.<$> ( x Data..:? "ChoiceAnswerSummaries"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Choices" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IsApplicable")
            Prelude.<*> (x Data..:? "PillarId")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "QuestionTitle")
            Prelude.<*> (x Data..:? "Reason")
            Prelude.<*> (x Data..:? "Risk")
            Prelude.<*> ( x Data..:? "SelectedChoices"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AnswerSummary where
  hashWithSalt _salt AnswerSummary' {..} =
    _salt `Prelude.hashWithSalt` choiceAnswerSummaries
      `Prelude.hashWithSalt` choices
      `Prelude.hashWithSalt` isApplicable
      `Prelude.hashWithSalt` pillarId
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` questionTitle
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` risk
      `Prelude.hashWithSalt` selectedChoices

instance Prelude.NFData AnswerSummary where
  rnf AnswerSummary' {..} =
    Prelude.rnf choiceAnswerSummaries
      `Prelude.seq` Prelude.rnf choices
      `Prelude.seq` Prelude.rnf isApplicable
      `Prelude.seq` Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf questionTitle
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf risk
      `Prelude.seq` Prelude.rnf selectedChoices
