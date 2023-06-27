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
-- Module      : Amazonka.WellArchitected.Types.ProfileQuestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileQuestion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ProfileChoice

-- | A profile question.
--
-- /See:/ 'newProfileQuestion' smart constructor.
data ProfileQuestion = ProfileQuestion'
  { -- | The maximum number of selected choices.
    maxSelectedChoices :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of selected choices.
    minSelectedChoices :: Prelude.Maybe Prelude.Natural,
    -- | The question choices.
    questionChoices :: Prelude.Maybe [ProfileChoice],
    questionDescription :: Prelude.Maybe Prelude.Text,
    questionId :: Prelude.Maybe Prelude.Text,
    questionTitle :: Prelude.Maybe Prelude.Text,
    -- | The selected choices.
    selectedChoiceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileQuestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxSelectedChoices', 'profileQuestion_maxSelectedChoices' - The maximum number of selected choices.
--
-- 'minSelectedChoices', 'profileQuestion_minSelectedChoices' - The minimum number of selected choices.
--
-- 'questionChoices', 'profileQuestion_questionChoices' - The question choices.
--
-- 'questionDescription', 'profileQuestion_questionDescription' - Undocumented member.
--
-- 'questionId', 'profileQuestion_questionId' - Undocumented member.
--
-- 'questionTitle', 'profileQuestion_questionTitle' - Undocumented member.
--
-- 'selectedChoiceIds', 'profileQuestion_selectedChoiceIds' - The selected choices.
newProfileQuestion ::
  ProfileQuestion
newProfileQuestion =
  ProfileQuestion'
    { maxSelectedChoices =
        Prelude.Nothing,
      minSelectedChoices = Prelude.Nothing,
      questionChoices = Prelude.Nothing,
      questionDescription = Prelude.Nothing,
      questionId = Prelude.Nothing,
      questionTitle = Prelude.Nothing,
      selectedChoiceIds = Prelude.Nothing
    }

-- | The maximum number of selected choices.
profileQuestion_maxSelectedChoices :: Lens.Lens' ProfileQuestion (Prelude.Maybe Prelude.Natural)
profileQuestion_maxSelectedChoices = Lens.lens (\ProfileQuestion' {maxSelectedChoices} -> maxSelectedChoices) (\s@ProfileQuestion' {} a -> s {maxSelectedChoices = a} :: ProfileQuestion)

-- | The minimum number of selected choices.
profileQuestion_minSelectedChoices :: Lens.Lens' ProfileQuestion (Prelude.Maybe Prelude.Natural)
profileQuestion_minSelectedChoices = Lens.lens (\ProfileQuestion' {minSelectedChoices} -> minSelectedChoices) (\s@ProfileQuestion' {} a -> s {minSelectedChoices = a} :: ProfileQuestion)

-- | The question choices.
profileQuestion_questionChoices :: Lens.Lens' ProfileQuestion (Prelude.Maybe [ProfileChoice])
profileQuestion_questionChoices = Lens.lens (\ProfileQuestion' {questionChoices} -> questionChoices) (\s@ProfileQuestion' {} a -> s {questionChoices = a} :: ProfileQuestion) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
profileQuestion_questionDescription :: Lens.Lens' ProfileQuestion (Prelude.Maybe Prelude.Text)
profileQuestion_questionDescription = Lens.lens (\ProfileQuestion' {questionDescription} -> questionDescription) (\s@ProfileQuestion' {} a -> s {questionDescription = a} :: ProfileQuestion)

-- | Undocumented member.
profileQuestion_questionId :: Lens.Lens' ProfileQuestion (Prelude.Maybe Prelude.Text)
profileQuestion_questionId = Lens.lens (\ProfileQuestion' {questionId} -> questionId) (\s@ProfileQuestion' {} a -> s {questionId = a} :: ProfileQuestion)

-- | Undocumented member.
profileQuestion_questionTitle :: Lens.Lens' ProfileQuestion (Prelude.Maybe Prelude.Text)
profileQuestion_questionTitle = Lens.lens (\ProfileQuestion' {questionTitle} -> questionTitle) (\s@ProfileQuestion' {} a -> s {questionTitle = a} :: ProfileQuestion)

-- | The selected choices.
profileQuestion_selectedChoiceIds :: Lens.Lens' ProfileQuestion (Prelude.Maybe [Prelude.Text])
profileQuestion_selectedChoiceIds = Lens.lens (\ProfileQuestion' {selectedChoiceIds} -> selectedChoiceIds) (\s@ProfileQuestion' {} a -> s {selectedChoiceIds = a} :: ProfileQuestion) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ProfileQuestion where
  parseJSON =
    Data.withObject
      "ProfileQuestion"
      ( \x ->
          ProfileQuestion'
            Prelude.<$> (x Data..:? "MaxSelectedChoices")
            Prelude.<*> (x Data..:? "MinSelectedChoices")
            Prelude.<*> ( x
                            Data..:? "QuestionChoices"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "QuestionDescription")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "QuestionTitle")
            Prelude.<*> ( x
                            Data..:? "SelectedChoiceIds"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ProfileQuestion where
  hashWithSalt _salt ProfileQuestion' {..} =
    _salt
      `Prelude.hashWithSalt` maxSelectedChoices
      `Prelude.hashWithSalt` minSelectedChoices
      `Prelude.hashWithSalt` questionChoices
      `Prelude.hashWithSalt` questionDescription
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` questionTitle
      `Prelude.hashWithSalt` selectedChoiceIds

instance Prelude.NFData ProfileQuestion where
  rnf ProfileQuestion' {..} =
    Prelude.rnf maxSelectedChoices
      `Prelude.seq` Prelude.rnf minSelectedChoices
      `Prelude.seq` Prelude.rnf questionChoices
      `Prelude.seq` Prelude.rnf questionDescription
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf questionTitle
      `Prelude.seq` Prelude.rnf selectedChoiceIds
