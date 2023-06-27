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
-- Module      : Amazonka.WellArchitected.Types.ProfileTemplateQuestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileTemplateQuestion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ProfileTemplateChoice

-- | A profile template question.
--
-- /See:/ 'newProfileTemplateQuestion' smart constructor.
data ProfileTemplateQuestion = ProfileTemplateQuestion'
  { -- | The maximum number of choices selected.
    maxSelectedChoices :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of choices selected.
    minSelectedChoices :: Prelude.Maybe Prelude.Natural,
    -- | The question choices.
    questionChoices :: Prelude.Maybe [ProfileTemplateChoice],
    questionDescription :: Prelude.Maybe Prelude.Text,
    questionId :: Prelude.Maybe Prelude.Text,
    questionTitle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileTemplateQuestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxSelectedChoices', 'profileTemplateQuestion_maxSelectedChoices' - The maximum number of choices selected.
--
-- 'minSelectedChoices', 'profileTemplateQuestion_minSelectedChoices' - The minimum number of choices selected.
--
-- 'questionChoices', 'profileTemplateQuestion_questionChoices' - The question choices.
--
-- 'questionDescription', 'profileTemplateQuestion_questionDescription' - Undocumented member.
--
-- 'questionId', 'profileTemplateQuestion_questionId' - Undocumented member.
--
-- 'questionTitle', 'profileTemplateQuestion_questionTitle' - Undocumented member.
newProfileTemplateQuestion ::
  ProfileTemplateQuestion
newProfileTemplateQuestion =
  ProfileTemplateQuestion'
    { maxSelectedChoices =
        Prelude.Nothing,
      minSelectedChoices = Prelude.Nothing,
      questionChoices = Prelude.Nothing,
      questionDescription = Prelude.Nothing,
      questionId = Prelude.Nothing,
      questionTitle = Prelude.Nothing
    }

-- | The maximum number of choices selected.
profileTemplateQuestion_maxSelectedChoices :: Lens.Lens' ProfileTemplateQuestion (Prelude.Maybe Prelude.Natural)
profileTemplateQuestion_maxSelectedChoices = Lens.lens (\ProfileTemplateQuestion' {maxSelectedChoices} -> maxSelectedChoices) (\s@ProfileTemplateQuestion' {} a -> s {maxSelectedChoices = a} :: ProfileTemplateQuestion)

-- | The minimum number of choices selected.
profileTemplateQuestion_minSelectedChoices :: Lens.Lens' ProfileTemplateQuestion (Prelude.Maybe Prelude.Natural)
profileTemplateQuestion_minSelectedChoices = Lens.lens (\ProfileTemplateQuestion' {minSelectedChoices} -> minSelectedChoices) (\s@ProfileTemplateQuestion' {} a -> s {minSelectedChoices = a} :: ProfileTemplateQuestion)

-- | The question choices.
profileTemplateQuestion_questionChoices :: Lens.Lens' ProfileTemplateQuestion (Prelude.Maybe [ProfileTemplateChoice])
profileTemplateQuestion_questionChoices = Lens.lens (\ProfileTemplateQuestion' {questionChoices} -> questionChoices) (\s@ProfileTemplateQuestion' {} a -> s {questionChoices = a} :: ProfileTemplateQuestion) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
profileTemplateQuestion_questionDescription :: Lens.Lens' ProfileTemplateQuestion (Prelude.Maybe Prelude.Text)
profileTemplateQuestion_questionDescription = Lens.lens (\ProfileTemplateQuestion' {questionDescription} -> questionDescription) (\s@ProfileTemplateQuestion' {} a -> s {questionDescription = a} :: ProfileTemplateQuestion)

-- | Undocumented member.
profileTemplateQuestion_questionId :: Lens.Lens' ProfileTemplateQuestion (Prelude.Maybe Prelude.Text)
profileTemplateQuestion_questionId = Lens.lens (\ProfileTemplateQuestion' {questionId} -> questionId) (\s@ProfileTemplateQuestion' {} a -> s {questionId = a} :: ProfileTemplateQuestion)

-- | Undocumented member.
profileTemplateQuestion_questionTitle :: Lens.Lens' ProfileTemplateQuestion (Prelude.Maybe Prelude.Text)
profileTemplateQuestion_questionTitle = Lens.lens (\ProfileTemplateQuestion' {questionTitle} -> questionTitle) (\s@ProfileTemplateQuestion' {} a -> s {questionTitle = a} :: ProfileTemplateQuestion)

instance Data.FromJSON ProfileTemplateQuestion where
  parseJSON =
    Data.withObject
      "ProfileTemplateQuestion"
      ( \x ->
          ProfileTemplateQuestion'
            Prelude.<$> (x Data..:? "MaxSelectedChoices")
            Prelude.<*> (x Data..:? "MinSelectedChoices")
            Prelude.<*> ( x
                            Data..:? "QuestionChoices"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "QuestionDescription")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "QuestionTitle")
      )

instance Prelude.Hashable ProfileTemplateQuestion where
  hashWithSalt _salt ProfileTemplateQuestion' {..} =
    _salt
      `Prelude.hashWithSalt` maxSelectedChoices
      `Prelude.hashWithSalt` minSelectedChoices
      `Prelude.hashWithSalt` questionChoices
      `Prelude.hashWithSalt` questionDescription
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` questionTitle

instance Prelude.NFData ProfileTemplateQuestion where
  rnf ProfileTemplateQuestion' {..} =
    Prelude.rnf maxSelectedChoices
      `Prelude.seq` Prelude.rnf minSelectedChoices
      `Prelude.seq` Prelude.rnf questionChoices
      `Prelude.seq` Prelude.rnf questionDescription
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf questionTitle
