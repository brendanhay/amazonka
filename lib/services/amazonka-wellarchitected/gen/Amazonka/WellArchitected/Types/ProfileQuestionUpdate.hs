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
-- Module      : Amazonka.WellArchitected.Types.ProfileQuestionUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ProfileQuestionUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An update to a profile question.
--
-- /See:/ 'newProfileQuestionUpdate' smart constructor.
data ProfileQuestionUpdate = ProfileQuestionUpdate'
  { questionId :: Prelude.Maybe Prelude.Text,
    -- | The selected choices.
    selectedChoiceIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProfileQuestionUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'questionId', 'profileQuestionUpdate_questionId' - Undocumented member.
--
-- 'selectedChoiceIds', 'profileQuestionUpdate_selectedChoiceIds' - The selected choices.
newProfileQuestionUpdate ::
  ProfileQuestionUpdate
newProfileQuestionUpdate =
  ProfileQuestionUpdate'
    { questionId =
        Prelude.Nothing,
      selectedChoiceIds = Prelude.Nothing
    }

-- | Undocumented member.
profileQuestionUpdate_questionId :: Lens.Lens' ProfileQuestionUpdate (Prelude.Maybe Prelude.Text)
profileQuestionUpdate_questionId = Lens.lens (\ProfileQuestionUpdate' {questionId} -> questionId) (\s@ProfileQuestionUpdate' {} a -> s {questionId = a} :: ProfileQuestionUpdate)

-- | The selected choices.
profileQuestionUpdate_selectedChoiceIds :: Lens.Lens' ProfileQuestionUpdate (Prelude.Maybe [Prelude.Text])
profileQuestionUpdate_selectedChoiceIds = Lens.lens (\ProfileQuestionUpdate' {selectedChoiceIds} -> selectedChoiceIds) (\s@ProfileQuestionUpdate' {} a -> s {selectedChoiceIds = a} :: ProfileQuestionUpdate) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable ProfileQuestionUpdate where
  hashWithSalt _salt ProfileQuestionUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` selectedChoiceIds

instance Prelude.NFData ProfileQuestionUpdate where
  rnf ProfileQuestionUpdate' {..} =
    Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf selectedChoiceIds

instance Data.ToJSON ProfileQuestionUpdate where
  toJSON ProfileQuestionUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("QuestionId" Data..=) Prelude.<$> questionId,
            ("SelectedChoiceIds" Data..=)
              Prelude.<$> selectedChoiceIds
          ]
      )
