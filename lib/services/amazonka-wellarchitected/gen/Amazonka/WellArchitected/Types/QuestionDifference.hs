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
-- Module      : Amazonka.WellArchitected.Types.QuestionDifference
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.QuestionDifference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.DifferenceStatus

-- | A question difference return object.
--
-- /See:/ 'newQuestionDifference' smart constructor.
data QuestionDifference = QuestionDifference'
  { -- | Indicates the type of change to the question.
    differenceStatus :: Prelude.Maybe DifferenceStatus,
    questionId :: Prelude.Maybe Prelude.Text,
    questionTitle :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuestionDifference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'differenceStatus', 'questionDifference_differenceStatus' - Indicates the type of change to the question.
--
-- 'questionId', 'questionDifference_questionId' - Undocumented member.
--
-- 'questionTitle', 'questionDifference_questionTitle' - Undocumented member.
newQuestionDifference ::
  QuestionDifference
newQuestionDifference =
  QuestionDifference'
    { differenceStatus =
        Prelude.Nothing,
      questionId = Prelude.Nothing,
      questionTitle = Prelude.Nothing
    }

-- | Indicates the type of change to the question.
questionDifference_differenceStatus :: Lens.Lens' QuestionDifference (Prelude.Maybe DifferenceStatus)
questionDifference_differenceStatus = Lens.lens (\QuestionDifference' {differenceStatus} -> differenceStatus) (\s@QuestionDifference' {} a -> s {differenceStatus = a} :: QuestionDifference)

-- | Undocumented member.
questionDifference_questionId :: Lens.Lens' QuestionDifference (Prelude.Maybe Prelude.Text)
questionDifference_questionId = Lens.lens (\QuestionDifference' {questionId} -> questionId) (\s@QuestionDifference' {} a -> s {questionId = a} :: QuestionDifference)

-- | Undocumented member.
questionDifference_questionTitle :: Lens.Lens' QuestionDifference (Prelude.Maybe Prelude.Text)
questionDifference_questionTitle = Lens.lens (\QuestionDifference' {questionTitle} -> questionTitle) (\s@QuestionDifference' {} a -> s {questionTitle = a} :: QuestionDifference)

instance Data.FromJSON QuestionDifference where
  parseJSON =
    Data.withObject
      "QuestionDifference"
      ( \x ->
          QuestionDifference'
            Prelude.<$> (x Data..:? "DifferenceStatus")
            Prelude.<*> (x Data..:? "QuestionId")
            Prelude.<*> (x Data..:? "QuestionTitle")
      )

instance Prelude.Hashable QuestionDifference where
  hashWithSalt _salt QuestionDifference' {..} =
    _salt `Prelude.hashWithSalt` differenceStatus
      `Prelude.hashWithSalt` questionId
      `Prelude.hashWithSalt` questionTitle

instance Prelude.NFData QuestionDifference where
  rnf QuestionDifference' {..} =
    Prelude.rnf differenceStatus
      `Prelude.seq` Prelude.rnf questionId
      `Prelude.seq` Prelude.rnf questionTitle
